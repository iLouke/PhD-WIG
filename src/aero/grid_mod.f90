module grid_mod
   !! Mesh / Grid Manager
   !!
   !! Manages the complete surface discretization: grid points (nodes),
   !! panel connectivity, boundary markers, and coordinate transformations.
   !!
   !! Legacy mapping:
   !!   SUBROUTINE GEOM     ->  mesh%load_geometry() / mesh%set_geometry()
   !!   SUBROUTINE ANALGEO  ->  mesh%compute_panel_geometry()
   !!   X(I),Y(I),Z(I)     ->  mesh%nodes(i)
   !!   IC(I,1:4)           ->  mesh%panels(i)%node_ids(1:4)
   !!   MARK(I)             ->  mesh%boundary_marks(i)
   !!   IPROP(I)            ->  mesh%panels(i)%property_id
   !!   NPAN, NGRID         ->  mesh%n_panels, mesh%n_nodes
   !!
   !! The Euler-angle rotation from GEOM is replaced by quaternion rotation.
   use base_kinds_mod, only: wp, ip
   use constants_mod, only: PI
   use vector3d_mod, only: vector3d_t, vec3
   use quaternion_mod, only: quaternion_t, from_euler_aero
   use panel_mod, only: panel_t
!$ use omp_lib
   implicit none
   private

   public :: mesh_t

   ! ─── Type Definition ─────────────────────────────────────────────────
   type :: mesh_t
      ! Grid data
      integer(ip) :: n_nodes = 0       !! Number of grid points
      integer(ip) :: n_panels = 0       !! Number of panels
      type(vector3d_t), allocatable :: nodes(:)   !! Grid point coordinates
      type(panel_t), allocatable   :: panels(:)   !! Panel array

      ! Boundary / property data
      integer(ip), allocatable :: boundary_marks(:) !! Node boundary markers (MARK)

      ! Flow parameters (from geometry input)
      real(wp) :: alpha = 0.0_wp    !! Angle of attack [rad]
      real(wp) :: beta = 0.0_wp    !! Sideslip angle [rad]
      real(wp) :: gamma_r = 0.0_wp    !! Roll/bank angle [rad]
      real(wp) :: vinit = 0.0_wp    !! Freestream velocity [m/s]
      real(wp) :: eps = 1.0e-3_wp !! Convergence tolerance
      real(wp) :: dt = 0.03_wp   !! Time step [s]
      real(wp) :: ref_length = 1.0_wp !! Reference length [m]
      real(wp) :: hfl = 0.0_wp    !! Height for ground effect [m]

      ! Flags
      integer(ip) :: nsym = 0         !! 1 = Y-symmetry enabled
      integer(ip) :: ngrnd = 0         !! 1 = Ground effect enabled

      ! Rotation quaternion (replaces Euler trig from legacy GEOM)
      type(quaternion_t) :: orientation
   contains
      procedure :: init => mesh_init
      procedure :: set_geometry => mesh_set_geometry
      procedure :: apply_rotation => mesh_apply_rotation
      procedure :: apply_ground_offset => mesh_apply_ground_offset
      procedure :: compute_panel_geometry => mesh_compute_panel_geometry
      procedure :: get_node => mesh_get_node
      procedure :: get_panel => mesh_get_panel
      procedure :: destroy => mesh_destroy
   end type mesh_t

contains

   !> Allocate mesh storage for known sizes
   subroutine mesh_init(this, n_nodes, n_panels)
      class(mesh_t), intent(inout) :: this
      integer(ip), intent(in) :: n_nodes, n_panels

      this%n_nodes = n_nodes
      this%n_panels = n_panels

      if (allocated(this%nodes)) deallocate (this%nodes)
      if (allocated(this%panels)) deallocate (this%panels)
      if (allocated(this%boundary_marks)) deallocate (this%boundary_marks)

      allocate (this%nodes(n_nodes))
      allocate (this%panels(n_panels))
      allocate (this%boundary_marks(n_nodes))

      this%boundary_marks = 0
   end subroutine mesh_init

   !> Set geometry data directly (for programmatic mesh generation)
   !!
   !! @param nodes      Array of 3D grid point coordinates
   !! @param panel_ids  Panel connectivity (n_panels x 4), 4th col = 3rd for tri
   !! @param marks      Boundary markers per node (0 = interior, /=0 = trailing edge)
   !! @param props      Panel property IDs
   subroutine mesh_set_geometry(this, nodes, panel_ids, marks, props)
      class(mesh_t), intent(inout) :: this
      type(vector3d_t), intent(in) :: nodes(:)
      integer(ip), intent(in)      :: panel_ids(:, :)  ! (n_panels, 4)
      integer(ip), intent(in)      :: marks(:)
      integer(ip), intent(in)      :: props(:)
      integer(ip) :: i

      this%n_nodes = size(nodes)
      this%n_panels = size(panel_ids, 1)
      call this%init(this%n_nodes, this%n_panels)

      this%nodes = nodes
      this%boundary_marks = marks

      do i = 1, this%n_panels
         this%panels(i)%node_ids = panel_ids(i, :)
         this%panels(i)%property_id = props(i)
         if (panel_ids(i, 3) == panel_ids(i, 4)) then
            this%panels(i)%n_nodes = 3
         else
            this%panels(i)%n_nodes = 4
         end if
      end do
   end subroutine mesh_set_geometry

   !> Apply quaternion rotation to all grid nodes
   !!
   !! Legacy equivalent: The coordinate transformation loop in GEOM:
   !!   XX = X*cosA*cosB + Y*sinB + Z*sinA*cosB
   !!   (etc.)
   !! Now replaced by: rotated_node = quaternion%rotate(node)
   !!
   !! The quaternion is reconstructed from (alpha, beta, gamma) to maintain
   !! backward compatibility with legacy input files, but internally uses
   !! quaternion math for singularity-free rotation.
   subroutine mesh_apply_rotation(this, alpha, beta, gamma_r)
      class(mesh_t), intent(inout) :: this
      real(wp), intent(in) :: alpha, beta, gamma_r
      integer(ip) :: i

      this%alpha = alpha
      this%beta = beta
      this%gamma_r = gamma_r

      ! Build orientation quaternion from aerospace Euler angles
      this%orientation = from_euler_aero(alpha, beta, gamma_r)

      ! Rotate all nodes (OpenMP parallelized for large meshes)
      !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i) SCHEDULE(STATIC)
      do i = 1, this%n_nodes
         this%nodes(i) = this%orientation%rotate(this%nodes(i))
      end do
      !$OMP END PARALLEL DO

   end subroutine mesh_apply_rotation

   !> Offset all nodes vertically for ground effect
   !!
   !! Legacy: IF(NGRND.EQ.1) Z(I)=Z(I)+HFL
   subroutine mesh_apply_ground_offset(this, hfl)
      class(mesh_t), intent(inout) :: this
      real(wp), intent(in) :: hfl
      integer(ip) :: i

      this%hfl = hfl
      do i = 1, this%n_nodes
         this%nodes(i)%z = this%nodes(i)%z + hfl
      end do
   end subroutine mesh_apply_ground_offset

   !> Compute geometric properties for all panels
   !!
   !! Legacy equivalent: SUBROUTINE ANALGEO(NPAN)
   !! Computes centroids, normals, areas, and local coordinate systems.
   !! OpenMP parallelized — each panel's computation is independent.
   subroutine mesh_compute_panel_geometry(this)
      class(mesh_t), intent(inout) :: this
      integer(ip) :: i

      !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i) SCHEDULE(STATIC)
      do i = 1, this%n_panels
         call this%panels(i)%compute_geometry(this%nodes)
      end do
      !$OMP END PARALLEL DO

   end subroutine mesh_compute_panel_geometry

   !> Get a node by index (bounds-checked in debug)
   pure function mesh_get_node(this, idx) result(node)
      class(mesh_t), intent(in) :: this
      integer(ip), intent(in) :: idx
      type(vector3d_t) :: node
      node = this%nodes(idx)
   end function mesh_get_node

   !> Get a panel by index
   pure function mesh_get_panel(this, idx) result(pan)
      class(mesh_t), intent(in) :: this
      integer(ip), intent(in) :: idx
      type(panel_t) :: pan
      pan = this%panels(idx)
   end function mesh_get_panel

   !> Clean up mesh memory
   subroutine mesh_destroy(this)
      class(mesh_t), intent(inout) :: this

      if (allocated(this%nodes)) deallocate (this%nodes)
      if (allocated(this%panels)) deallocate (this%panels)
      if (allocated(this%boundary_marks)) deallocate (this%boundary_marks)

      this%n_nodes = 0
      this%n_panels = 0
   end subroutine mesh_destroy

end module grid_mod
