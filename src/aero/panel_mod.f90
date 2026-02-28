module panel_mod
   !! Panel Geometry Type
   !!
   !! Encapsulates a single aerodynamic panel (triangular or quadrilateral)
   !! with all its geometric properties: centroid, normal, tangent vectors,
   !! area, and local coordinate system.
   !!
   !! Legacy mapping:
   !!   SUBROUTINE ANALGEO  ->  panel%compute_geometry()
   !!   IC(I,1:4)           ->  panel%node_ids(1:4)
   !!   GX(I),GY(I),GZ(I)  ->  panel%centroid
   !!   ANX,ANY,ANZ         ->  panel%normal
   !!   ALX,ALY,ALZ         ->  panel%longitudinal
   !!   ATX,ATY,ATZ         ->  panel%tangent
   !!   DS(I)               ->  panel%area
   !!   MARK(I)             ->  panel%boundary_tag
   !!   IPROP(I)            ->  panel%property_id
   use base_kinds_mod, only: wp, ip
   use vector3d_mod, only: vector3d_t, vec3, zero_vec3, norm, normalize, &
                           operator(-), operator(+), operator(*), operator(/), &
                           operator(.cross.)
   implicit none
   private

   public :: panel_t

   ! ─── Constants for panel types ────────────────────────────────────────
   integer(ip), parameter, public :: PANEL_QUAD = 4   !! Quadrilateral
   integer(ip), parameter, public :: PANEL_TRI = 3   !! Triangular

   ! ─── Type Definition ─────────────────────────────────────────────────
   type :: panel_t
      ! Connectivity
      integer(ip) :: node_ids(4) = 0     !! Grid point indices (4th = 3rd for triangle)
      integer(ip) :: n_nodes = 4     !! 3 or 4

      ! Computed geometric properties
      type(vector3d_t) :: centroid        !! Panel center (collocation point)
      type(vector3d_t) :: normal          !! Unit outward normal
      type(vector3d_t) :: tangent         !! Unit tangent (transverse direction)
      type(vector3d_t) :: longitudinal    !! Unit longitudinal (chordwise direction)
      real(wp)         :: area = 0.0_wp   !! Panel area [m²]

      ! Tags
      integer(ip) :: property_id = 0    !! Panel property (e.g., 2 = propeller, 14 = special)
      integer(ip) :: boundary_tag = 0    !! Boundary marker from grid
   contains
      procedure :: compute_geometry => panel_compute_geometry
      procedure :: is_triangle => panel_is_triangle
      procedure :: is_trailing_edge_node => panel_is_te_node
   end type panel_t

contains

   !> Compute all geometric properties of a panel from its node coordinates
   !!
   !! This is the modern equivalent of the legacy ANALGEO subroutine.
   !! Computes: centroid, unit normal, area, longitudinal & tangent vectors.
   !!
   !! Algorithm (matching legacy exactly):
   !!   1. Centroid = average of node coordinates
   !!   2. Normal = normalized cross product of diagonals (1→3) x (4→2)
   !!      (with sign inverted to point outward)
   !!   3. Area = sum of two triangle areas (split by diagonal 1→3)
   !!   4. Longitudinal = unit vector from centroid toward midpoint of edge 1-2
   !!   5. Tangent = longitudinal x normal (right-hand system)
   !!
   !! @param nodes Array of ALL grid point coordinates (indexed by node_ids)
   subroutine panel_compute_geometry(this, nodes)
      class(panel_t), intent(inout) :: this
      type(vector3d_t), intent(in)  :: nodes(:)

      type(vector3d_t) :: p1, p2, p3, p4
      type(vector3d_t) :: d13, d42, d12, d14
      type(vector3d_t) :: cross_vec, midpoint, to_mid
      real(wp) :: anorm_val, ds1, ds2

      ! Retrieve node positions
      p1 = nodes(this%node_ids(1))
      p2 = nodes(this%node_ids(2))
      p3 = nodes(this%node_ids(3))
      p4 = nodes(this%node_ids(4))

      ! ── Centroid ───────────────────────────────────────────────────────
      if (this%is_triangle()) then
         this%centroid = (p1 + p2 + p3)*(1.0_wp/3.0_wp)
      else
         this%centroid = (p1 + p2 + p3 + p4)*0.25_wp
      end if

      ! ── Normal Vector ──────────────────────────────────────────────────
      ! Diagonal vectors: 1→3 and 4→2
      d13 = p1 - p3
      d42 = p4 - p2

      ! Cross product of diagonals (legacy: XX = Y13*Z42 - Z13*Y42, etc.)
      cross_vec = d13.cross.d42
      anorm_val = norm(cross_vec)

      ! Normal is NEGATED (legacy convention: ANX(I) = -XX/ANORM)
      if (anorm_val > 0.0_wp) then
         this%normal = vec3(-cross_vec%x/anorm_val, &
                            -cross_vec%y/anorm_val, &
                            -cross_vec%z/anorm_val)
      else
         this%normal = vec3(0.0_wp, 0.0_wp, 1.0_wp)  ! Fallback
      end if

      ! ── Area ───────────────────────────────────────────────────────────
      ! Two triangles split by diagonal 1→3
      ! Triangle 1: nodes 1-2-3 (cross product of d13 x d12, half magnitude)
      d12 = p1 - p2  ! Note: d12 in legacy is vertices 1→2 subtracted
      ! Wait - legacy does X12=X(IC(I,2))-X(IC(I,1)), so d12 = p2 - p1
      d12 = p2 - p1
      d14 = p4 - p1

      ! Legacy: XX=Y13*Z12-Z13*Y12 (cross of d13 x d12 with d13 from p1-p3)
      ! d13 = p1 - p3 (already computed above)
      cross_vec = d13.cross.d12
      ds1 = 0.5_wp*norm(cross_vec)

      ! Triangle 2: nodes 1-3-4 (cross product of d13 x d14)
      cross_vec = d13.cross.d14
      ds2 = 0.5_wp*norm(cross_vec)

      this%area = ds1 + ds2

      ! ── Longitudinal Vector ────────────────────────────────────────────
      ! Direction from centroid toward midpoint of edge 1-2
      midpoint = (p1 + p2)*0.5_wp
      to_mid = midpoint - this%centroid
      anorm_val = norm(to_mid)

      if (anorm_val > 0.0_wp) then
         this%longitudinal = to_mid/anorm_val
      else
         this%longitudinal = vec3(1.0_wp, 0.0_wp, 0.0_wp)  ! Fallback
      end if

      ! ── Tangent Vector ─────────────────────────────────────────────────
      ! t = l x n (right-hand coordinate system)
      cross_vec = this%longitudinal.cross.this%normal
      anorm_val = norm(cross_vec)

      if (anorm_val > 0.0_wp) then
         this%tangent = cross_vec/anorm_val
      else
         this%tangent = vec3(0.0_wp, 1.0_wp, 0.0_wp)  ! Fallback
      end if

   end subroutine panel_compute_geometry

   !> Check if this panel is a triangle (node 3 == node 4)
   pure function panel_is_triangle(this) result(is_tri)
      class(panel_t), intent(in) :: this
      logical :: is_tri
      is_tri = (this%node_ids(3) == this%node_ids(4))
   end function panel_is_triangle

   !> Check if a given node index is at the trailing edge
   !! A trailing edge node has a non-zero boundary marker in the grid
   pure function panel_is_te_node(this, node_idx, boundary_marks) result(is_te)
      class(panel_t), intent(in) :: this
      integer(ip), intent(in) :: node_idx
      integer(ip), intent(in) :: boundary_marks(:)
      logical :: is_te
      is_te = (boundary_marks(node_idx) /= 0)
   end function panel_is_te_node

end module panel_mod
