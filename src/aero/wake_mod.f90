module wake_mod
   !! Wake Panel Management
   !!
   !! Manages the free-wake model: trailing-edge wake shedding, wake
   !! convection (relaxation), and wake grid point tracking over time.
   !!
   !! Legacy mapping:
   !!   SUBROUTINE WAKE    ->  wake%shed_from_trailing_edge()
   !!   SUBROUTINE WAKREL  ->  wake%relax()
   !!   XW(I,J), YW(I,J)  ->  wake%positions(iter, grid_idx)
   !!   CW(I,J)            ->  wake%strengths(iter, panel_idx)
   !!   ICW(J,1:2)         ->  wake%panel_connectivity(j, 1:2)
   !!   NPW, NGW           ->  wake%n_panels, wake%n_grid_points
   !!   MARKW(I)           ->  wake%wake_node_map(i) (grid→wake index)
   !!   IPROPW(I)          ->  wake%panel_props(i)
   !!   NC(I)              ->  wake%source_node_ids(i) (body grid origin)
   !!
   !! Wake structure:
   !!   At each iteration, new wake row is shed from the trailing edge.
   !!   Wake panels are formed between consecutive time rows:
   !!     Row i  : positions at time step i
   !!     Row i+1: positions at time step i+1
   !!   A wake panel connects: (i,ICW(j,1)) - (i,ICW(j,2)) -
   !!                          (i+1,ICW(j,2)) - (i+1,ICW(j,1))
   use base_kinds_mod, only: wp, ip
   use vector3d_mod, only: vector3d_t, vec3, zero_vec3, norm, &
                           operator(+), operator(-), operator(*), operator(.dot.)
   use grid_mod, only: mesh_t
   use vortex_mod, only: vortex_ring_velocity
   use symmetry_mod, only: mirror_y, mirror_z, SYM_Y, GND_MIRROR
!$ use omp_lib
   implicit none
   private

   public :: wake_t

   ! ─── Constants ────────────────────────────────────────────────────────
   integer(ip), parameter :: MAX_WAKE_ITERS = 60  !! Max time steps stored

   ! ─── Type Definition ─────────────────────────────────────────────────
   type :: wake_t
      integer(ip) :: n_panels = 0  !! Number of wake panels (NPW)
      integer(ip) :: n_grid_pts = 0  !! Number of wake grid points (NGW)
      integer(ip) :: max_iters = MAX_WAKE_ITERS

      ! Wake grid positions: (max_iter+1, n_grid_pts)
      ! Row index = time step, column = wake grid point
      type(vector3d_t), allocatable :: positions(:, :)

      ! Wake panel strengths: (max_iter, n_panels)
      real(wp), allocatable :: strengths(:, :)

      ! Wake panel connectivity: (n_panels, 2) — indices into wake grid
      integer(ip), allocatable :: panel_conn(:, :)

      ! Wake panel properties: 0 = normal wake, 1 = propeller wake
      integer(ip), allocatable :: panel_props(:)

      ! Mapping from body grid to wake grid
      integer(ip), allocatable :: wake_node_map(:)    !! Size = n_body_nodes
      integer(ip), allocatable :: source_node_ids(:)  !! Wake grid → body node (NC)

      ! Original TE positions (before convection)
      type(vector3d_t), allocatable :: te_positions(:)  !! Size = n_grid_pts
   contains
      procedure :: init => wake_init
      procedure :: shed => wake_shed
      procedure :: convect_freestream => wake_convect_freestream
      procedure :: relax => wake_relax
      procedure :: get_panel_corners => wake_get_panel_corners
      procedure :: compute_induced_velocity => wake_induced_vel
      procedure :: destroy => wake_destroy
   end type wake_t

contains

   !> Initialize wake storage
   subroutine wake_init(this, n_body_nodes, max_iters)
      class(wake_t), intent(inout) :: this
      integer(ip), intent(in) :: n_body_nodes
      integer(ip), intent(in), optional :: max_iters

      if (present(max_iters)) this%max_iters = max_iters

      if (allocated(this%wake_node_map)) deallocate (this%wake_node_map)
      allocate (this%wake_node_map(n_body_nodes))
      this%wake_node_map = 0  ! 0 = not a wake node

      this%n_panels = 0
      this%n_grid_pts = 0
   end subroutine wake_init

   !> Shed wake from trailing edge
   !!
   !! Legacy equivalent: SUBROUTINE WAKE(ITER, VINIT, DT, NPAN, NPW, NGW, NGRID)
   !!
   !! Identifies trailing-edge edges from the mesh (edges where both nodes
   !! have non-zero boundary markers), creates wake grid points at those
   !! positions, and records wake panel connectivity and strengths.
   !!
   !! @param mesh       The body surface mesh
   !! @param gamma      Vortex strengths on body panels (size n_panels)
   !! @param iter       Current iteration number (1-based)
   subroutine wake_shed(this, mesh, gamma, iter)
      class(wake_t), intent(inout) :: this
      type(mesh_t), intent(in)     :: mesh
      real(wp), intent(in)         :: gamma(:)
      integer(ip), intent(in)      :: iter

      integer(ip) :: i, j, k, k1, k2, mk, ii, npw_new
      integer(ip) :: lpan, kk1, kk2, mkk, m
      integer(ip), allocatable :: icheck(:)
      integer(ip) :: it1
      integer(ip) :: temp_conn(mesh%n_panels*4, 2)    ! Oversized temp storage
      real(wp)    :: temp_strength(mesh%n_panels*4)
      integer(ip) :: temp_props(mesh%n_panels*4)

      it1 = iter + 1  ! New time row index

      ! ── On subsequent iterations, reuse existing topology ──────────────
      !  Only update strengths and TE positions
      if (this%n_panels > 0 .and. this%n_grid_pts > 0) then
         ! Update strengths for this iteration
         call compute_wake_strengths(this, mesh, gamma, iter)

         ! Update TE positions for the new time row
         do i = 1, mesh%n_nodes
            if (this%wake_node_map(i) > 0) then
               j = this%wake_node_map(i)
               this%te_positions(j) = mesh%nodes(i)
               this%positions(it1, j) = mesh%nodes(i)
            end if
         end do
         return
      end if

      ! ── First call: Discover wake topology ─────────────────────────────
      allocate (icheck(mesh%n_panels))
      icheck = 0

      ii = 0           ! Wake grid point counter
      npw_new = 0      ! Wake panel counter

      ! Initialize wake node map
      if (.not. allocated(this%wake_node_map)) then
         allocate (this%wake_node_map(mesh%n_nodes))
      end if
      this%wake_node_map = 0

      ! ── Phase 1: Identify trailing-edge edges and create wake grid points ──
      do j = 1, mesh%n_panels
         if (icheck(j) == 1) cycle

         mk = mesh%panels(j)%n_nodes

         ! Check each edge of the panel for trailing-edge edges
         do k = 1, mk
            k1 = k
            k2 = k + 1
            if (k == mk) then
               k1 = 1
               k2 = mk
            end if

            ! Both nodes must be trailing edge (non-zero mark)
            if (mesh%boundary_marks(mesh%panels(j)%node_ids(k1)) == 0) cycle
            if (mesh%boundary_marks(mesh%panels(j)%node_ids(k2)) == 0) cycle

            ! Both must not already be wake nodes
            if (this%wake_node_map(mesh%panels(j)%node_ids(k1)) /= 0 .and. &
                this%wake_node_map(mesh%panels(j)%node_ids(k2)) /= 0) cycle

            ! Found a trailing edge edge — find the neighbor panel
            lpan = 0
            kk1 = 0; kk2 = 0
            do m = j + 1, mesh%n_panels
               mkk = mesh%panels(m)%n_nodes
               kk1 = 0; kk2 = 0
               do i = 1, mkk
                  if (mesh%panels(m)%node_ids(i) == &
                      mesh%panels(j)%node_ids(k1)) kk1 = i
                  if (mesh%panels(m)%node_ids(i) == &
                      mesh%panels(j)%node_ids(k2)) kk2 = i
               end do
               if (kk1*kk2 /= 0) then
                  lpan = m
                  icheck(lpan) = 1
                  exit
               end if
            end do

            ! Create wake grid point for node k1 if not already mapped
            if (this%wake_node_map(mesh%panels(j)%node_ids(k1)) == 0) then
               ii = ii + 1
               this%wake_node_map(mesh%panels(j)%node_ids(k1)) = ii
            end if

            ! Create wake grid point for node k2 if not already mapped
            if (this%wake_node_map(mesh%panels(j)%node_ids(k2)) == 0) then
               ii = ii + 1
               this%wake_node_map(mesh%panels(j)%node_ids(k2)) = ii
            end if

            ! Record wake panel
            npw_new = npw_new + 1
            temp_conn(npw_new, 1) = this%wake_node_map(mesh%panels(j)%node_ids(k1))
            temp_conn(npw_new, 2) = this%wake_node_map(mesh%panels(j)%node_ids(k2))

            ! Wake panel strength = difference of adjacent panel circulations
            temp_strength(npw_new) = gamma(j)

            ! Adjust sign based on edge orientation (legacy convention)
            if (abs(k2 - k1) == (mk - 1)) then
               temp_strength(npw_new) = -temp_strength(npw_new)
            end if

            ! Subtract/add neighbor panel contribution
            if (lpan > 0) then
               mkk = mesh%panels(lpan)%n_nodes
               if (kk2 < kk1 .and. (kk1 - kk2) == 1) then
                  temp_strength(npw_new) = temp_strength(npw_new) - gamma(lpan)
               else if (kk2 < kk1 .and. (kk1 - kk2) == (mkk - 1)) then
                  temp_strength(npw_new) = temp_strength(npw_new) + gamma(lpan)
               else if (kk2 > kk1 .and. (kk2 - kk1) == 1) then
                  temp_strength(npw_new) = temp_strength(npw_new) + gamma(lpan)
               else if (kk2 > kk1 .and. (kk2 - kk1) == (mkk - 1)) then
                  temp_strength(npw_new) = temp_strength(npw_new) - gamma(lpan)
               end if
            end if

            ! Wake panel property (propeller wake flag)
            temp_props(npw_new) = 0
            if (mesh%panels(j)%property_id == 2) temp_props(npw_new) = 1

            exit  ! Found the TE edge for this panel, move on
         end do
      end do

      ! ── Store results ──────────────────────────────────────────────────
      this%n_grid_pts = ii
      this%n_panels = npw_new

      ! Allocate/resize wake arrays
      call reallocate_wake_arrays(this, iter)

      ! Copy connectivity and properties
      if (allocated(this%panel_conn)) deallocate (this%panel_conn)
      if (allocated(this%panel_props)) deallocate (this%panel_props)
      allocate (this%panel_conn(npw_new, 2))
      allocate (this%panel_props(npw_new))
      this%panel_conn(1:npw_new, :) = temp_conn(1:npw_new, :)
      this%panel_props(1:npw_new) = temp_props(1:npw_new)

      ! Store strengths for this iteration
      this%strengths(iter, 1:npw_new) = temp_strength(1:npw_new)

      ! Store TE positions in the new time row (it1)
      ! Also track source node mapping
      if (allocated(this%source_node_ids)) deallocate (this%source_node_ids)
      if (allocated(this%te_positions)) deallocate (this%te_positions)
      allocate (this%source_node_ids(ii))
      allocate (this%te_positions(ii))

      do i = 1, mesh%n_nodes
         if (this%wake_node_map(i) > 0) then
            j = this%wake_node_map(i)
            this%source_node_ids(j) = i
            this%te_positions(j) = mesh%nodes(i)
            this%positions(it1, j) = mesh%nodes(i)
         end if
      end do

      deallocate (icheck)
   end subroutine wake_shed

   !> Convect wake grid points by freestream velocity
   !!
   !! Legacy equivalent: The XW(ITER,KL) = X1 + VINIT*DT section in WAKE
   !! Moves the latest-shed row downstream and shifts all older rows.
   subroutine wake_convect_freestream(this, vinit, dt, iter)
      class(wake_t), intent(inout) :: this
      real(wp), intent(in)         :: vinit, dt
      integer(ip), intent(in)      :: iter
      integer(ip) :: i, j

      ! Set latest-shed row positions (iter row)
      do j = 1, this%n_grid_pts
         this%positions(iter, j)%x = this%te_positions(j)%x + vinit*dt
         this%positions(iter, j)%y = this%te_positions(j)%y
         this%positions(iter, j)%z = this%te_positions(j)%z
      end do

      ! Shift all older rows downstream by vinit*dt
      if (iter > 1) then
         do i = 1, iter - 1
            do j = 1, this%n_grid_pts
               this%positions(i, j)%x = this%positions(i, j)%x + vinit*dt
            end do
         end do
      end if
   end subroutine wake_convect_freestream

   !> Free-wake relaxation: convect wake by local flow velocity
   !!
   !! Legacy equivalent: SUBROUTINE WAKREL
   !! For each wake grid point at each time row, compute the local induced
   !! velocity (from body panels + other wake panels) and convect.
   !!
   !! This is OpenMP parallelized over the grid points at each time row.
   !!
   !! @param mesh  Body surface mesh
   !! @param gamma Body panel circulation strengths
   !! @param dt    Time step
   !! @param nsym  Symmetry flag
   !! @param ngrnd Ground effect flag
   subroutine wake_relax(this, mesh, gamma, iter, dt, nsym, ngrnd)
      class(wake_t), intent(inout) :: this
      type(mesh_t), intent(in)     :: mesh
      real(wp), intent(in)         :: gamma(:)
      integer(ip), intent(in)      :: iter
      real(wp), intent(in)         :: dt
      integer(ip), intent(in)      :: nsym, ngrnd

      type(vector3d_t), allocatable :: vel_field(:, :)
      type(vector3d_t) :: eval_pt, vel_body, vel_wake
      integer(ip) :: i, j

      if (iter < 1 .or. this%n_grid_pts < 1) return

      allocate (vel_field(iter, this%n_grid_pts))

      ! ── Compute velocity at each wake grid point ───────────────────────
      ! OpenMP parallelized: each (i,j) combination is independent
      !$OMP PARALLEL DO COLLAPSE(2) DEFAULT(SHARED) &
      !$OMP PRIVATE(i, j, eval_pt, vel_body, vel_wake) SCHEDULE(DYNAMIC)
      do i = 1, iter
         do j = 1, this%n_grid_pts
            eval_pt = this%positions(i, j)

            ! Velocity from body panels
            call compute_body_induced_vel(mesh, gamma, eval_pt, &
                                          nsym, ngrnd, vel_body)

            ! Velocity from wake panels
            call compute_wake_induced_vel_at(this, iter, eval_pt, &
                                             nsym, ngrnd, vel_wake)

            vel_field(i, j) = vec3(vel_body%x + vel_wake%x, &
                                   vel_body%y + vel_wake%y, &
                                   vel_body%z + vel_wake%z)
         end do
      end do
      !$OMP END PARALLEL DO

      ! ── Apply convection ───────────────────────────────────────────────
      do i = 1, iter
         do j = 1, this%n_grid_pts
            this%positions(i, j)%x = this%positions(i, j)%x + vel_field(i, j)%x*dt
            this%positions(i, j)%y = this%positions(i, j)%y + vel_field(i, j)%y*dt
            this%positions(i, j)%z = this%positions(i, j)%z + vel_field(i, j)%z*dt
         end do
      end do

      deallocate (vel_field)
   end subroutine wake_relax

   !> Get the four corner points of a wake panel at a given time row
   !!
   !! Wake panel j at time row i has corners:
   !!   p1 = positions(i,   conn(j,1))
   !!   p2 = positions(i,   conn(j,2))
   !!   p3 = positions(i+1, conn(j,2))
   !!   p4 = positions(i+1, conn(j,1))
   pure subroutine wake_get_panel_corners(this, i_row, j_pan, p1, p2, p3, p4)
      class(wake_t), intent(in) :: this
      integer(ip), intent(in) :: i_row, j_pan
      type(vector3d_t), intent(out) :: p1, p2, p3, p4

      p1 = this%positions(i_row, this%panel_conn(j_pan, 1))
      p2 = this%positions(i_row, this%panel_conn(j_pan, 2))
      p3 = this%positions(i_row + 1, this%panel_conn(j_pan, 2))
      p4 = this%positions(i_row + 1, this%panel_conn(j_pan, 1))
   end subroutine wake_get_panel_corners

   !> Compute velocity induced by all wake panels at a given point
   !!
   !! Legacy equivalent: SUBROUTINE VELWAK
   subroutine wake_induced_vel(this, iter, eval_point, nsym, ngrnd, vel)
      class(wake_t), intent(in)    :: this
      integer(ip), intent(in)      :: iter
      type(vector3d_t), intent(in) :: eval_point
      integer(ip), intent(in)      :: nsym, ngrnd
      type(vector3d_t), intent(out) :: vel

      call compute_wake_induced_vel_at(this, iter, eval_point, nsym, ngrnd, vel)
   end subroutine wake_induced_vel

   !> Clean up wake memory
   subroutine wake_destroy(this)
      class(wake_t), intent(inout) :: this

      if (allocated(this%positions)) deallocate (this%positions)
      if (allocated(this%strengths)) deallocate (this%strengths)
      if (allocated(this%panel_conn)) deallocate (this%panel_conn)
      if (allocated(this%panel_props)) deallocate (this%panel_props)
      if (allocated(this%wake_node_map)) deallocate (this%wake_node_map)
      if (allocated(this%source_node_ids)) deallocate (this%source_node_ids)
      if (allocated(this%te_positions)) deallocate (this%te_positions)

      this%n_panels = 0
      this%n_grid_pts = 0
   end subroutine wake_destroy

   ! ═══════════════════════════════════════════════════════════════════════
   !                   PRIVATE HELPER ROUTINES
   ! ═══════════════════════════════════════════════════════════════════════

   !> Compute wake panel strengths for current iteration (reuses topology)
   !!
   !! For each wake panel, the strength is the net circulation shed from
   !! the trailing-edge edge: gamma_j - gamma_neighbor, with sign conventions
   !! matching the original topology discovery.
   subroutine compute_wake_strengths(this, mesh, gamma, iter)
      type(wake_t), intent(inout) :: this
      type(mesh_t), intent(in)    :: mesh
      real(wp), intent(in)        :: gamma(:)
      integer(ip), intent(in)     :: iter

      integer(ip) :: i, j, k, k1, k2, mk, npw_idx
      integer(ip) :: lpan, kk1, kk2, mkk, m
      integer(ip), allocatable :: icheck(:)

      allocate (icheck(mesh%n_panels))
      icheck = 0
      npw_idx = 0

      ! Retrace the same edge-finding logic to compute new strengths
      do j = 1, mesh%n_panels
         if (icheck(j) == 1) cycle
         mk = mesh%panels(j)%n_nodes

         do k = 1, mk
            k1 = k
            k2 = k + 1
            if (k == mk) then
               k1 = 1
               k2 = mk
            end if

            if (mesh%boundary_marks(mesh%panels(j)%node_ids(k1)) == 0) cycle
            if (mesh%boundary_marks(mesh%panels(j)%node_ids(k2)) == 0) cycle

            ! Check that both nodes are valid wake nodes
            if (this%wake_node_map(mesh%panels(j)%node_ids(k1)) == 0) cycle
            if (this%wake_node_map(mesh%panels(j)%node_ids(k2)) == 0) cycle

            ! Find neighbor panel
            lpan = 0; kk1 = 0; kk2 = 0
            do m = j + 1, mesh%n_panels
               mkk = mesh%panels(m)%n_nodes
               kk1 = 0; kk2 = 0
               do i = 1, mkk
                  if (mesh%panels(m)%node_ids(i) == &
                      mesh%panels(j)%node_ids(k1)) kk1 = i
                  if (mesh%panels(m)%node_ids(i) == &
                      mesh%panels(j)%node_ids(k2)) kk2 = i
               end do
               if (kk1*kk2 /= 0) then
                  lpan = m
                  icheck(lpan) = 1
                  exit
               end if
            end do

            npw_idx = npw_idx + 1
            if (npw_idx > this%n_panels) exit

            ! Same strength computation as first-call
            this%strengths(iter, npw_idx) = gamma(j)

            if (abs(k2 - k1) == (mk - 1)) then
               this%strengths(iter, npw_idx) = -this%strengths(iter, npw_idx)
            end if

            if (lpan > 0) then
               mkk = mesh%panels(lpan)%n_nodes
               if (kk2 < kk1 .and. (kk1 - kk2) == 1) then
                  this%strengths(iter, npw_idx) = this%strengths(iter, npw_idx) - gamma(lpan)
               else if (kk2 < kk1 .and. (kk1 - kk2) == (mkk - 1)) then
                  this%strengths(iter, npw_idx) = this%strengths(iter, npw_idx) + gamma(lpan)
               else if (kk2 > kk1 .and. (kk2 - kk1) == 1) then
                  this%strengths(iter, npw_idx) = this%strengths(iter, npw_idx) + gamma(lpan)
               else if (kk2 > kk1 .and. (kk2 - kk1) == (mkk - 1)) then
                  this%strengths(iter, npw_idx) = this%strengths(iter, npw_idx) - gamma(lpan)
               end if
            end if

            exit  ! Found the TE edge for this panel, move on
         end do
      end do

      deallocate (icheck)
   end subroutine compute_wake_strengths

   !> Ensure wake arrays are allocated for the current iteration count
   subroutine reallocate_wake_arrays(this, iter)
      type(wake_t), intent(inout) :: this
      integer(ip), intent(in)     :: iter
      integer(ip) :: n_rows

      n_rows = iter + 1  ! +1 because TE row is at iter+1

      if (.not. allocated(this%positions)) then
         allocate (this%positions(this%max_iters + 1, this%n_grid_pts))
      else if (size(this%positions, 2) < this%n_grid_pts) then
         deallocate (this%positions)
         allocate (this%positions(this%max_iters + 1, this%n_grid_pts))
      end if

      if (.not. allocated(this%strengths)) then
         allocate (this%strengths(this%max_iters, max(this%n_panels, 1)))
         this%strengths = 0.0_wp
      else if (size(this%strengths, 2) < this%n_panels) then
         deallocate (this%strengths)
         allocate (this%strengths(this%max_iters, this%n_panels))
         this%strengths = 0.0_wp
      end if
   end subroutine reallocate_wake_arrays

   !> Compute velocity induced by body panels at a point
   !! (Internal helper, used by wake relaxation)
   !!
   !! Legacy equivalent: SUBROUTINE VELPAN
   subroutine compute_body_induced_vel(mesh, gamma, eval_point, &
                                       nsym, ngrnd, vel)
      type(mesh_t), intent(in)     :: mesh
      real(wp), intent(in)         :: gamma(:)
      type(vector3d_t), intent(in) :: eval_point
      integer(ip), intent(in)      :: nsym, ngrnd
      type(vector3d_t), intent(out) :: vel

      type(vector3d_t) :: p1, p2, p3, p4, v_ind
      integer(ip) :: ik, ng, na
      real(wp) :: sign_factor

      vel = zero_vec3()

      do ng = 0, ngrnd
         do ik = 1, mesh%n_panels
            do na = 0, nsym
               ! Get panel corners
               p1 = mesh%nodes(mesh%panels(ik)%node_ids(1))
               p2 = mesh%nodes(mesh%panels(ik)%node_ids(2))
               p3 = mesh%nodes(mesh%panels(ik)%node_ids(3))
               p4 = mesh%nodes(mesh%panels(ik)%node_ids(4))

               ! Apply symmetry mirror (Y)
               if (na == 1) then
                  p1 = mirror_y_local(p1)
                  p2 = mirror_y_local(p2)
                  p3 = mirror_y_local(p3)
                  p4 = mirror_y_local(p4)
               end if

               ! Apply ground mirror (Z)
               if (ng == 1) then
                  p1 = mirror_z_local(p1)
                  p2 = mirror_z_local(p2)
                  p3 = mirror_z_local(p3)
                  p4 = mirror_z_local(p4)
               end if

               ! Compute induced velocity
               v_ind = vortex_ring_velocity(eval_point, p1, p2, p3, p4)

               ! Sign factor: (-1)^na * (-1)^ng
               sign_factor = real((-1)**na, wp)*real((-1)**ng, wp)

               vel%x = vel%x + v_ind%x*gamma(ik)*sign_factor
               vel%y = vel%y + v_ind%y*gamma(ik)*sign_factor
               vel%z = vel%z + v_ind%z*gamma(ik)*sign_factor
            end do
         end do
      end do
   end subroutine compute_body_induced_vel

   !> Compute velocity induced by all wake panels at a point
   !! (Internal helper, called from relax and induced_vel)
   !!
   !! Legacy equivalent: SUBROUTINE VELWAK
   subroutine compute_wake_induced_vel_at(wake, iter, eval_point, &
                                          nsym, ngrnd, vel)
      type(wake_t), intent(in)    :: wake
      integer(ip), intent(in)      :: iter
      type(vector3d_t), intent(in) :: eval_point
      integer(ip), intent(in)      :: nsym, ngrnd
      type(vector3d_t), intent(out) :: vel

      type(vector3d_t) :: p1, p2, p3, p4, v_ind
      integer(ip) :: ik, jk, ng, na
      real(wp) :: sign_factor

      vel = zero_vec3()
      if (wake%n_panels < 1 .or. iter < 1) return

      do ng = 0, ngrnd
         do ik = 1, iter
            do jk = 1, wake%n_panels
               do na = 0, nsym
                  ! Get wake panel corners
                  call wake%get_panel_corners(ik, jk, p1, p2, p3, p4)

                  ! Apply symmetry mirror (Y)
                  if (na == 1) then
                     p1 = mirror_y_local(p1)
                     p2 = mirror_y_local(p2)
                     p3 = mirror_y_local(p3)
                     p4 = mirror_y_local(p4)
                  end if

                  ! Apply ground mirror (Z)
                  if (ng == 1) then
                     p1 = mirror_z_local(p1)
                     p2 = mirror_z_local(p2)
                     p3 = mirror_z_local(p3)
                     p4 = mirror_z_local(p4)
                  end if

                  v_ind = vortex_ring_velocity(eval_point, p1, p2, p3, p4)

                  sign_factor = real((-1)**na, wp)*real((-1)**ng, wp)

                  vel%x = vel%x + v_ind%x*wake%strengths(ik, jk)*sign_factor
                  vel%y = vel%y + v_ind%y*wake%strengths(ik, jk)*sign_factor
                  vel%z = vel%z + v_ind%z*wake%strengths(ik, jk)*sign_factor
               end do
            end do
         end do
      end do
   end subroutine compute_wake_induced_vel_at

   ! ── Local mirror functions (to avoid circular dependencies) ──────────
   pure function mirror_y_local(v) result(vm)
      type(vector3d_t), intent(in) :: v
      type(vector3d_t) :: vm
      vm%x = v%x; vm%y = -v%y; vm%z = v%z
   end function mirror_y_local

   pure function mirror_z_local(v) result(vm)
      type(vector3d_t), intent(in) :: v
      type(vector3d_t) :: vm
      vm%x = v%x; vm%y = v%y; vm%z = -v%z
   end function mirror_z_local

end module wake_mod
