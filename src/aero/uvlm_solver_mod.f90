module uvlm_solver_mod
   !! Unsteady Vortex Lattice Method (UVLM) Solver
   !!
   !! Top-level orchestrator that ties together all aerodynamic modules
   !! to implement the complete UVLM solution algorithm.
   !!
   !! Legacy mapping:
   !!   PROGRAM ROS -> uvlm_solver_t
   !!   The main program loop in the legacy code is now encapsulated as:
   !!     solver%init()           -> GEOM + ANALGEO
   !!     solver%step()           -> One iteration of the main loop
   !!     solver%run()            -> Full convergence loop
   !!     solver%get_forces()     -> AIRLOAD1 results
   !!
   !! Algorithm (matching legacy flow):
   !!   1. Initialize:
   !!      - Load/set geometry (grid + panels)
   !!      - Apply quaternion rotation (replaces Euler rotation in GEOM)
   !!      - Apply ground effect offset
   !!      - Compute panel geometric properties (ANALGEO)
   !!
   !!   2. Iteration loop:
   !!      iter=0: Build AIC matrix, factorize, solve for initial gamma
   !!      iter>0:
   !!        a. Shed wake from trailing edge (WAKE)
   !!        b. Convect wake by freestream (WAKE)
   !!        c. Handle wake-body penetration (WAKCOR, WAKINT)
   !!        d. Relax wake (WAKREL) — free-wake convection
   !!        e. Build new RHS with wake contribution
   !!        f. Solve for updated gamma using stored LU factors
   !!        g. Compute aerodynamic forces (AIRLOAD1)
   !!        h. Check convergence: |lift_new - lift_old| / |lift_new| < eps
   !!
   !!   3. Post-processing:
   !!      - Pressure coefficients (CPAIP)
   !!      - Force/moment output
   use base_kinds_mod, only: wp, ip
   use constants_mod, only: PI, RHO_AIR
   use vector3d_mod, only: vector3d_t, vec3
   use quaternion_mod, only: quaternion_t, from_euler_aero
   use panel_mod, only: panel_t
   use grid_mod, only: mesh_t
   use vortex_mod, only: vortex_ring_velocity
   use aero_solver_mod, only: aero_linsys_t
   use symmetry_mod, only: SYM_NONE, SYM_Y, GND_NONE, GND_MIRROR
   use wake_mod, only: wake_t
   use influence_mod, only: solve_system
   use penetration_mod, only: correct_wake_penetrations, handle_wake_intersections
   use aerodynamic_loads_mod, only: compute_forces, compute_pressure_coefficients, &
                                    aero_forces_t
   use logger_mod, only: logger_t, global_logger, LOG_INFO, LOG_DEBUG, LOG_WARN
   use timer_mod, only: timer_t
   implicit none
   private

   public :: uvlm_solver_t
   public :: uvlm_config_t

   ! ─── Configuration Type ───────────────────────────────────────────────
   type :: uvlm_config_t
      real(wp) :: vinit = 10.0_wp     !! Freestream velocity [m/s]
      real(wp) :: alpha_deg = 5.0_wp      !! Angle of attack [degrees]
      real(wp) :: beta_deg = 0.0_wp      !! Sideslip angle [degrees]
      real(wp) :: gamma_deg = 0.0_wp      !! Roll angle [degrees]
      real(wp) :: rho = RHO_AIR     !! Air density [kg/m³]
      real(wp) :: eps = 1.0e-3_wp   !! Convergence tolerance
      real(wp) :: dt = 0.03_wp     !! Time step [s]
      real(wp) :: hfl = 0.0_wp      !! Ground effect height [m]
      real(wp) :: vaip = 0.0_wp      !! Additional velocity component
      integer(ip) :: max_iter = 8         !! Maximum iterations
      integer(ip) :: nsym = 0         !! Y-symmetry flag (0 or 1)
      integer(ip) :: ngrnd = 0         !! Ground effect flag (0 or 1)
   end type uvlm_config_t

   ! ─── Solver Type ──────────────────────────────────────────────────────
   type :: uvlm_solver_t
      private
      ! Configuration
      type(uvlm_config_t) :: config

      ! Core components
      type(mesh_t)          :: mesh       !! Body surface mesh
      type(wake_t)          :: wake       !! Free wake model
      type(aero_linsys_t)   :: linsys     !! Linear system solver (LU factors)

      ! Solution state
      real(wp), allocatable :: gamma(:)   !! Vortex strengths per panel
      real(wp), allocatable :: cp(:)      !! Pressure coefficients per panel
      integer(ip)           :: iter = 0   !! Current iteration count

      ! Results
      type(aero_forces_t) :: forces       !! Latest aerodynamic forces
      real(wp)            :: lift_prev = 0.0_wp  !! Previous lift (for convergence)
      logical             :: converged = .false.

      ! Diagnostics
      type(timer_t) :: solve_timer
      type(timer_t) :: wake_timer
   contains
      ! Setup
      procedure :: init => solver_init
      procedure :: set_config => solver_set_config

      ! Execution
      procedure :: step => solver_step
      procedure :: run => solver_run

      ! Queries
      procedure :: get_forces => solver_get_forces
      procedure :: get_gamma => solver_get_gamma
      procedure :: get_iter => solver_get_iter
      procedure :: is_converged => solver_is_converged

      ! Data export (for plotting)
      procedure :: export_mesh_panels => solver_export_mesh_panels
      procedure :: export_wake_panels => solver_export_wake_panels

      ! Cleanup
      procedure :: destroy => solver_destroy
   end type uvlm_solver_t

contains

   ! ═══════════════════════════════════════════════════════════════════════
   !                         INITIALIZATION
   ! ═══════════════════════════════════════════════════════════════════════

   !> Initialize the UVLM solver with a mesh and configuration
   !!
   !! Legacy equivalent: CALL GEOM(...) + CALL ANALGEO(NPAN)
   !!
   !! Steps:
   !!   1. Store configuration
   !!   2. Apply quaternion rotation to the mesh (replaces Euler trig)
   !!   3. Apply ground effect offset (if enabled)
   !!   4. Compute panel geometric properties (centroids, normals, areas)
   !!   5. Initialize wake storage
   !!   6. Allocate solution arrays
   subroutine solver_init(this, mesh, config)
      class(uvlm_solver_t), intent(inout) :: this
      type(mesh_t), intent(in)            :: mesh
      type(uvlm_config_t), intent(in)     :: config

      real(wp) :: alpha_rad, beta_rad, gamma_rad

      this%config = config
      this%mesh = mesh
      this%iter = 0
      this%converged = .false.

      ! Convert angles to radians
      alpha_rad = config%alpha_deg*PI/180.0_wp
      beta_rad = config%beta_deg*PI/180.0_wp
      gamma_rad = config%gamma_deg*PI/180.0_wp

      ! Apply quaternion rotation (replacing legacy Euler angle matrix)
      call this%mesh%apply_rotation(alpha_rad, beta_rad, gamma_rad)

      ! Apply ground effect vertical offset
      if (config%ngrnd == GND_MIRROR .and. config%hfl > 0.0_wp) then
         call this%mesh%apply_ground_offset(config%hfl)
      end if

      ! Compute panel geometric properties (ANALGEO equivalent)
      call this%mesh%compute_panel_geometry()

      ! Initialize wake (allocates wake node map)
      call this%wake%init(this%mesh%n_nodes)

      ! Allocate solution arrays
      if (allocated(this%gamma)) deallocate (this%gamma)
      if (allocated(this%cp)) deallocate (this%cp)
      allocate (this%gamma(this%mesh%n_panels))
      allocate (this%cp(this%mesh%n_panels))
      this%gamma = 0.0_wp
      this%cp = 0.0_wp

      call global_logger%msg(LOG_INFO, "UVLM solver initialized: "// &
                             trim(itoa(this%mesh%n_panels))//" panels, "// &
                             trim(itoa(this%mesh%n_nodes))//" nodes")

   end subroutine solver_init

   !> Update configuration (can be called before run)
   subroutine solver_set_config(this, config)
      class(uvlm_solver_t), intent(inout) :: this
      type(uvlm_config_t), intent(in)     :: config
      this%config = config
   end subroutine solver_set_config

   ! ═══════════════════════════════════════════════════════════════════════
   !                         EXECUTION
   ! ═══════════════════════════════════════════════════════════════════════

   !> Execute a single iteration of the UVLM algorithm
   !!
   !! Legacy equivalent: One pass through the main loop in PROGRAM ROS
   !!
   !! @param info  Status code: 0 = success, /= 0 = error
   subroutine solver_step(this, info)
      class(uvlm_solver_t), intent(inout) :: this
      integer, intent(out) :: info

      info = 0

      ! ── Wake operations (iter > 0) ────────────────────────────────────
      if (this%iter > 0) then
         call this%wake_timer%start()

         ! Shed new wake row from trailing edge
         call this%wake%shed(this%mesh, this%gamma, this%iter)

         ! Convect wake by freestream
         call this%wake%convect_freestream(this%config%vinit, this%config%dt, this%iter)

         ! Handle wake-body intersections (iter > 1)
         if (this%iter > 1) then
            call handle_wake_intersections(this%wake, this%iter)
            call correct_wake_penetrations(this%mesh, this%wake, this%iter)
         end if

         ! Free-wake relaxation
         call this%wake%relax(this%mesh, this%gamma, this%iter, &
                              this%config%dt, this%config%nsym, this%config%ngrnd)

         ! Post-relaxation corrections
         call handle_wake_intersections(this%wake, this%iter)
         if (this%iter > 1) then
            call correct_wake_penetrations(this%mesh, this%wake, this%iter)
         end if

         call this%wake_timer%stop()
      end if

      ! ── Build and solve linear system ──────────────────────────────────
      call this%solve_timer%start()

      call solve_system(this%linsys, this%mesh, this%wake, &
                        this%config%vinit, this%iter, &
                        this%config%nsym, this%config%ngrnd, &
                        this%config%vaip, this%gamma, info)

      call this%solve_timer%stop()

      if (info /= 0) then
         call global_logger%msg(LOG_WARN, "Linear system solver returned info="// &
                                trim(itoa(info)))
         return
      end if

      ! ── Compute forces (iter > 0) ─────────────────────────────────────
      if (this%iter > 0) then
         this%forces = compute_forces(this%mesh, this%wake, this%gamma, &
                                      this%config%vinit, this%config%rho, &
                                      this%iter, this%config%nsym, this%config%ngrnd)

         call global_logger%msg(LOG_INFO, "Iter "//trim(itoa(this%iter))// &
                                ": Lift="//trim(rtoa(this%forces%lift))// &
                                " Drag="//trim(rtoa(this%forces%drag))// &
                                " Side="//trim(rtoa(this%forces%side)))

         ! Check convergence
         if (abs(this%forces%lift) > 0.0_wp) then
            if (abs((this%lift_prev - this%forces%lift)/this%forces%lift) &
                <= this%config%eps) then
               this%converged = .true.
            end if
         end if

         this%lift_prev = this%forces%lift
      end if

      ! Advance iteration counter
      this%iter = this%iter + 1

   end subroutine solver_step

   !> Run the complete UVLM solution to convergence
   !!
   !! Legacy equivalent: The main DO loop in PROGRAM ROS with GO TO 93/95
   !!
   !! Iterates until either:
   !!   - Lift converges (relative change < eps)
   !!   - Maximum iterations reached
   subroutine solver_run(this, info)
      class(uvlm_solver_t), intent(inout) :: this
      integer, intent(out) :: info
      type(timer_t) :: total_timer

      call total_timer%start()

      call global_logger%msg(LOG_INFO, "Starting UVLM solver...")
      call global_logger%msg(LOG_INFO, "  Vinf  = "//trim(rtoa(this%config%vinit))//" m/s")
      call global_logger%msg(LOG_INFO, "  Alpha = "//trim(rtoa(this%config%alpha_deg))//" deg")
      call global_logger%msg(LOG_INFO, "  Eps   = "//trim(rtoa(this%config%eps)))

      ! Iteration 0: initial solve (no wake)
      call this%step(info)
      if (info /= 0) return

      ! Iterative wake convergence loop
      do while (.not. this%converged .and. this%iter <= this%config%max_iter)
         call this%step(info)
         if (info /= 0) return
      end do

      ! Post-processing: pressure coefficients
      if (this%config%vaip /= 0.0_wp) then
         call compute_pressure_coefficients(this%mesh, this%wake, this%gamma, &
                                            this%config%vinit, this%iter - 1, &
                                            this%config%nsym, this%config%ngrnd, this%cp)
      end if

      call total_timer%stop()

      if (this%converged) then
         call global_logger%msg(LOG_INFO, "UVLM converged in "// &
                                trim(itoa(this%iter - 1))//" iterations")
      else
         call global_logger%msg(LOG_WARN, "UVLM reached max iterations ("// &
                                trim(itoa(this%config%max_iter))//") without full convergence")
      end if

      call global_logger%msg(LOG_INFO, "Final: Lift="//trim(rtoa(this%forces%lift))// &
                             " Drag="//trim(rtoa(this%forces%drag)))
      call global_logger%msg(LOG_INFO, "Total time: "//trim(rtoa(total_timer%report()))//" s")
      call global_logger%msg(LOG_INFO, "  Solve time: "//trim(rtoa(this%solve_timer%report()))//" s")
      call global_logger%msg(LOG_INFO, "  Wake time:  "//trim(rtoa(this%wake_timer%report()))//" s")

   end subroutine solver_run

   ! ═══════════════════════════════════════════════════════════════════════
   !                         QUERY METHODS
   ! ═══════════════════════════════════════════════════════════════════════

   !> Get the latest computed aerodynamic forces
   pure function solver_get_forces(this) result(f)
      class(uvlm_solver_t), intent(in) :: this
      type(aero_forces_t) :: f
      f = this%forces
   end function solver_get_forces

   !> Get the vortex strength array
   function solver_get_gamma(this) result(g)
      class(uvlm_solver_t), intent(in) :: this
      real(wp), allocatable :: g(:)
      g = this%gamma
   end function solver_get_gamma

   !> Get current iteration count
   pure function solver_get_iter(this) result(n)
      class(uvlm_solver_t), intent(in) :: this
      integer(ip) :: n
      n = this%iter
   end function solver_get_iter

   !> Check convergence status
   pure function solver_is_converged(this) result(conv)
      class(uvlm_solver_t), intent(in) :: this
      logical :: conv
      conv = this%converged
   end function solver_is_converged

   ! ═══════════════════════════════════════════════════════════════════════
   !                    DATA EXPORT (FOR PLOTTING)
   ! ═══════════════════════════════════════════════════════════════════════

   !> Export body mesh panel corners as real arrays for plotting
   !!
   !! Returns the 4 corner coordinates of each body panel as (4, n_panels)
   !! arrays. If include_mirror is .true. and Y-symmetry is enabled,
   !! the mirrored (negative-Y) panels are appended.
   !!
   !! @param px             X-coordinates of corners (4, n_panels)
   !! @param py             Y-coordinates of corners (4, n_panels)
   !! @param pz             Z-coordinates of corners (4, n_panels)
   !! @param include_mirror If .true. and nsym=1, also export mirrored panels
   subroutine solver_export_mesh_panels(this, px, py, pz, include_mirror)
      class(uvlm_solver_t), intent(in) :: this
      real(wp), allocatable, intent(out) :: px(:, :), py(:, :), pz(:, :)
      logical, intent(in), optional :: include_mirror

      logical :: do_mirror
      integer(ip) :: i, c, np, idx

      do_mirror = .false.
      if (present(include_mirror)) do_mirror = include_mirror .and. (this%config%nsym == 1)

      np = this%mesh%n_panels
      if (do_mirror) np = np*2

      allocate (px(4, np), py(4, np), pz(4, np))

      do i = 1, this%mesh%n_panels
         do c = 1, 4
            idx = this%mesh%panels(i)%node_ids(c)
            px(c, i) = this%mesh%nodes(idx)%x
            py(c, i) = this%mesh%nodes(idx)%y
            pz(c, i) = this%mesh%nodes(idx)%z
         end do
      end do

      ! Append mirrored panels (Y -> -Y)
      if (do_mirror) then
         do i = 1, this%mesh%n_panels
            idx = this%mesh%n_panels + i
            px(:, idx) = px(:, i)
            py(:, idx) = -py(:, i)
            pz(:, idx) = pz(:, i)
         end do
      end if
   end subroutine solver_export_mesh_panels

   !> Export wake panel corners and strengths as real arrays for plotting
   !!
   !! Extracts corners and circulation strengths of all wake panels across
   !! all time rows. Output shape: (4, n_total) where
   !! n_total = n_wake_rows * n_wake_panels_per_row.
   !!
   !! @param px             X-coordinates of corners (4, n_total)
   !! @param py             Y-coordinates of corners (4, n_total)
   !! @param pz             Z-coordinates of corners (4, n_total)
   !! @param strengths      Per-panel circulation strength (n_total)
   !! @param include_mirror If .true. and nsym=1, also export mirrored panels
   subroutine solver_export_wake_panels(this, px, py, pz, strengths, include_mirror)
      class(uvlm_solver_t), intent(in) :: this
      real(wp), allocatable, intent(out) :: px(:, :), py(:, :), pz(:, :)
      real(wp), allocatable, intent(out) :: strengths(:)
      logical, intent(in), optional :: include_mirror

      type(vector3d_t) :: p1, p2, p3, p4
      logical :: do_mirror
      integer(ip) :: i, j, idx, n_rows, n_total, n_half

      do_mirror = .false.
      if (present(include_mirror)) do_mirror = include_mirror .and. (this%config%nsym == 1)

      n_rows = this%iter - 1  ! Number of wake time rows
      if (n_rows < 1 .or. this%wake%n_panels < 1) then
         allocate (px(4, 0), py(4, 0), pz(4, 0), strengths(0))
         return
      end if

      n_half = n_rows*this%wake%n_panels
      n_total = n_half
      if (do_mirror) n_total = n_half*2

      allocate (px(4, n_total), py(4, n_total), pz(4, n_total), strengths(n_total))

      idx = 0
      do i = 1, n_rows
         do j = 1, this%wake%n_panels
            idx = idx + 1
            call this%wake%get_panel_corners(i, j, p1, p2, p3, p4)
            px(:, idx) = [p1%x, p2%x, p3%x, p4%x]
            py(:, idx) = [p1%y, p2%y, p3%y, p4%y]
            pz(:, idx) = [p1%z, p2%z, p3%z, p4%z]
            strengths(idx) = this%wake%strengths(i, j)
         end do
      end do

      ! Append mirrored wake panels (Y -> -Y)
      if (do_mirror) then
         do i = 1, n_half
            idx = n_half + i
            px(:, idx) = px(:, i)
            py(:, idx) = -py(:, i)
            pz(:, idx) = pz(:, i)
            strengths(idx) = strengths(i)
         end do
      end if
   end subroutine solver_export_wake_panels

   ! ═══════════════════════════════════════════════════════════════════════
   !                         CLEANUP
   ! ═══════════════════════════════════════════════════════════════════════

   subroutine solver_destroy(this)
      class(uvlm_solver_t), intent(inout) :: this
      call this%mesh%destroy()
      call this%wake%destroy()
      call this%linsys%destroy()
      if (allocated(this%gamma)) deallocate (this%gamma)
      if (allocated(this%cp)) deallocate (this%cp)
      this%iter = 0
      this%converged = .false.
   end subroutine solver_destroy

   ! ═══════════════════════════════════════════════════════════════════════
   !                      UTILITY FUNCTIONS
   ! ═══════════════════════════════════════════════════════════════════════

   !> Integer to string (helper)
   function itoa(i) result(str)
      integer(ip), intent(in) :: i
      character(len=20) :: str
      write (str, '(I0)') i
      str = adjustl(str)
   end function itoa

   !> Real to string (helper)
   function rtoa(r) result(str)
      real(wp), intent(in) :: r
      character(len=24) :: str
      write (str, '(ES12.5)') r
      str = adjustl(str)
   end function rtoa

end module uvlm_solver_mod
