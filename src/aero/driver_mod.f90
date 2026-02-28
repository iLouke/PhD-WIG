module driver_mod
   !! ═══════════════════════════════════════════════════════════════════════
   !! UVLM Driver Module
   !! ═══════════════════════════════════════════════════════════════════════
   !!
   !! Encapsulates the complete UVLM analysis workflow:
   !! 1. Load configuration and mesh data
   !! 2. Visualize initial geometry
   !! 3. Execute UVLM solver
   !! 4. Report results
   !!
   use base_kinds_mod, only: wp, ip
   use constants_mod, only: PI
   use logger_mod, only: global_logger, LOG_INFO, LOG_DEBUG
   use io_mod, only: config_t, config_read, mesh_point_t, mesh_panel_t, &
                     read_points_file, read_panels_file
   use helper_mod, only: real_to_char
   use vector3d_mod, only: vector3d_t, vec3
   use grid_mod, only: mesh_t
   use uvlm_solver_mod, only: uvlm_solver_t, uvlm_config_t
   use plotting_mod, only: plotter_t
   implicit none

   private
   public :: uvlm_driver_t

   !> Main UVLM driver type
   type :: uvlm_driver_t
      type(config_t) :: config
      type(mesh_point_t), allocatable :: mesh_points(:)
      type(mesh_panel_t), allocatable :: mesh_panels(:)
      integer(ip) :: n_points = 0
      integer(ip) :: n_panels = 0
      logical :: is_initialized = .false.
   contains
      procedure :: init => driver_init
      procedure :: run => driver_run
      procedure :: finalize => driver_finalize
      procedure :: plot_mesh => driver_plot_mesh
      procedure :: solve => driver_solve
   end type uvlm_driver_t

contains

   !> Initialize driver: load configuration and mesh
   subroutine driver_init(this, config_file)
      class(uvlm_driver_t), intent(inout) :: this
      character(len=*), intent(in) :: config_file

      character(len=256) :: points_file, panels_file

      ! Load configuration
      call config_read(this%config, config_file)
      if (.not. this%config%is_loaded()) then
         call global_logger%msg(LOG_INFO, "WARNING: Configuration not loaded properly")
         return
      end if

      call global_logger%msg(LOG_INFO, "Configuration loaded successfully")

      ! Get mesh file paths from config
      points_file = trim(this%config%get_string("mesh", "points_file", "points.dat"))
      panels_file = trim(this%config%get_string("mesh", "panels_file", "panels.dat"))

      ! Read mesh files
      call read_points_file(points_file, this%mesh_points, this%n_points)
      call read_panels_file(panels_file, this%mesh_panels, this%n_panels)

      this%is_initialized = .true.

   end subroutine driver_init

   !> Execute the complete UVLM workflow
   subroutine driver_run(this)
      class(uvlm_driver_t), intent(inout) :: this

      if (.not. this%is_initialized) then
         call global_logger%msg(LOG_INFO, "ERROR: Driver not initialized")
         return
      end if

      ! Stage 1: Visualize mesh
      call this%plot_mesh()

      ! Stage 2: Run UVLM solver
      call this%solve()

   end subroutine driver_run

   !> Finalize driver and clean up
   subroutine driver_finalize(this)
      class(uvlm_driver_t), intent(inout) :: this

      if (allocated(this%mesh_points)) deallocate (this%mesh_points)
      if (allocated(this%mesh_panels)) deallocate (this%mesh_panels)

      this%is_initialized = .false.

   end subroutine driver_finalize

   !> Plot mesh geometry
   subroutine driver_plot_mesh(this)
      class(uvlm_driver_t), intent(in) :: this

      type(plotter_t) :: plt
      real(wp), allocatable :: px(:, :), py(:, :), pz(:, :)
      integer(ip) :: i, j

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "--- Plotting Initial Mesh ---")

      ! Allocate panel corner arrays
      allocate (px(4, this%n_panels), py(4, this%n_panels), pz(4, this%n_panels))

      ! Convert panels to corner coordinates
      do i = 1, this%n_panels
         do j = 1, 4
            if (this%mesh_panels(i)%nodes(j) > 0 .and. &
                this%mesh_panels(i)%nodes(j) <= this%n_points) then
               px(j, i) = this%mesh_points(this%mesh_panels(i)%nodes(j))%x
               py(j, i) = this%mesh_points(this%mesh_panels(i)%nodes(j))%y
               pz(j, i) = this%mesh_points(this%mesh_panels(i)%nodes(j))%z
            end if
         end do
      end do

      ! Create 3D plot
      call plt%figure_3d()
      call plt%title("UVLM Mesh Geometry")
      call plt%xlabel("X [m]")
      call plt%ylabel("Y [m]")
      call plt%zlabel("Z [m]")
      call plt%view(55.0_wp, 315.0_wp)
      call plt%grid(.true.)

      if (this%n_panels > 0) then
         call plt%add_panels_3d(px, py, pz, "Wing", "blue")
      end if

      call plt%save_3d("output/mesh_geometry.png")
      call plt%render_3d()

      deallocate (px, py, pz)
      call global_logger%msg(LOG_INFO, "  [DONE] Mesh plot saved")

   end subroutine driver_plot_mesh

   !> Run UVLM solver
   subroutine driver_solve(this)
      class(uvlm_driver_t), intent(in) :: this

      type(vector3d_t), allocatable :: nodes(:)
      integer(ip), allocatable :: panel_ids(:, :), marks(:), props(:)
      type(mesh_t) :: mesh
      type(uvlm_solver_t) :: solver
      type(uvlm_config_t) :: config
      integer(ip) :: i, info
      real(wp), allocatable :: gamma_out(:)

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "--- UVLM Solver ---")

      ! Convert mesh format
      allocate (nodes(this%n_points))
      allocate (panel_ids(this%n_panels, 4))
      allocate (marks(this%n_points))
      allocate (props(this%n_panels))

      do i = 1, this%n_points
         nodes(i) = vec3(this%mesh_points(i)%x, this%mesh_points(i)%y, this%mesh_points(i)%z)
         marks(i) = this%mesh_points(i)%marked
      end do

      do i = 1, this%n_panels
         panel_ids(i, :) = this%mesh_panels(i)%nodes
         props(i) = this%mesh_panels(i)%section
      end do

      ! Setup mesh
      call mesh%set_geometry(nodes, panel_ids, marks, props)

      ! Configure solver from config
      config%vinit = this%config%get_real("simulation", "velocity_inf", 10.0_wp)
      config%alpha_deg = this%config%get_real("simulation", "angle_of_attack", 5.0_wp)
      config%beta_deg = 0.0_wp
      config%gamma_deg = 0.0_wp
      config%rho = 1.225_wp
      config%eps = this%config%get_real("solver", "convergence_tol", 1.0e-3_wp)
      config%dt = 0.05_wp
      config%max_iter = this%config%get_integer("solver", "max_iterations", 50_ip)
      config%nsym = 0
      config%ngrnd = 0
      config%hfl = 0.0_wp
      config%vaip = 0.0_wp

      call global_logger%msg(LOG_INFO, "  Vinf = "//trim(real_to_char(config%vinit))//" m/s")
      call global_logger%msg(LOG_INFO, "  Alpha = "//trim(real_to_char(config%alpha_deg))//" deg")

      ! Initialize and run solver
      call solver%init(mesh, config)
      call solver%run(info)

      ! Extract results
      gamma_out = solver%get_gamma()

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "  ╔═══════════════════════════════════════╗")
      call global_logger%msg(LOG_INFO, "  ║       UVLM RESULTS                    ║")
      call global_logger%msg(LOG_INFO, "  ╠═══════════════════════════════════════╣")
      call global_logger%msg(LOG_INFO, "  ║  Converged: "// &
                             trim(merge("YES", "NO ", solver%is_converged()))// &
                             "             Iters: "//trim(itoa(solver%get_iter()))//" ║")
      call global_logger%msg(LOG_INFO, "  ╚═══════════════════════════════════════╝")

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "--- Solution Complete ---")
      call global_logger%msg(LOG_INFO, "  Initial mesh plot saved to: output/mesh_geometry.png")

      ! Cleanup
      call solver%destroy()
      if (allocated(nodes)) deallocate (nodes)
      if (allocated(panel_ids)) deallocate (panel_ids)
      if (allocated(marks)) deallocate (marks)
      if (allocated(props)) deallocate (props)
      if (allocated(gamma_out)) deallocate (gamma_out)

   end subroutine driver_solve

   !> Convert integer to string
   function itoa(i) result(str)
      integer, intent(in) :: i
      character(len=20) :: str
      write (str, '(I0)') i
      str = adjustl(str)
   end function itoa

end module driver_mod
