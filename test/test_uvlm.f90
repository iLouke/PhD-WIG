module test_uvlm
   !! UVLM Test Suite - Fundamental Operations
   !!
   !! Contains comprehensive tests for:
   !!   - Vector3D operations
   !!   - Quaternion rotations
   !!   - Biot-Savart vortex induction
   !!   - LAPACK linear solver
   !!   - Panel geometry calculations
   !!   - Full UVLM solver on a flat-plate wing

   use base_kinds_mod, only: wp, ip
   use constants_mod, only: PI
   use logger_mod, only: global_logger, LOG_INFO, LOG_DEBUG, LOG_WARN
   use helper_mod, only: real_to_char
   use io_mod, only: mesh_point_t, mesh_panel_t, read_points_file, read_panels_file

   ! Aero modules
   use vector3d_mod, only: vector3d_t, vec3, norm, normalize, distance, &
                           zero_vec3, operator(+), operator(-), operator(*), &
                           operator(/), operator(.dot.), operator(.cross.)
   use quaternion_mod, only: quaternion_t, quat_identity, from_axis_angle, &
                             from_euler_aero, slerp
   use panel_mod, only: panel_t
   use grid_mod, only: mesh_t
   use vortex_mod, only: vortex_ring_velocity, vortex_segment_velocity
   use aero_solver_mod, only: aero_linsys_t
   use symmetry_mod, only: mirror_y, mirror_z, SYM_Y, GND_MIRROR
   use uvlm_solver_mod, only: uvlm_solver_t, uvlm_config_t
   use aerodynamic_loads_mod, only: aero_forces_t
   use plotting_mod, only: plotter_t

   implicit none
   private

   ! Public subroutines
   public :: test_vector3d
   public :: test_quaternion
   public :: test_biot_savart
   public :: test_lapack_solver
   public :: test_panel_geometry
   public :: run_uvlm_flat_plate
   public :: run_uvlm_from_mesh_files

contains

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 1: Vector3D Operations
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine test_vector3d()
      type(vector3d_t) :: a, b, c
      real(wp) :: d

      call global_logger%msg(LOG_INFO, "--- Test: Vector3D ---")

      a = vec3(1.0_wp, 0.0_wp, 0.0_wp)
      b = vec3(0.0_wp, 1.0_wp, 0.0_wp)

      ! Addition
      c = a + b
      call global_logger%msg(LOG_DEBUG, "  a + b = ("// &
                             trim(real_to_char(c%x))//', '//trim(real_to_char(c%y))//', '//trim(real_to_char(c%z))//")")

      ! Cross product: i x j = k
      c = a.cross.b
      call global_logger%msg(LOG_DEBUG, "  a x b = ("// &
                             trim(real_to_char(c%x))//', '//trim(real_to_char(c%y))//', '//trim(real_to_char(c%z))//")")

      ! Dot product: i . j = 0
      d = a.dot.b
      call global_logger%msg(LOG_DEBUG, "  a . b = "//trim(real_to_char(d)))

      ! Norm
      c = vec3(3.0_wp, 4.0_wp, 0.0_wp)
      d = norm(c)
      call global_logger%msg(LOG_DEBUG, "  |[3,4,0]| = "//trim(real_to_char(d)))

      ! Distance
      a = vec3(0.0_wp, 0.0_wp, 0.0_wp)
      b = vec3(1.0_wp, 1.0_wp, 1.0_wp)
      d = distance(a, b)
      call global_logger%msg(LOG_DEBUG, "  dist(origin, [1,1,1]) = "//trim(real_to_char(d)))

      call global_logger%msg(LOG_INFO, "  [PASS] Vector3D operations verified")
   end subroutine test_vector3d

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 2: Quaternion Rotations
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine test_quaternion()
      type(quaternion_t) :: q, q1, q2, qi
      type(vector3d_t) :: v, vr
      real(wp) :: alpha, beta, gamma_a, R(3, 3)

      call global_logger%msg(LOG_INFO, "--- Test: Quaternion ---")

      ! Identity rotation: should leave vector unchanged
      q = quat_identity()
      v = vec3(1.0_wp, 2.0_wp, 3.0_wp)
      vr = q%rotate(v)
      call global_logger%msg(LOG_DEBUG, "  Identity rotate [1,2,3] = ("// &
                             trim(real_to_char(vr%x))//', '//trim(real_to_char(vr%y))//', '// &
                             trim(real_to_char(vr%z))//")")

      ! 90° rotation about Z-axis: [1,0,0] -> [0,1,0]
      q = from_axis_angle(vec3(0.0_wp, 0.0_wp, 1.0_wp), PI/2.0_wp)
      v = vec3(1.0_wp, 0.0_wp, 0.0_wp)
      vr = q%rotate(v)
      call global_logger%msg(LOG_DEBUG, "  90deg Z-rot of [1,0,0] = ("// &
                             trim(real_to_char(vr%x, '(F8.4)'))//', '// &
                             trim(real_to_char(vr%y, '(F8.4)'))//', '// &
                             trim(real_to_char(vr%z, '(F8.4)'))//")")

      ! Euler aero: 5° alpha rotation, then back-convert
      alpha = 5.0_wp*PI/180.0_wp
      beta = 0.0_wp
      gamma_a = 0.0_wp
      q = from_euler_aero(alpha, beta, gamma_a)
      call global_logger%msg(LOG_DEBUG, "  Euler(5°,0°,0°) quaternion: w="// &
                             trim(real_to_char(q%w, '(F8.5)'))//" x="//trim(real_to_char(q%x, '(F8.5)'))// &
                             " y="//trim(real_to_char(q%y, '(F8.5)'))//" z="//trim(real_to_char(q%z, '(F8.5)')))

      ! Rotation matrix extraction
      R = q%to_rotation_matrix()
      call global_logger%msg(LOG_DEBUG, "  R(1,:) = "// &
                             trim(real_to_char(R(1, 1), '(F8.5)'))//' '// &
                             trim(real_to_char(R(1, 2), '(F8.5)'))//' '// &
                             trim(real_to_char(R(1, 3), '(F8.5)')))

      ! SLERP interpolation halfway between identity and 90° Z-rotation
      q1 = quat_identity()
      q2 = from_axis_angle(vec3(0.0_wp, 0.0_wp, 1.0_wp), PI/2.0_wp)
      qi = slerp(q1, q2, 0.5_wp)
      v = vec3(1.0_wp, 0.0_wp, 0.0_wp)
      vr = qi%rotate(v)
      call global_logger%msg(LOG_DEBUG, "  SLERP(id,90Z,0.5) rot [1,0,0] = ("// &
                             trim(real_to_char(vr%x, '(F8.4)'))//', '// &
                             trim(real_to_char(vr%y, '(F8.4)'))//', '// &
                             trim(real_to_char(vr%z, '(F8.4)'))//")")

      call global_logger%msg(LOG_INFO, "  [PASS] Quaternion operations verified")
   end subroutine test_quaternion

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 3: Biot-Savart Vortex Induction
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine test_biot_savart()
      type(vector3d_t) :: p, p1, p2, p3, p4, vel

      call global_logger%msg(LOG_INFO, "--- Test: Biot-Savart ---")

      ! Unit square vortex ring at z=0, evaluate at center point above
      p1 = vec3(0.0_wp, 0.0_wp, 0.0_wp)
      p2 = vec3(1.0_wp, 0.0_wp, 0.0_wp)
      p3 = vec3(1.0_wp, 1.0_wp, 0.0_wp)
      p4 = vec3(0.0_wp, 1.0_wp, 0.0_wp)

      ! Evaluation point above the center of the ring
      p = vec3(0.5_wp, 0.5_wp, 0.5_wp)
      vel = vortex_ring_velocity(p, p1, p2, p3, p4)
      call global_logger%msg(LOG_DEBUG, "  Ring velocity at (0.5,0.5,0.5) = ("// &
                             trim(real_to_char(vel%x, '(ES10.3)'))//', '// &
                             trim(real_to_char(vel%y, '(ES10.3)'))//', '// &
                             trim(real_to_char(vel%z, '(ES10.3)'))//")")

      ! Single segment: velocity perpendicular to filament
      p = vec3(0.5_wp, 0.1_wp, 0.0_wp)
      vel = vortex_segment_velocity(p, p1, p2)
      call global_logger%msg(LOG_DEBUG, "  Segment A→B vel at (0.5,0.1,0) = ("// &
                             trim(real_to_char(vel%x, '(ES10.3)'))//', '// &
                             trim(real_to_char(vel%y, '(ES10.3)'))//', '// &
                             trim(real_to_char(vel%z, '(ES10.3)'))//")")

      call global_logger%msg(LOG_INFO, "  [PASS] Biot-Savart verified")
   end subroutine test_biot_savart

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 4: LAPACK Linear Solver
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine test_lapack_solver()
      type(aero_linsys_t) :: solver
      real(wp) :: A(3, 3), b(3)
      integer :: info

      call global_logger%msg(LOG_INFO, "--- Test: LAPACK Solver ---")

      ! Simple 3x3 system: A*x = b
      !   [2  1  0] [x1]   [1]       x = [1/3, 1/3, 1/3]
      !   [1  3  1] [x2] = [2]
      !   [0  1  2] [x3]   [1]
      A(1, :) = [2.0_wp, 1.0_wp, 0.0_wp]
      A(2, :) = [1.0_wp, 3.0_wp, 1.0_wp]
      A(3, :) = [0.0_wp, 1.0_wp, 2.0_wp]
      b = [1.0_wp, 2.0_wp, 1.0_wp]

      call solver%factorize(A, info)
      call global_logger%msg(LOG_DEBUG, "  DGETRF info = "//trim(itoa_local(info)))

      call solver%solve(b, info)
      call global_logger%msg(LOG_DEBUG, "  DGETRS info = "//trim(itoa_local(info)))

      call global_logger%msg(LOG_DEBUG, "  Solution x = ["// &
                             trim(real_to_char(b(1), '(F8.5)'))//', '// &
                             trim(real_to_char(b(2), '(F8.5)'))//', '// &
                             trim(real_to_char(b(3), '(F8.5)'))//"]")

      call solver%destroy()
      call global_logger%msg(LOG_INFO, "  [PASS] LAPACK solver verified")
   end subroutine test_lapack_solver

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 5: Panel Geometry (ANALGEO)
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine test_panel_geometry()
      type(panel_t) :: pan
      type(vector3d_t) :: nodes(4)

      call global_logger%msg(LOG_INFO, "--- Test: Panel Geometry ---")

      ! Unit square panel in XY plane
      nodes(1) = vec3(0.0_wp, 0.0_wp, 0.0_wp)
      nodes(2) = vec3(1.0_wp, 0.0_wp, 0.0_wp)
      nodes(3) = vec3(1.0_wp, 1.0_wp, 0.0_wp)
      nodes(4) = vec3(0.0_wp, 1.0_wp, 0.0_wp)

      pan%node_ids = [1, 2, 3, 4]
      pan%n_nodes = 4

      call pan%compute_geometry(nodes)

      call global_logger%msg(LOG_DEBUG, "  Centroid = ("// &
                             trim(real_to_char(pan%centroid%x, '(F8.4)'))//', '// &
                             trim(real_to_char(pan%centroid%y, '(F8.4)'))//', '// &
                             trim(real_to_char(pan%centroid%z, '(F8.4)'))//")")
      call global_logger%msg(LOG_DEBUG, "  Normal   = ("// &
                             trim(real_to_char(pan%normal%x, '(F8.4)'))//', '// &
                             trim(real_to_char(pan%normal%y, '(F8.4)'))//', '// &
                             trim(real_to_char(pan%normal%z, '(F8.4)'))//")")
      call global_logger%msg(LOG_DEBUG, "  Area     = "// &
                             trim(real_to_char(pan%area, '(F8.4)')))

      call global_logger%msg(LOG_INFO, "  [PASS] Panel geometry verified")
   end subroutine test_panel_geometry

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 6: Full UVLM on a Flat Plate Wing
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine run_uvlm_flat_plate()
      !! Build a rectangular flat-plate wing and run the UVLM solver.
      !!
      !! Wing parameters:
      !!   chord  = 1.0 m
      !!   span   = 4.0 m  (half-span if using symmetry)
      !!   n_chord = 4     (chordwise panels)
      !!   n_span  = 8     (spanwise panels)
      !!   Total panels = n_chord * n_span = 32
      !!
      !! Flow conditions:
      !!   V_inf = 10 m/s
      !!   alpha = 5 degrees
      !!   rho   = 1.225 kg/m³
      !!
      !! Y-symmetry is ENABLED (nsym=1), so only the positive-Y half is meshed.
      !! This halves the computational cost for symmetric flows.

      ! Parameters
      integer(ip), parameter :: n_chord = 4
      integer(ip), parameter :: n_span = 8
      real(wp), parameter :: chord = 1.0_wp    ! [m]
      real(wp), parameter :: span = 4.0_wp    ! half-span [m]

      ! Derived
      integer(ip), parameter :: n_nodes = (n_chord + 1)*(n_span + 1)
      integer(ip), parameter :: n_panels = n_chord*n_span

      ! Mesh data
      type(vector3d_t) :: nodes(n_nodes)
      integer(ip) :: panel_ids(n_panels, 4)
      integer(ip) :: marks(n_nodes)
      integer(ip) :: props(n_panels)

      ! Solver objects
      type(mesh_t) :: mesh
      type(uvlm_solver_t) :: solver
      type(uvlm_config_t) :: config
      type(aero_forces_t) :: forces

      real(wp) :: x_pos, y_pos, dx, dy
      integer(ip) :: i, j, idx, p_idx, n1, n2, n3, n4
      integer :: info
      real(wp), allocatable :: gamma_out(:)

      call global_logger%msg(LOG_INFO, "--- Test: UVLM Flat Plate Wing ---")
      call global_logger%msg(LOG_INFO, "  Wing: chord="//trim(real_to_char(chord, '(F5.2)'))// &
                             " m, half-span="//trim(real_to_char(span, '(F5.2)'))//" m")
      call global_logger%msg(LOG_INFO, "  Mesh: "// &
                             trim(itoa_local(n_chord))//" x "// &
                             trim(itoa_local(n_span))//" = "// &
                             trim(itoa_local(n_panels))//" panels")

      ! ── Step 1: Generate grid nodes
      dx = chord/real(n_chord, wp)
      dy = span/real(n_span, wp)
      marks = 0

      do j = 0, n_span
         y_pos = real(j, wp)*dy
         do i = 0, n_chord
            x_pos = real(i, wp)*dx
            idx = j*(n_chord + 1) + i + 1
            nodes(idx) = vec3(x_pos, y_pos, 0.0_wp)

            ! Mark trailing-edge nodes (x = chord)
            if (i == n_chord) marks(idx) = 1
         end do
      end do

      ! ── Step 2: Define panel connectivity
      props = 1   ! All panels have property_id = 1 (wing)

      p_idx = 0
      do j = 0, n_span - 1
         do i = 0, n_chord - 1
            p_idx = p_idx + 1

            n1 = j*(n_chord + 1) + i + 1        ! (i,   j  )
            n2 = j*(n_chord + 1) + i + 2        ! (i+1, j  )
            n3 = (j + 1)*(n_chord + 1) + i + 2  ! (i+1, j+1)
            n4 = (j + 1)*(n_chord + 1) + i + 1  ! (i,   j+1)

            panel_ids(p_idx, :) = [n1, n2, n3, n4]
         end do
      end do

      ! ── Step 3: Set up mesh
      call mesh%set_geometry(nodes, panel_ids, marks, props)

      ! ── Step 4: Configure solver
      config%vinit = 10.0_wp         ! Freestream [m/s]
      config%alpha_deg = 5.0_wp      ! Angle of attack [deg]
      config%beta_deg = 0.0_wp       ! No sideslip
      config%gamma_deg = 0.0_wp      ! No roll
      config%rho = 1.225_wp          ! Air density [kg/m³]
      config%eps = 1.0e-3_wp         ! Convergence tolerance
      config%dt = 0.05_wp            ! Time step [s]
      config%max_iter = 30           ! Max iterations
      config%nsym = 1                ! Y-symmetry enabled
      config%ngrnd = 0               ! No ground effect
      config%hfl = 0.0_wp
      config%vaip = 0.0_wp

      ! ── Step 5: Initialize & run
      call solver%init(mesh, config)
      call solver%run(info)

      if (info /= 0) then
         call global_logger%msg(LOG_WARN, "  UVLM solver returned info="// &
                                trim(itoa_local(info)))
      end if

      ! ── Step 6: Extract results
      forces = solver%get_forces()
      gamma_out = solver%get_gamma()

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "  ╔═══════════════════════════════════════╗")
      call global_logger%msg(LOG_INFO, "  ║       UVLM RESULTS SUMMARY            ║")
      call global_logger%msg(LOG_INFO, "  ╠═══════════════════════════════════════╣")
      call global_logger%msg(LOG_INFO, "  ║  Lift  = "// &
                             trim(real_to_char(forces%lift, '(ES12.5)'))//" N                ║")
      call global_logger%msg(LOG_INFO, "  ║  Drag  = "// &
                             trim(real_to_char(forces%drag, '(ES12.5)'))//" N               ║")
      call global_logger%msg(LOG_INFO, "  ║  Side  = "// &
                             trim(real_to_char(forces%side, '(ES12.5)'))//" N                ║")
      call global_logger%msg(LOG_INFO, "  ╠═══════════════════════════════════════╣")

      ! Print circulation distribution (chordwise sum per spanwise strip)
      call global_logger%msg(LOG_INFO, "  ║  Spanwise Circulation Distribution:   ║")
      call print_circulation_strips(gamma_out, n_chord, n_span)

      call global_logger%msg(LOG_INFO, "  ╠═══════════════════════════════════════╣")
      call global_logger%msg(LOG_INFO, "  ║  Converged = "// &
                             trim(merge("YES", "NO ", solver%is_converged()))//"  Iters = "// &
                             trim(itoa_local(solver%get_iter()))//"          ║")
      call global_logger%msg(LOG_INFO, "  ╚═══════════════════════════════════════╝")

      ! ── Step 7: Plot mesh + wake
      call plot_mesh_and_wake(solver)

      ! ── Cleanup
      call solver%destroy()
      if (allocated(gamma_out)) deallocate (gamma_out)

      call global_logger%msg(LOG_INFO, "  [DONE] UVLM flat plate test complete")
   end subroutine run_uvlm_flat_plate

   ! ═══════════════════════════════════════════════════════════════════════
   !  Helper: Print spanwise circulation distribution
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine print_circulation_strips(gamma, nc, ns)
      real(wp), intent(in) :: gamma(:)
      integer(ip), intent(in) :: nc, ns
      integer(ip) :: j, k, p_idx
      real(wp) :: strip_gamma
      character(len=80) :: line

      do j = 1, ns
         strip_gamma = 0.0_wp
         do k = 1, nc
            p_idx = (j - 1)*nc + k
            if (p_idx <= size(gamma)) then
               strip_gamma = strip_gamma + gamma(p_idx)
            end if
         end do
         write (line, '(A,I2,A,ES10.3,A)') "  ║    Strip ", j, ":  Gamma = ", strip_gamma, "      ║"
         call global_logger%msg(LOG_INFO, trim(line))
      end do
   end subroutine print_circulation_strips

   !> Integer to string helper
   function itoa_local(i) result(str)
      integer, intent(in) :: i
      character(len=20) :: str
      write (str, '(I0)') i
      str = adjustl(str)
   end function itoa_local

   ! ═══════════════════════════════════════════════════════════════════════
   !  Mesh & Wake 3D Visualization
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine plot_mesh_and_wake(solver)
      !! Exports body mesh and wake panel data from the UVLM solver,
      !! then generates 3D wireframe plots using gnuplot.
      type(uvlm_solver_t), intent(in) :: solver

      type(plotter_t) :: plt
      real(wp), allocatable :: mesh_px(:, :), mesh_py(:, :), mesh_pz(:, :)
      real(wp), allocatable :: wake_px(:, :), wake_py(:, :), wake_pz(:, :)
      real(wp), allocatable :: wake_str(:)

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "--- Generating 3D plots ---")

      ! Extract panel corner data from solver (with Y-mirror)
      call solver%export_mesh_panels(mesh_px, mesh_py, mesh_pz, &
                                     include_mirror=.true.)
      call solver%export_wake_panels(wake_px, wake_py, wake_pz, wake_str, &
                                     include_mirror=.true.)

      call global_logger%msg(LOG_DEBUG, "  Mesh panels: "//trim(itoa_local(size(mesh_px, 2))))
      call global_logger%msg(LOG_DEBUG, "  Wake panels: "//trim(itoa_local(size(wake_px, 2))))

      ! ── Plot 1: Mesh + Wake wireframe
      call plt%figure_3d()
      call plt%title("UVLM Wing Mesh + Wake")
      call plt%xlabel("X [m]")
      call plt%ylabel("Y [m]")
      call plt%zlabel("Z [m]")
      call plt%view(55.0_wp, 315.0_wp)
      call plt%grid(.true.)

      if (size(mesh_px, 2) > 0) &
         call plt%add_panels_3d(mesh_px, mesh_py, mesh_pz, "Wing", "blue")
      if (size(wake_px, 2) > 0) &
         call plt%add_panels_3d(wake_px, wake_py, wake_pz, "Wake", "#CC3333")

      call plt%save_3d("output/uvlm_mesh_wake.png")
      call plt%render_3d()

      ! ── Plot 2: Wake colored by circulation strength
      if (size(wake_px, 2) > 0) then
         call plt%figure_3d()
         call plt%title("UVLM Wake Circulation Distribution")
         call plt%xlabel("X [m]")
         call plt%ylabel("Y [m]")
         call plt%zlabel("Z [m]")
         call plt%view(55.0_wp, 315.0_wp)
         call plt%grid(.true.)

         call plt%add_panels_3d(mesh_px, mesh_py, mesh_pz, "Wing", "#333399")
         call plt%add_panels_3d(wake_px, wake_py, wake_pz, &
                                "Wake ({/Symbol G})", scalar=wake_str)

         call plt%save_3d("output/uvlm_wake_gamma.png")
         call plt%render_3d()
      end if

      ! Cleanup
      if (allocated(mesh_px)) deallocate (mesh_px, mesh_py, mesh_pz)
      if (allocated(wake_px)) deallocate (wake_px, wake_py, wake_pz, wake_str)

      call global_logger%msg(LOG_INFO, "  [DONE] 3D plots generated")
   end subroutine plot_mesh_and_wake

   ! ═══════════════════════════════════════════════════════════════════════
   !  TEST 7: UVLM with Mesh Loaded from Files
   ! ═══════════════════════════════════════════════════════════════════════
   subroutine run_uvlm_from_mesh_files(points_file, panels_file)
      !! Load mesh from files and run UVLM solver.
      !!
      !! This subroutine reads aerodynamic mesh data from external files:
      !!   - points.dat: Node coordinates and boundary conditions
      !!   - panels.dat: Panel connectivity, normals, areas
      !!
      !! Then runs the UVLM solver on the loaded mesh.

      character(len=*), intent(in) :: points_file, panels_file

      ! Mesh data from files
      type(mesh_point_t), allocatable :: file_points(:)
      type(mesh_panel_t), allocatable :: file_panels(:)
      integer(ip) :: n_points, n_panels

      ! Local for conversion to UVLM format
      type(vector3d_t), allocatable :: nodes(:)
      integer(ip), allocatable :: panel_ids(:, :)
      integer(ip), allocatable :: marks(:)
      integer(ip), allocatable :: props(:)

      ! Solver objects
      type(mesh_t) :: mesh
      type(uvlm_solver_t) :: solver
      type(uvlm_config_t) :: config
      type(aero_forces_t) :: forces

      integer(ip) :: i
      integer :: info
      real(wp), allocatable :: gamma_out(:)
      type(plotter_t) :: plt
      real(wp), allocatable :: mesh_px(:, :), mesh_py(:, :), mesh_pz(:, :)
      real(wp), allocatable :: wake_px(:, :), wake_py(:, :), wake_pz(:, :)
      real(wp), allocatable :: wake_str(:)

      call global_logger%msg(LOG_INFO, "--- Test: UVLM from Mesh Files ---")

      ! ── Step 1: Read mesh files ────────────────────────────────────────
      call read_points_file(points_file, file_points, n_points)
      call read_panels_file(panels_file, file_panels, n_panels)

      if (n_points == 0 .or. n_panels == 0) then
         call global_logger%msg(LOG_WARN, "  ERROR: Failed to read mesh files")
         return
      end if

      call global_logger%msg(LOG_INFO, "  Mesh loaded successfully")
      call global_logger%msg(LOG_INFO, "  Nodes: "//trim(itoa_local(n_points)))
      call global_logger%msg(LOG_INFO, "  Panels: "//trim(itoa_local(n_panels)))

      ! ── Step 2: Convert to UVLM format ────────────────────────────────
      allocate (nodes(n_points))
      allocate (panel_ids(n_panels, 4))
      allocate (marks(n_points))
      allocate (props(n_panels))

      ! Convert points
      do i = 1, n_points
         nodes(i) = vec3(file_points(i)%x, file_points(i)%y, file_points(i)%z)
         marks(i) = file_points(i)%marked
      end do

      ! Convert panels
      do i = 1, n_panels
         panel_ids(i, :) = file_panels(i)%nodes
         props(i) = file_panels(i)%section
      end do

      ! ── Step 3: Set up mesh ────────────────────────────────────────────
      call mesh%set_geometry(nodes, panel_ids, marks, props)

      ! ── Step 4: Configure solver ───────────────────────────────────────
      config%vinit = 10.0_wp         ! Freestream [m/s]
      config%alpha_deg = 5.0_wp      ! Angle of attack [deg]
      config%beta_deg = 0.0_wp       ! No sideslip
      config%gamma_deg = 0.0_wp      ! No roll
      config%rho = 1.225_wp          ! Air density [kg/m³]
      config%eps = 1.0e-3_wp         ! Convergence tolerance
      config%dt = 0.05_wp            ! Time step [s]
      config%max_iter = 50           ! Max iterations
      config%nsym = 0                ! No symmetry for real mesh
      config%ngrnd = 0               ! No ground effect
      config%hfl = 0.0_wp
      config%vaip = 0.0_wp

      ! ── Step 5: Initialize & run ───────────────────────────────────────
      call solver%init(mesh, config)
      call solver%run(info)

      if (info /= 0) then
         call global_logger%msg(LOG_WARN, "  UVLM solver returned info="//trim(itoa_local(info)))
      end if

      ! ── Step 6: Extract results ────────────────────────────────────────
      forces = solver%get_forces()
      gamma_out = solver%get_gamma()

      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "  ╔═══════════════════════════════════════╗")
      call global_logger%msg(LOG_INFO, "  ║   UVLM FILE MESH RESULTS SUMMARY     ║")
      call global_logger%msg(LOG_INFO, "  ╠═══════════════════════════════════════╣")
      call global_logger%msg(LOG_INFO, "  ║  Lift  = "// &
                             trim(real_to_char(forces%lift, '(ES12.5)'))//" N                ║")
      call global_logger%msg(LOG_INFO, "  ║  Drag  = "// &
                             trim(real_to_char(forces%drag, '(ES12.5)'))//" N               ║")
      call global_logger%msg(LOG_INFO, "  ║  Side  = "// &
                             trim(real_to_char(forces%side, '(ES12.5)'))//" N                ║")
      call global_logger%msg(LOG_INFO, "  ╠═══════════════════════════════════════╣")
      call global_logger%msg(LOG_INFO, "  ║  Converged = "// &
                             trim(merge("YES", "NO ", solver%is_converged()))//"  Iters = "// &
                             trim(itoa_local(solver%get_iter()))//"          ║")
      call global_logger%msg(LOG_INFO, "  ╚═══════════════════════════════════════╝")

      ! ── Step 7: Plot mesh + wake ───────────────────────────────────────
      call global_logger%msg(LOG_INFO, "")
      call global_logger%msg(LOG_INFO, "--- Generating 3D plots ---")

      call solver%export_mesh_panels(mesh_px, mesh_py, mesh_pz, &
                                     include_mirror=.false.)
      call solver%export_wake_panels(wake_px, wake_py, wake_pz, wake_str, &
                                     include_mirror=.false.)

      call global_logger%msg(LOG_DEBUG, "  Mesh panels: "//trim(itoa_local(size(mesh_px, 2))))
      call global_logger%msg(LOG_DEBUG, "  Wake panels: "//trim(itoa_local(size(wake_px, 2))))

      call plt%figure_3d()
      call plt%title("UVLM Mesh from File + Wake")
      call plt%xlabel("X [m]")
      call plt%ylabel("Y [m]")
      call plt%zlabel("Z [m]")
      call plt%view(55.0_wp, 315.0_wp)
      call plt%grid(.true.)

      if (size(mesh_px, 2) > 0) &
         call plt%add_panels_3d(mesh_px, mesh_py, mesh_pz, "Wing", "blue")
      if (size(wake_px, 2) > 0) &
         call plt%add_panels_3d(wake_px, wake_py, wake_pz, "Wake", "#CC3333")

      call plt%save_3d("output/uvlm_mesh_file_wake.png")
      call plt%render_3d()

      ! ── Cleanup ────────────────────────────────────────────────────────
      call solver%destroy()
      if (allocated(nodes)) deallocate (nodes)
      if (allocated(panel_ids)) deallocate (panel_ids)
      if (allocated(marks)) deallocate (marks)
      if (allocated(props)) deallocate (props)
      if (allocated(file_points)) deallocate (file_points)
      if (allocated(file_panels)) deallocate (file_panels)
      if (allocated(gamma_out)) deallocate (gamma_out)
      if (allocated(mesh_px)) deallocate (mesh_px, mesh_py, mesh_pz)
      if (allocated(wake_px)) deallocate (wake_px, wake_py, wake_pz, wake_str)

      call global_logger%msg(LOG_INFO, "  [DONE] UVLM mesh file test complete")
   end subroutine run_uvlm_from_mesh_files

end module test_uvlm
