program z_up_free_fall
   use base_kinds_mod, only: wp
   use constants_mod, only: PI
   use rigid_body_mod
   use plotting_mod
   use logger_mod
   use helper_mod, only: real_to_char
   implicit none

   ! --- Objects ---
   type(rigid_body_t) :: body
   type(plotter_t)    :: plt_pos

   ! --- Simulation Parameters ---
   real(wp), parameter :: dt = 0.01_wp
   real(wp), parameter :: t_end = 10.0_wp
   real(wp), parameter :: mass = 1.0_wp
   real(wp), parameter :: g = 9.81_wp
   integer :: n_steps, i
   integer :: n_steps_g, n_steps_free

   ! --- Data Recording ---
   real(wp), allocatable :: t_hist(:)
   real(wp), allocatable :: x_hist(:)
   real(wp), allocatable :: y_hist(:)
   real(wp), allocatable :: z_hist(:) ! Altitude (+Z)
   real(wp), allocatable :: w_hist(:) ! Vertical Velocity (+Z is up)

   real(wp) :: inertia(3, 3), force(3)
   real(wp) :: current_pos(3), current_vel(3)
   real(wp) :: energy_ref, rel_err_energy, rel_err_lin, rel_err_ang
   real(wp) :: lin_mom_ref(3), ang_mom_ref(3)
   real(wp) :: force_impulse(3)
   logical :: ok

   ! =================================================================
   ! 1. SETUP (Start at 1000m Altitude)
   ! =================================================================
   call global_logger%init("z_up_test.log", level=LOG_INFO)
   call global_logger%msg(LOG_INFO, "Initializing Z-UP Free Fall...")

   n_steps = int(t_end/dt)
   allocate (t_hist(n_steps), x_hist(n_steps), y_hist(n_steps), z_hist(n_steps), w_hist(n_steps))

   ! Simple Inertia
   inertia = 0.0_wp
   inertia(1, 1) = 1.0_wp; inertia(2, 2) = 1.0_wp; inertia(3, 3) = 1.0_wp

   ! Init at Z = 1000m (HIGH UP)
   call body%init(mass, inertia, init_pos=[0.0_wp, 0.0_wp, 1000.0_wp])

   ! =================================================================
   ! 2. SIMULATION LOOP
   ! =================================================================
   do i = 1, n_steps

      force = [mass*g*cos(PI*real(i, wp)/n_steps), mass*g*sin(PI*real(i, wp)/n_steps), -mass*g]

      call body%add_force(force, [0.0_wp, 0.0_wp, 0.0_wp])
      call body%step(dt)

      ! Record
      current_pos = body%get_pos()
      current_vel = body%get_vel()

      t_hist(i) = real(i, wp)*dt
      x_hist(i) = current_pos(1) ! X (should stay near 0)
      y_hist(i) = current_pos(2) ! Y (should stay near 0
      z_hist(i) = current_pos(3) ! Altitude
      w_hist(i) = current_vel(3) ! Vertical Speed (will go negative)
   end do

   call global_logger%msg(LOG_INFO, "Sim Complete.")
   call global_logger%msg(LOG_INFO, "Final Alt: "//trim(real_to_char(z_hist(n_steps)))//" m")
   call global_logger%msg(LOG_INFO, "Final Vel: "//trim(real_to_char(w_hist(n_steps)))//" m/s")

   ! =================================================================
   ! 3. PLOTTING (Intuitive Z-Up Graphs)
   ! =================================================================

   call plt_pos%figure()
   call plt_pos%title("X Direction")
   call plt_pos%xlabel("Time (s)")
   call plt_pos%ylabel("X Position (m)")
   call plt_pos%grid(.true.)
   call plt_pos%add(t_hist, x_hist, "X Position", "lines lw 2 lc rgb 'blue'")
   call plt_pos%save("output/x_vs_time.png")

   call plt_pos%figure()
   call plt_pos%title("Y Direction")
   call plt_pos%xlabel("Time (s)")
   call plt_pos%ylabel("Y Position (m)")
   call plt_pos%grid(.true.)
   call plt_pos%add(t_hist, y_hist, "Y Position", "lines lw 2 lc rgb 'green'")
   call plt_pos%save("output/y_vs_time.png")

   call plt_pos%figure()
   call plt_pos%title("Z Direction")
   call plt_pos%xlabel("Time (s)")
   call plt_pos%ylabel("Z Position (m)")
   call plt_pos%grid(.true.)
   call plt_pos%add(t_hist, z_hist, "Z Position", "lines lw 2 lc rgb 'green'")
   call plt_pos%save("output/z_vs_time.png")

   ! =================================================================
   ! 4. GRAVITY HELPER EXAMPLE (No Plots)
   ! =================================================================
   call global_logger%msg(LOG_INFO, "Running gravity helper example...")

   call body%init(mass, inertia, init_pos=[0.0_wp, 0.0_wp, 100.0_wp])
   n_steps_g = int(2.0_wp/dt)
   do i = 1, n_steps_g
      call body%add_gravity()
      call body%step(dt)
   end do
   current_pos = body%get_pos()
   current_vel = body%get_vel()
   call global_logger%msg(LOG_INFO, "Gravity helper final Alt: "//trim(real_to_char(current_pos(3)))//" m")
   call global_logger%msg(LOG_INFO, "Gravity helper final Vel: "//trim(real_to_char(current_vel(3)))//" m/s")

   ! =================================================================
   ! 5. ENERGY/MOMENTUM CHECK (Impulse then Free Flight)
   ! =================================================================
   call global_logger%msg(LOG_INFO, "Running energy/momentum check...")

   call body%init(mass, inertia, init_pos=[0.0_wp, 0.0_wp, 0.0_wp])
   force_impulse = [1.0_wp, 0.0_wp, 0.0_wp]
   call body%add_force(force_impulse, [0.0_wp, 0.0_wp, 0.0_wp])
   call body%step(dt)

   call body%get_energy_momentum(energy_ref, lin_mom_ref, ang_mom_ref)

   n_steps_free = 500
   do i = 1, n_steps_free
      call body%step(dt)
   end do

   call body%check_energy_momentum(energy_ref, lin_mom_ref, ang_mom_ref, &
                                   rel_err_energy, rel_err_lin, rel_err_ang, ok)

   call global_logger%msg(LOG_INFO, "Energy rel err: "//trim(real_to_char(rel_err_energy)))
   call global_logger%msg(LOG_INFO, "Lin mom rel err: "//trim(real_to_char(rel_err_lin)))
   call global_logger%msg(LOG_INFO, "Ang mom rel err: "//trim(real_to_char(rel_err_ang)))
   if (ok) then
      call global_logger%msg(LOG_INFO, "Energy/momentum check: PASS")
   else
      call global_logger%msg(LOG_INFO, "Energy/momentum check: FAIL")
   end if

   call global_logger%close()

end program z_up_free_fall
