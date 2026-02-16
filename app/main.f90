program test_foundations
   use base_kinds_mod, only: wp
   use constants_mod, only: PI
   use math_utils_mod
   use timer_mod
   use logger_mod
   use plotting_mod
   use helper_mod, only: real_to_char
   implicit none

   ! --- Object Declarations ---
   type(timer_t)   :: main_timer
   type(plotter_t) :: plot1, plot2  ! Renamed for clarity (was plot, plot_)

   ! --- Data Variables ---
   real(wp) :: v1(3), v2(3), v3(3)
   real(wp) :: A(2, 2), B(2)
   integer  :: info

   real(wp), allocatable :: x(:), y1(:), y2(:)
   integer :: i, n_pts

   ! --- 1. Initialization ---
   n_pts = 100

   ! Initialize global_logger (File + Console enabled by default)
   call global_logger%init("foundation_test.log", level=LOG_DEBUG)

   call global_logger%msg(LOG_INFO, "Starting system test...")
   call main_timer%start()

   ! --- 2. Math & LAPACK Tests ---
   call global_logger%msg(LOG_DEBUG, "Running Vector/Matrix operations...")

   v1 = [1.0_wp, 0.0_wp, 0.0_wp]
   v2 = [0.0_wp, 1.0_wp, 0.0_wp]

   ! Assuming cross_product returns a real(wp) array of size 3
   v3 = cross_product(v1, v2)

   A = reshape([4.0_wp, 1.0_wp, 1.0_wp, 3.0_wp], [2, 2])
   B = [1.0_wp, 2.0_wp]

   ! Assuming solve_linear_system handles the LAPACK call
   call solve_linear_system(A, B, info)

   if (info == 0) then
      call global_logger%msg(LOG_INFO, "Linear system solved successfully.")
   else
      call global_logger%msg(LOG_ERROR, "Linear system solution failed.")
   end if

   ! --- 3. Advanced Plotting Test ---
   call global_logger%msg(LOG_INFO, "Infra: Generating Plot Data...")

   allocate (x(n_pts), y1(n_pts), y2(n_pts))

   do i = 1, n_pts
      ! Create x from 0 to 4*PI
      x(i) = real(i - 1, wp)*(4.0_wp*PI/real(n_pts - 1, wp))

      ! y1: Pure Sine
      y1(i) = sin(x(i))

      ! y2: Damped Cosine (e.g. system decay)
      y2(i) = cos(x(i))*exp(-0.2_wp*x(i))
   end do

   ! --- Plot 1: Standard Interactive View ---
   call global_logger%msg(LOG_INFO, "Infra: Rendering Figure 1 (Interactive)...")

   call plot1%figure()
   call plot1%title("System Response (Interactive)")
   call plot1%xlabel("Time (s)")
   call plot1%ylabel("Amplitude")
   call plot1%grid(.true.)
   call plot1%xrange(0.0_wp, 12.0_wp)

   ! Add Series with Styles
   call plot1%add(x, y1, "Pure Sine", "lines lw 2 lc rgb 'blue'")
   call plot1%add(x, y2, "Damped Cosine", "lines lw 2 lc rgb 'red' dt 2")

   call plot1%render() ! Opens Gnuplot Window

   ! --- Plot 2: Zoomed View (Saved to File) ---
   call global_logger%msg(LOG_INFO, "Infra: Rendering Figure 2 (PNG Export)...")

   call plot2%figure()
   call plot2%title("Zoomed Decay Analysis")
   call plot2%xlabel("Time (s)")
   call plot2%ylabel("Amplitude")
   call plot2%grid(.true.)

   ! Zoom in specifically on the tail end
   call plot2%xrange(2.0_wp, 10.0_wp)
   call plot2%yrange(-0.8_wp, 0.8_wp)

   call plot2%add(x, y2, "Decay Envelope", "linespoints pt 7 ps 0.5 lc rgb 'dark-green'")

   ! Save directly to file (No window needed)
   call plot2%save("output/damped_cosine_zoom.png")

   ! --- 4. Finalization ---
   call main_timer%stop()

   call global_logger%msg(LOG_INFO, "--- Test Complete ---")
   call global_logger%msg(LOG_INFO, "Wall Time: "//trim(real_to_char(main_timer%report()))//" s")
   call global_logger%close()

end program test_foundations
