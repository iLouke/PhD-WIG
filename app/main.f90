program test_foundations
    use base_kinds_mod
    use constants_mod
    use math_utils_mod
    use timer_mod
    use logger_mod
    use plotting_mod
    implicit none

    type(timer_t)   :: main_timer
    type(logger_t)  :: log
    type(plotter_t) :: plot
    
    real(wp) :: v1(3), v2(3), v3(3)
    real(wp) :: A(2,2), B(2)
    integer  :: info
    
    real(wp), allocatable :: x(:), y1(:), y2(:)
    integer :: i, n_pts = 100

    call log%init("foundation_test.log", LOG_DEBUG)
    call log%log_msg(LOG_INFO, "Starting system test...")
    call main_timer%start()

    ! --- Math & LAPACK Tests (Kept same as before) ---
    v1 = [1.0_wp, 0.0_wp, 0.0_wp]
    v2 = [0.0_wp, 1.0_wp, 0.0_wp]
    v3 = cross_product(v1, v2)

    A = reshape([4.0_wp, 1.0_wp, 1.0_wp, 3.0_wp], [2,2])
    B = [1.0_wp, 2.0_wp]
    call solve_linear_system(A, B, info)

    ! --- Advanced Plotting Test ---
    allocate(x(n_pts), y1(n_pts), y2(n_pts))
    
    do i = 1, n_pts
        x(i)  = real(i-1, wp) * (4.0_wp * PI / real(n_pts-1, wp)) ! 0 to 4pi
        y1(i) = sin(x(i))
        y2(i) = cos(x(i)) * exp(-0.2_wp * x(i)) ! Damped cosine
    end do
    
    call log%log_msg(LOG_INFO, "Infra: Launching Advanced Plot...")
    
    ! 1. Initialize
    call plot%figure()
    
    ! 2. Configure (MATLAB style)
    call plot%title("Damped System Response")
    call plot%xlabel("Time (s)")
    call plot%ylabel("Amplitude")
    call plot%grid(.true.)
    call plot%xrange(0.0_wp, 12.0_wp) ! Zoom in slightly
    
    ! 3. Add Data Series
    !    Arg 4 is the Gnuplot style string: 
    !    "lines lw 2 lc 'blue'", "points pt 7", "linespoints", etc.
    call plot%add(x, y1, "Pure Sine",      "lines lw 2 lc rgb 'blue'")
    call plot%add(x, y2, "Damped Cosine",  "lines lw 2 lc rgb 'red' dt 2") ! dt 2 = dashed
    
    ! 4. Render
    call plot%render()
    
    ! --- Finish ---
    call main_timer%stop()
    print *, "--- Test Complete ---"
    print *, "Wall Time:", main_timer%report(), "s"
    
    call log%finalize()
end program test_foundations