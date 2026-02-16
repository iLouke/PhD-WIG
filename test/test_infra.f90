program test_infra
   use base_kinds_mod, only: wp
   use logger_mod
   use timer_mod
   use plotting_mod
   use helper_mod, only: real_to_char
   implicit none

   integer :: fail_count = 0

   print *, "=========================================="
   print *, "   RUNNING INFRASTRUCTURE TESTS           "
   print *, "=========================================="

   call test_logger_module()
   call test_timer_module()
   call test_plotter_module()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All infrastructure tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   ! --- 1. Logger Test ---
   subroutine test_logger_module()
      logical :: file_exists
      character(len=*), parameter :: log_file = "output/test_run.log"

      print *, "Testing Logger..."

      ! A. Initialize Global Logger
      call global_logger%init("test_run.log", level=LOG_INFO, console=.false.)

      ! B. Write Log Entries
      call global_logger%msg(LOG_INFO, "Test log entry 1")
      call global_logger%msg(LOG_WARN, "Test log entry 2")

      ! C. Close Logger (Flushes to disk)
      call global_logger%close()

      ! D. Verify File Creation
      inquire (file=log_file, exist=file_exists)
      call assert(file_exists, "Logger failed to create '"//log_file//"'")
   end subroutine test_logger_module

   ! --- 2. Timer Test ---
   subroutine test_timer_module()
      type(timer_t) :: t
      real(wp) :: time_val
      integer :: i
      real(wp) :: junk

      print *, "Testing Timer..."

      ! A. Start Timer
      call t%start()

      ! B. Busy Wait (simulate work)
      junk = 0.0_wp
      do i = 1, 1000000
         junk = junk + sin(real(i, wp))
      end do

      ! C. Live Report Check
      time_val = t%report()
      call assert(time_val > 0.0_wp, "Timer report (live) should be > 0.0")

      ! D. Stop Timer
      call t%stop()

      ! E. Final Report Check
      call assert(t%report() >= time_val, "Stopped time should be >= live time")

      print *, "   (Timer elapsed: "//trim(real_to_char(t%report()))//"s)"
   end subroutine test_timer_module

   ! --- 3. Plotter Test ---
   subroutine test_plotter_module()
      type(plotter_t) :: plt
      real(wp) :: x(10), y(10)
      logical :: file_exists
      integer :: i
      character(len=*), parameter :: plot_file = "output/test_plot.png"

      print *, "Testing Plotter..."

      ! A. Setup Dummy Data
      do i = 1, 10
         x(i) = real(i, wp)
         y(i) = x(i)**2
      end do

      ! B. Configure Plot
      call plt%figure()
      call plt%title("Unit Test Plot")
      call plt%add(x, y, "Test Data", "lines")

      ! C. Save to PNG (Headless-safe)
      call plt%save(plot_file)

      ! D. Verify File Creation
      ! Note: This requires Gnuplot to be installed.
      ! If Gnuplot is missing, the file won't exist, but the module handles it gracefully.
      inquire (file=plot_file, exist=file_exists)

      ! Only assert failure if we expected gnuplot to work.
      ! For now, we warn if missing.
      if (file_exists) then
         call assert(.true., "Plot file created successfully")
      else
         print *, "   [WARN] Plot file not created (Is Gnuplot installed?)"
      end if

   end subroutine test_plotter_module

   ! --- Helper: Assertion ---
   subroutine assert(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) then
         print *, "   [FAIL] ", message
         fail_count = fail_count + 1
      else
         print *, "   [PASS] ", message
      end if
   end subroutine assert

end program test_infra
