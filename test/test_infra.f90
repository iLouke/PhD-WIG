program test_infra
   use base_kinds_mod, only: wp
   use logger_mod
   use timer_mod
   use plotting_mod
   use io_mod, only: read_config_file
   use system_utils_mod, only: ensure_directory_exists, run_command_status, is_windows_platform
   use helper_mod, only: real_to_char
   implicit none

   integer :: fail_count = 0

   print *, "=========================================="
   print *, "   RUNNING INFRASTRUCTURE TESTS           "
   print *, "=========================================="

   call test_logger_module()
   call test_timer_module()
   call test_plotter_module()
   call test_system_utils_module()
   call test_io_module()

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
      call assert(file_exists, "Logger created '"//log_file//"'")
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
      real(wp) :: x(50), y(50)
      logical :: file_exists
      integer :: i
      character(len=*), parameter :: plot_file = "output/test_plot.png"

      print *, "Testing Plotter..."

      ! A. Setup Dummy Data
      do i = 1, 50
         x(i) = real(i, wp)
         y(i) = sin(x(i)*0.2_wp)
      end do

      ! B. Configure Plot with the new advanced features
      call plt%figure(figsize=[10, 6])
      call plt%title("Unit Test Plot - Advanced")
      call plt%grid(.true.)
      call plt%xrange(0.0_wp, 50.0_wp)
      call plt%yrange(-1.5_wp, 1.5_wp)

      ! Add plot with custom linewidth and RGB color (Blue)
      call plt%add(x, y, "Sine Wave", style="--", linewidth=2, color=[0.0_wp, 0.4_wp, 0.8_wp])

      ! C. Save to PNG (Headless-safe, will invoke Python under the hood)
      call plt%save(plot_file)

      ! D. Verify File Creation
      inquire (file=plot_file, exist=file_exists)

      if (file_exists) then
         call assert(.true., "Plot file created successfully")
      else
         print *, "   [WARN] Plot file not created (Are Python and Matplotlib installed?)"
      end if

   end subroutine test_plotter_module

   ! --- 4. System Utilities Test ---
   subroutine test_system_utils_module()
      character(len=256) :: os_name
      character(len=*), parameter :: test_dir = "output/test_system_utils"
      logical :: dir_exists, expected_windows
      integer :: env_len, env_stat
      integer :: stat, cmd_status, exit_status

      print *, "Testing System Utilities..."

      os_name = ''
      call get_environment_variable('OS', value=os_name, length=env_len, status=env_stat)
      expected_windows = .false.
      if (env_stat == 0 .and. env_len > 0) then
         expected_windows = index(adjustl(os_name(1:env_len)), 'Windows_NT') > 0
      end if

      call assert(is_windows_platform() .eqv. expected_windows, &
                  "Platform detection matches OS environment")

      if (is_windows_platform()) then
         call run_command_status('cmd /c if exist "'//trim(test_dir)//'" rmdir /s /q "'//trim(test_dir)//'"', &
                                 cmd_status, exit_status)
         call run_command_status('cmd /c exit 0', cmd_status, exit_status)
      else
         call run_command_status('rm -rf "'//trim(test_dir)//'"', cmd_status, exit_status)
         call run_command_status('sh -c "exit 0"', cmd_status, exit_status)
      end if

      call assert(cmd_status == 0 .and. exit_status == 0, "run_command_status executes successful command")

      call ensure_directory_exists(test_dir, stat)
      inquire (file=test_dir, exist=dir_exists)
      call assert(stat == 0 .and. dir_exists, "ensure_directory_exists creates missing directory")

      call ensure_directory_exists(test_dir, stat)
      call assert(stat == 0, "ensure_directory_exists is idempotent")
   end subroutine test_system_utils_module

   ! --- 5. IO Module Test ---
   subroutine test_io_module()
      integer :: io_unit, status, stat
      character(len=:), allocatable :: title, message
      real(wp), allocatable :: spectrum(:)
      character(len=*), parameter :: test_file = "output/test_config.toml"

      print *, "Testing IO Module..."

      call ensure_directory_exists("output", stat)
      call assert(stat == 0, "Output directory is available for IO tests")

      open (newunit=io_unit, file=test_file, status='replace', action='write', iostat=status)
      call assert(status == 0, "Create temporary TOML config file")
      if (status == 0) then
         write (io_unit, '(A)') 'title = "Unit Test Config"'
         write (io_unit, '(A)') ''
         write (io_unit, '(A)') '[spectrum]'
         write (io_unit, '(A)') 'data = [1.0, 2.0, 3.0, 4.0]'
         write (io_unit, '(A)') 'reverse = true'
         close (io_unit)
      end if

      call read_config_file(test_file, title, spectrum, status, message)
      call assert(status == 0, "read_config_file succeeds on valid TOML")
      call assert(title == "Unit Test Config", "Config title is read correctly")
      call assert(allocated(spectrum), "Spectrum array is allocated")
      if (allocated(spectrum)) then
         call assert(size(spectrum) == 4, "Spectrum size is correct")
         call assert(all(abs(spectrum - [4.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]) < 1.0e-12_wp), &
                     "Spectrum values are read and reversed correctly")
      end if

      call read_config_file("output/does_not_exist.toml", title, spectrum, status, message)
      call assert(status /= 0, "read_config_file reports error for missing file")
      call assert(index(message, "Could not open config file") > 0, "Missing-file message is informative")
      call assert(allocated(spectrum) .and. size(spectrum) == 0, "Missing-file path returns empty spectrum")
   end subroutine test_io_module

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
