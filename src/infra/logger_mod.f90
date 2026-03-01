module logger_mod
   use base_kinds_mod, only: wp
   implicit none
   private

   ! Public Types and Constants
   public :: logger_t
   public :: LOG_DEBUG, LOG_INFO, LOG_WARN, LOG_ERROR

   ! Log Levels
   integer, parameter :: LOG_DEBUG = 0
   integer, parameter :: LOG_INFO = 1
   integer, parameter :: LOG_WARN = 2
   integer, parameter :: LOG_ERROR = 3

   type :: logger_t
      private
      integer :: file_unit = -1
      integer :: level = LOG_INFO
      logical :: console_output = .true.
      logical :: file_enabled = .false.
   contains
      procedure, public :: init => logger_init
      procedure, public :: msg => logger_write
      procedure, public :: close => logger_close
      procedure, public :: set_level => logger_set_level
      procedure, public :: set_console => logger_set_console
   end type logger_t

   ! Global Logger Instance (Optional)
   public :: global_logger
   type(logger_t), save :: global_logger

contains

   !> Initialize the logger
   !> @param filename: Name of the log file
   !> @param level: (Optional) Initial log level
   !> @param console: (Optional) Enable/Disable console output (default: .true.)
   subroutine logger_init(this, filename, level, console)
      class(logger_t), intent(inout) :: this
      character(len=*), intent(in)   :: filename
      integer, intent(in), optional  :: level
      logical, intent(in), optional  :: console

      integer :: stat
      character(len=256) :: full_path

      ! Defaults
      if (present(level)) this%level = level
      if (present(console)) this%console_output = console

      ! Directory creation (Graceful fallback)
      call create_output_directory(stat)
      if (stat /= 0) then
         print *, "[LOGGER] WARNING: Could not create 'output/' directory. Logging to current dir."
         full_path = trim(filename)
      else
         full_path = "output/"//trim(filename)
      end if

      ! Open Log File with Error Checking
      open (newunit=this%file_unit, file=trim(full_path), &
            status='unknown', position='append', action='write', iostat=stat)

      if (stat /= 0) then
         print *, "[LOGGER] ERROR: Failed to open log file: ", trim(full_path)
         this%file_unit = -1
         this%file_enabled = .false.
      else
         this%file_enabled = .true.
         call this%msg(LOG_INFO, "--- Logger Initialized ---")
      end if

   end subroutine logger_init

   !> Set the logging level dynamically
   subroutine logger_set_level(this, level)
      class(logger_t), intent(inout) :: this
      integer, intent(in) :: level
      this%level = level
   end subroutine logger_set_level

   !> Enable/Disable console output
   subroutine logger_set_console(this, enable)
      class(logger_t), intent(inout) :: this
      logical, intent(in) :: enable
      this%console_output = enable
   end subroutine logger_set_console

   !> Write a message to the log
   subroutine logger_write(this, level, message)
      class(logger_t), intent(in)  :: this
      integer, intent(in)          :: level
      character(len=*), intent(in) :: message

      character(len=10) :: lvl_str
      character(len=8)  :: date_vals
      character(len=10) :: time_vals
      character(len=30) :: timestamp

      ! Filter based on log level
      if (level < this%level) return

      ! Determine Level String
      select case (level)
      case (LOG_DEBUG); lvl_str = "[DEBUG]"
      case (LOG_INFO); lvl_str = "[INFO] "
      case (LOG_WARN); lvl_str = "[WARN] "
      case (LOG_ERROR); lvl_str = "[ERROR]"
      case default; lvl_str = "[UNKNOWN]"
      end select

      ! Construct Timestamp: YYYY-MM-DD HH:MM:SS
      call date_and_time(date=date_vals, time=time_vals)
      timestamp = date_vals(1:4)//"-"//date_vals(5:6)//"-"//date_vals(7:8)//" "// &
                  time_vals(1:2)//":"//time_vals(3:4)//":"//time_vals(5:6)

      ! Terminal Output
      if (this%console_output) then
         write (*, '(A, " | ", A, " | ", A)') trim(timestamp), trim(lvl_str), trim(message)
      end if

      ! File Output
      if (this%file_enabled .and. this%file_unit /= -1) then
         write (this%file_unit, '(A, " | ", A, " | ", A)') trim(timestamp), trim(lvl_str), trim(message)
         flush (this%file_unit)
      end if
   end subroutine logger_write

   !> Close the logger
   subroutine logger_close(this)
      class(logger_t), intent(inout) :: this

      if (this%file_enabled .and. this%file_unit /= -1) then
         call this%msg(LOG_INFO, "--- Logger Finalized ---")
         close (this%file_unit)
         this%file_unit = -1
         this%file_enabled = .false.
      end if
   end subroutine logger_close

   subroutine create_output_directory(stat)
      integer, intent(out) :: stat

      logical :: dir_exists
      integer :: cmdstat, exitstat

      inquire (file='output', exist=dir_exists)
      if (dir_exists) then
         stat = 0
         return
      end if

      if (is_windows_platform()) then
         call execute_command_line('cmd /c if not exist output mkdir output', &
                                   cmdstat=cmdstat, exitstat=exitstat)
      else
         call execute_command_line('mkdir -p output', cmdstat=cmdstat, exitstat=exitstat)
      end if

      inquire (file='output', exist=dir_exists)
      if (cmdstat == 0 .and. exitstat == 0 .and. dir_exists) then
         stat = 0
      else
         stat = 1
      end if
   end subroutine create_output_directory

   logical function is_windows_platform() result(is_windows)
      character(len=64) :: os_name
      integer :: env_len, env_stat

      os_name = ''
      call get_environment_variable('OS', value=os_name, length=env_len, status=env_stat)

      if (env_stat == 0 .and. env_len > 0) then
         is_windows = index(adjustl(os_name(1:env_len)), 'Windows_NT') > 0
      else
         is_windows = .false.
      end if
   end function is_windows_platform

end module logger_mod
