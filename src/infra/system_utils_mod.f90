module system_utils_mod
   implicit none
   private

   public :: ensure_directory_exists
   public :: run_command_status
   public :: is_windows_platform

contains

   subroutine run_command_status(command, cmd_status, exit_status)
      character(len=*), intent(in) :: command
      integer, intent(out) :: cmd_status
      integer, intent(out) :: exit_status

      call execute_command_line(trim(command), cmdstat=cmd_status, exitstat=exit_status)
   end subroutine run_command_status

   subroutine ensure_directory_exists(dir_path, stat)
      character(len=*), intent(in) :: dir_path
      integer, intent(out) :: stat

      logical :: dir_exists
      integer :: cmd_status, exit_status

      inquire (file=trim(dir_path), exist=dir_exists)
      if (dir_exists) then
         stat = 0
         return
      end if

      if (is_windows_platform()) then
         call run_command_status('cmd /c if not exist "'//trim(dir_path)//'" mkdir "'//trim(dir_path)//'"', &
                                 cmd_status, exit_status)
      else
         call run_command_status('mkdir -p "'//trim(dir_path)//'"', cmd_status, exit_status)
      end if

      inquire (file=trim(dir_path), exist=dir_exists)
      if (cmd_status == 0 .and. exit_status == 0 .and. dir_exists) then
         stat = 0
      else
         stat = 1
      end if
   end subroutine ensure_directory_exists

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

end module system_utils_mod
