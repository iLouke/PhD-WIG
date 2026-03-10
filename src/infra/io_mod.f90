module io_mod
   use base_kinds_mod, only: wp
   use tomlf, only: toml_table, toml_parse, toml_error
   implicit none
   private

   public :: load_toml_file

contains

   subroutine load_toml_file(filename, table, status, message)
      character(len=*), intent(in) :: filename
      type(toml_table), allocatable, intent(out) :: table
      integer, intent(out) :: status
      character(len=:), allocatable, intent(out) :: message

      type(toml_error), allocatable :: parse_error
      integer :: io_unit, io_stat

      status = 0
      message = ''

      open (file=trim(filename), newunit=io_unit, status='old', action='read', iostat=io_stat)
      if (io_stat /= 0) then
         status = io_stat
         message = "Could not open TOML file: "//trim(filename)
         return
      end if

      ! Parse απευθείας από το file unit
      call toml_parse(table, io_unit, parse_error)
      close (io_unit)

      if (allocated(parse_error)) then
         status = 1
         message = "TOML parse error in "//trim(filename)//": "//trim(parse_error%message)
         return
      end if
   end subroutine load_toml_file

end module io_mod
