module io_mod
   use base_kinds_mod, only: wp
   use tomlf, only: toml_table, toml_array, toml_parse, toml_error, get_value, len
   implicit none
   private

   public :: read_data, read_config_file

contains

   subroutine read_config_file(filename, title, spectrum, status, message)
      character(len=*), intent(in) :: filename
      character(len=:), allocatable, intent(out) :: title
      real(wp), allocatable, intent(out) :: spectrum(:)
      integer, intent(out) :: status
      character(len=:), allocatable, intent(out) :: message

      type(toml_table), allocatable :: table
      type(toml_error), allocatable :: parse_error
      integer :: io_unit, io_stat

      status = 0
      message = ''

      open (file=trim(filename), newunit=io_unit, status='old', action='read', iostat=io_stat)
      if (io_stat /= 0) then
         status = io_stat
         message = "Could not open config file: "//trim(filename)
         if (.not. allocated(spectrum)) allocate (spectrum(0))
         return
      end if

      call toml_parse(table, io_unit, parse_error)
      close (io_unit)

      if (allocated(parse_error)) then
         status = 1
         message = "TOML parse error in "//trim(filename)//": "//trim(parse_error%message)
         if (.not. allocated(spectrum)) allocate (spectrum(0))
         return
      end if

      call read_data(table, title, spectrum)
   end subroutine read_config_file

   subroutine read_data(table, title, spectrum)
      type(toml_table), intent(inout) :: table
      character(len=:), allocatable, intent(out) :: title
      real(wp), allocatable, intent(out) :: spectrum(:)

      type(toml_table), pointer :: child
      type(toml_array), pointer :: array
      logical :: reverse
      integer :: ival

      ! Get character value from entry "title"
      call get_value(table, "title", title)

      ! Get subtable reference from entry "spectrum"
      call get_value(table, "spectrum", child)

      ! Get array reference from entry "data"
      call get_value(child, "data", array)

      ! Read all values from the data array
      allocate (spectrum(len(array)))
      do ival = 1, size(spectrum)
         call get_value(array, ival, spectrum(ival))
      end do

      ! Data is stored in reverse order
      call get_value(child, "reverse", reverse, .false.)
      if (reverse) spectrum(:) = spectrum(size(spectrum):1:-1)
   end subroutine read_data

end module io_mod
