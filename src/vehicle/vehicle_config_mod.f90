module vehicle_config_mod
   use base_kinds_mod, only: wp, ip
   use tomlf, only: toml_table, get_value
   use io_mod, only: load_toml_file
   implicit none
   private

   public :: vehicle_config_t

   type :: vehicle_config_t
      character(len=50) :: name = ""
      integer(ip) :: points_number = 0
      character(len=256) :: points_filename = ""
      integer(ip) :: panels_number = 0
      character(len=256) :: panels_filename = ""
   contains
      procedure :: parse => vehicle_config_parse
   end type vehicle_config_t

contains

   subroutine vehicle_config_parse(this, filename)
      class(vehicle_config_t), intent(inout) :: this
      character(len=*), intent(in) :: filename

      type(toml_table), allocatable :: table
      type(toml_table), pointer :: mesh_table
      integer :: status
      character(len=:), allocatable :: msg

      ! Συμβατές μεταβλητές με toml-f
      character(len=:), allocatable :: temp_str
      integer :: temp_int

      call load_toml_file(filename, table, status, msg)
      if (status /= 0) error stop msg

      ! Ανάγνωση του ονόματος
      call get_value(table, "name", temp_str)
      if (allocated(temp_str)) then
         this%name = temp_str
         deallocate (temp_str)
      end if

      ! Ανάγνωση του [mesh] block
      call get_value(table, "mesh", mesh_table)
      if (associated(mesh_table)) then

         call get_value(mesh_table, "points_number", temp_int)
         this%points_number = int(temp_int, kind=ip)

         call get_value(mesh_table, "points_filename", temp_str)
         if (allocated(temp_str)) then
            this%points_filename = temp_str
            deallocate (temp_str)
         end if

         call get_value(mesh_table, "panels_number", temp_int)
         this%panels_number = int(temp_int, kind=ip)

         call get_value(mesh_table, "panels_filename", temp_str)
         if (allocated(temp_str)) then
            this%panels_filename = temp_str
            deallocate (temp_str)
         end if

      else
         error stop "vehicle_config_mod: Missing [mesh] in "//trim(filename)
      end if

      if (allocated(table)) deallocate (table)
   end subroutine vehicle_config_parse

end module vehicle_config_mod
