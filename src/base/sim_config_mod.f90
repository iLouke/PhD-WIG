module sim_config_mod
   use base_kinds_mod, only: wp, ip
   use tomlf, only: toml_table, toml_array, get_value, len
   use io_mod, only: load_toml_file
   implicit none
   private

   public :: sim_config_t

   type :: sim_config_t
      character(len=100) :: title = ""
      real(wp) :: dt = 0.0_wp
      integer :: num_vehicles = 0
      character(len=256), allocatable :: vehicle_files(:)
   contains
      procedure :: parse => sim_config_parse
   end type sim_config_t

contains

   subroutine sim_config_parse(this, filename)
      class(sim_config_t), intent(inout) :: this
      character(len=*), intent(in) :: filename

      type(toml_table), allocatable :: table
      type(toml_array), pointer :: v_array
      integer :: status, i
      character(len=:), allocatable :: msg

      character(len=:), allocatable :: temp_str
      real(8) :: temp_dt

      call load_toml_file(filename, table, status, msg)
      if (status /= 0) error stop msg

      call get_value(table, "title", temp_str)
      if (allocated(temp_str)) then
         this%title = temp_str
         deallocate (temp_str)
      end if

      call get_value(table, "dt", temp_dt)
      this%dt = real(temp_dt, kind=wp)

      call get_value(table, "vehicles", v_array)
      if (associated(v_array)) then
         this%num_vehicles = len(v_array)
         allocate (this%vehicle_files(this%num_vehicles))

         do i = 1, this%num_vehicles
            call get_value(v_array, i, temp_str)
            if (allocated(temp_str)) then
               this%vehicle_files(i) = temp_str
               deallocate (temp_str)
            end if
         end do
      else
         error stop "sim_config_mod: No 'vehicles' array found in master config."
      end if

      if (allocated(table)) deallocate (table)
   end subroutine sim_config_parse

end module sim_config_mod
