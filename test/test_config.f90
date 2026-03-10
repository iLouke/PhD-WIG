program test_config
   use base_kinds_mod, only: wp, ip
   use sim_config_mod, only: sim_config_t
   use vehicle_config_mod, only: vehicle_config_t
   implicit none

   integer :: fail_count = 0
   real(wp), parameter :: TOL = 1.0e-10_wp

   print *, "=========================================="
   print *, "   RUNNING CONFIG MODULE TESTS            "
   print *, "=========================================="

   call create_dummy_toml_files()

   call test_sim_config_parsing()
   call test_vehicle_config_parsing()

   call cleanup_dummy_toml_files()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All config tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   subroutine test_sim_config_parsing()
      type(sim_config_t) :: sim_cfg

      print *, "Testing simulation config parsing..."

      call sim_cfg%parse("test_master.toml")

      call assert(trim(sim_cfg%title) == "WIG Multi-Vehicle Simulation", &
                  "Master config reads title correctly")

      call assert(abs(sim_cfg%dt - 0.05_wp) < TOL, &
                  "Master config reads dt (real number) correctly")

      call assert(sim_cfg%num_vehicles == 2, &
                  "Master config detects correct number of vehicles in array")

      call assert(allocated(sim_cfg%vehicle_files), &
                  "Master config allocates vehicle files array")

      if (allocated(sim_cfg%vehicle_files)) then
         call assert(trim(sim_cfg%vehicle_files(1)) == "test_vehicle_alpha.toml", &
                     "Master config reads first vehicle filename")
         call assert(trim(sim_cfg%vehicle_files(2)) == "test_vehicle_beta.toml", &
                     "Master config reads second vehicle filename")
      end if
   end subroutine test_sim_config_parsing

   subroutine test_vehicle_config_parsing()
      type(vehicle_config_t) :: v_cfg

      print *, "Testing vehicle config parsing..."

      call v_cfg%parse("test_vehicle_alpha.toml")

      call assert(trim(v_cfg%name) == "WIG_Alpha", &
                  "Vehicle config reads name correctly")

      call assert(v_cfg%points_number == 1500_ip, &
                  "Vehicle config reads points number correctly")

      call assert(trim(v_cfg%points_filename) == "points_alpha.dat", &
                  "Vehicle config reads points filename correctly")

      call assert(v_cfg%panels_number == 12434_ip, &
                  "Vehicle config reads panels number correctly")

      call assert(trim(v_cfg%panels_filename) == "panels_alpha.dat", &
                  "Vehicle config reads panels filename correctly")
   end subroutine test_vehicle_config_parsing

   subroutine create_dummy_toml_files()
      integer :: io_unit

      open (newunit=io_unit, file="test_master.toml", status="replace")
      write (io_unit, '(A)') 'title = "WIG Multi-Vehicle Simulation"'
      write (io_unit, '(A)') 'dt = 0.05'
      write (io_unit, '(A)') 'vehicles = ["test_vehicle_alpha.toml", "test_vehicle_beta.toml"]'
      close (io_unit)

      open (newunit=io_unit, file="test_vehicle_alpha.toml", status="replace")
      write (io_unit, '(A)') 'name = "WIG_Alpha"'
      write (io_unit, '(A)') '[mesh]'
      write (io_unit, '(A)') 'points_number = 1500'
      write (io_unit, '(A)') 'points_filename = "points_alpha.dat"'
      write (io_unit, '(A)') 'panels_number = 12434'
      write (io_unit, '(A)') 'panels_filename = "panels_alpha.dat"'
      close (io_unit)
   end subroutine create_dummy_toml_files

   subroutine cleanup_dummy_toml_files()
      integer :: io_unit, io_stat

      open (newunit=io_unit, file="test_master.toml", status="old", iostat=io_stat)
      if (io_stat == 0) close (io_unit, status="delete")

      open (newunit=io_unit, file="test_vehicle_alpha.toml", status="old", iostat=io_stat)
      if (io_stat == 0) close (io_unit, status="delete")
   end subroutine cleanup_dummy_toml_files

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

end program test_config
