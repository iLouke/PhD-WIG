module setup_mod
   !! Handles the initialization and orchestration of the entire simulation.

   use base_kinds_mod, only: wp, ip
   use logger_mod, only: global_logger, LOG_INFO, LOG_ERROR
   use io_mod, only: config_t
   implicit none
   private

   public :: simulation_setup_t

   ! Main object that holds the entire state of your simulation
   type :: simulation_setup_t
      type(config_t) :: master_config
      type(config_t), allocatable :: vehicle_configs(:)

      ! We can store global variables here later:
      ! real(wp) :: dt_global, dt_aero
      ! integer(ip) :: num_vehicles
   contains
      procedure :: initialize => setup_initialize
   end type simulation_setup_t

contains

   !> Orchestrates the loading of all configuration files
   subroutine setup_initialize(this, master_filename)
      class(simulation_setup_t), intent(inout) :: this
      character(len=*), intent(in) :: master_filename

      integer(ip) :: num_vehicles
      integer :: i
      character(len=64) :: v_key
      character(len=:), allocatable :: v_filename

      call global_logger%msg(LOG_INFO, "[Setup] Initializing Simulation...")

      ! 1. Read the Master Config
      call this%master_config%read(master_filename, log=.true.)
      if (.not. this%master_config%is_loaded()) then
         error stop "[Setup] FATAL: Could not load master config."
      end if

      ! 2. Find out how many vehicles we have
      num_vehicles = this%master_config%get_int("vehicles", "number_of_vehicles", 0_ip)

      if (num_vehicles <= 0) then
         call global_logger%msg(LOG_ERROR, "[Setup] No vehicles defined in master config.")
         error stop
      end if

      ! 3. Allocate the array of vehicle config objects
      allocate (this%vehicle_configs(num_vehicles))
      call global_logger%msg(LOG_INFO, "[Setup] Preparing to load vehicles...")

      ! 4. Loop through and load each vehicle's specific .ini file
      do i = 1, num_vehicles
         ! Dynamically build the key, e.g., "vehicle_1"
         write (v_key, '("vehicle_", I0)') i

         ! Get the filename from the master config
         v_filename = this%master_config%get_string("vehicles", trim(v_key), "")

         if (len_trim(v_filename) == 0) then
            call global_logger%msg(LOG_ERROR, "[Setup] Missing filename for "//trim(v_key))
            error stop
         end if

         ! Read and validate the vehicle configuration
         call this%vehicle_configs(i)%read(v_filename, log=.true.)
         call this%vehicle_configs(i)%validate()
      end do

      call global_logger%msg(LOG_INFO, "[Setup] All configurations successfully loaded and validated.")

   end subroutine setup_initialize

end module setup_mod
