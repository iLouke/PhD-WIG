program phd
   use base_kinds_mod, only: wp, ip
   use helper_mod, only: real_to_char
   use logger_mod, only: global_logger, LOG_INFO, LOG_ERROR
   use timer_mod, only: timer_t
   use io_mod
   implicit none

   type(timer_t)       :: timer
   real(wp) :: max_time = 10.0_wp
   real(wp) :: time = 0.0_wp
   real(wp) :: dt = 0.01_wp

   call global_logger%init("simulation.log", level=0, console=.true.)
   call global_logger%msg(LOG_INFO, "=== WIG Simulation Starting ===")
   call timer%start()

   call setup_simulation()

   ! Time Stepping Loop
   do while (time < max_time)
      ! call get_aerodynamic(state, vehicle)
      ! call get_hydronamic(state, vehicle)
      ! call update_state(state)
      ! call write_output(state, time)
      time = time + dt
   end do

   call timer%stop()
   call global_logger%msg(LOG_INFO, "=== Simulation Complete ===")
   call global_logger%msg(LOG_INFO, "Simulation ran in "//trim(real_to_char(timer%report(), '(F10.3)'))//" seconds.")
   call global_logger%close()

contains

   subroutine setup_simulation()
      implicit none

      character(len=:), allocatable :: title
      character(len=:), allocatable :: io_message
      real(wp), allocatable :: spectrum(:)
      integer :: io_status

      call read_config_file("input.toml", title, spectrum, io_status, io_message)
      if (io_status /= 0) then
         call global_logger%msg(LOG_ERROR, "[IO] "//trim(io_message))
         error stop 1
      end if

      if (allocated(title)) then
         print '(a)', "Title: '"//title//"'"
      end if

      print '(*(g0, 1x))', "Entries:", size(spectrum)
      if (size(spectrum) > 0) then
         print '(*(g0, 1x))', "Spectrum:", spectrum
      end if
   end subroutine setup_simulation

   subroutine get_aerodynamic()
      ! Placeholder for aerodynamic calculations
   end subroutine get_aerodynamic

   subroutine get_hydronamic()
      ! Placeholder for hydrodynamic calculations
   end subroutine get_hydronamic

   subroutine update_state()
      ! Placeholder for state update logic (position, velocity, etc.)
   end subroutine update_state

end program phd
