program phd
   !! ═══════════════════════════════════════════════════════════════════════
   !! PHD - WIG Simulation Program
   !! ═══════════════════════════════════════════════════════════════════════

   use base_kinds_mod, only: wp, ip
   use helper_mod, only: real_to_char
   use logger_mod, only: global_logger, LOG_INFO
   use timer_mod, only: timer_t
   use plotting_mod, only: plotter_t, check_matplotlib
   implicit none

   type(timer_t)       :: timer

   call global_logger%init("simulation.log", level=0, console=.true.)
   call global_logger%msg(LOG_INFO, "=== WIG Simulation Starting ===")
   call timer%start()

   call timer%stop()
   call global_logger%msg(LOG_INFO, "=== Simulation Complete ===")
   call global_logger%msg(LOG_INFO, "Simulation ran in "//trim(real_to_char(timer%report(), '(F10.3)'))//" seconds.")
   call global_logger%close()

end program phd
