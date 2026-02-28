program uvlm_demo
   !! ═══════════════════════════════════════════════════════════════════════
   !! UVLM Aerodynamic Solver
   !! ═══════════════════════════════════════════════════════════════════════
   !!
   !! Main entry point for UVLM analysis.
   !! Delegates all processing to the UVLM driver module.
   !!
   use base_kinds_mod, only: wp
   use timer_mod, only: timer_t
   use logger_mod, only: global_logger, LOG_INFO, LOG_DEBUG
   use helper_mod, only: real_to_char
   use driver_mod, only: uvlm_driver_t
   implicit none

   type(timer_t) :: timer
   type(uvlm_driver_t) :: driver

   ! Initialize logger and timer
   call global_logger%init("uvlm_demo.log", level=LOG_DEBUG)
   call global_logger%msg(LOG_INFO, "=== UVLM Aerodynamics Solver ===")
   call timer%start()

   ! Run UVLM analysis via driver
   call driver%init("config.ini")
   call driver%run()
   call driver%finalize()

   ! Report completion
   call timer%stop()
   call global_logger%msg(LOG_INFO, "=== Analysis Complete ===")
   call global_logger%msg(LOG_INFO, "Total wall time: "// &
                          trim(real_to_char(timer%report(), '(F10.4)'))//" s")
   call global_logger%close()

end program uvlm_demo
