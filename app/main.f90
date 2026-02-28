program phd
   !! ═══════════════════════════════════════════════════════════════════════
   !! PHD - WIG Simulation Program
   !! ═══════════════════════════════════════════════════════════════════════

   use timer_mod, only: timer_t
   use logger_mod, only: global_logger, LOG_INFO
   use setup_mod, only: simulation_setup_t
   implicit none

   type(timer_t) :: timer
   type(simulation_setup_t) :: sim_env

   ! 1. Initialize Environment
   call global_logger%init("simulation.log", level=LOG_INFO)
   call global_logger%msg(LOG_INFO, "=== WIG Simulation Starting ===")
   call timer%start()

   ! 2. Setup (Reads configs, loads vehicles, validates everything)
   call sim_env%initialize("config.ini")

   ! 3. Run (This is where the actual UVLM solver will go later)
   ! call sim_env%run()

   ! 4. Finalize
   call timer%stop()
   call global_logger%msg(LOG_INFO, "=== Simulation Complete ===")
   call global_logger%close()

end program phd
