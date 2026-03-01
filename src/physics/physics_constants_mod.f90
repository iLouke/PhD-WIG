module physics_constants_mod
   use base_kinds_mod, only: wp
   implicit none

   ! Physical Constants (Defaults - can be used as reference)
   real(wp), parameter :: RHO_AIR = 1.225_wp       ! [kg/m^3] ISA Sea Level
   real(wp), parameter :: RHO_WATER = 1025.0_wp      ! [kg/m^3] Seawater
   real(wp), parameter :: G_ACCEL = 9.80665_wp     ! [m/s^2] Standard Gravity

end module physics_constants_mod
