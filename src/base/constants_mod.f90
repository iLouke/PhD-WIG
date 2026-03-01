module constants_mod
   use base_kinds_mod, only: wp
   implicit none
   private
   public :: PI, RHO_AIR, RHO_WATER, G_ACCEL, SMALL
   public :: deg2rad, rad2deg

   ! Mathematical Constants
   real(wp), parameter :: PI = 4.0_wp*atan(1.0_wp)

   ! Physical Constants (Defaults - can be used as reference)
   real(wp), parameter :: RHO_AIR = 1.225_wp       ! [kg/m^3] ISA Sea Level
   real(wp), parameter :: RHO_WATER = 1025.0_wp      ! [kg/m^3] Seawater
   real(wp), parameter :: G_ACCEL = 9.80665_wp     ! [m/s^2] Standard Gravity

   ! Numerical Constants
   ! Used for core-radii in vortices or divide-by-zero protection
   real(wp), parameter :: SMALL = 1.0e-12_wp

   real(wp), parameter :: deg2rad = PI/180.0_wp
   real(wp), parameter :: rad2deg = 180.0_wp/PI
end module constants_mod
