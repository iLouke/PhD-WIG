module math_constants_mod
   use base_kinds_mod, only: wp
   implicit none
   private

   public :: PI, TWO_PI, HALF_PI
   public :: deg2rad, rad2deg
   public :: SMALL, ZERO, ONE

   ! Mathematical Constants
   real(wp), parameter :: PI = 4.0_wp*atan(1.0_wp)
   real(wp), parameter :: TWO_PI = 2.0_wp*PI
   real(wp), parameter :: HALF_PI = PI/2.0_wp
   ! Conversion Factors
   real(wp), parameter :: SMALL = 1.0e-12_wp
   real(wp), parameter :: ZERO = 0.0_wp
   real(wp), parameter :: ONE = 1.0_wp
   ! Angle Conversion
   real(wp), parameter :: deg2rad = PI/180.0_wp
   real(wp), parameter :: rad2deg = 180.0_wp/PI
end module math_constants_mod
