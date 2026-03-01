module vehicle_mod
   use base_kinds_mod, only: wp, ip
   implicit none

   type :: vehicle_t
      real(wp) :: mass
      real(wp) :: velocity
      real(wp) :: position
   end type vehicle_t

end module vehicle_mod
