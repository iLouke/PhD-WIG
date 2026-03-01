module state_mod
   use base_kinds_mod, only: wp, ip
   implicit none

   type :: state_t
      real(wp) :: time
      real(wp) :: position
      real(wp) :: velocity
      real(wp) :: acceleration
   end type state_t

end module state_mod
