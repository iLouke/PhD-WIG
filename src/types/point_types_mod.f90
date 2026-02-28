module point_types_mod
   use base_kinds_mod, only: wp, ip
   implicit none
   private

   public :: point_t

   type :: point_t
      integer(ip) :: id = 0_ip
      real(wp) :: x = 0.0_wp
      real(wp) :: y = 0.0_wp
      real(wp) :: z = 0.0_wp
      integer(ip) :: marked = 0_ip
   end type point_t

end module point_types_mod
