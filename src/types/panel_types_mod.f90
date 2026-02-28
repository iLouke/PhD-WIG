module panel_types_mod
   use base_kinds_mod, only: wp, ip
   use point_types_mod, only: point_t
   implicit none
   private

   public :: panel_data_t

   type :: panel_data_t
      integer(ip) :: id = 0_ip
      integer(ip) :: nodes(4) = 0_ip
      type(point_t) :: points(4)
      integer(ip) :: section = 0_ip
      real(wp) :: normal(3) = 0.0_wp
      real(wp) :: collocation(3) = 0.0_wp
      real(wp) :: area = 0.0_wp
      real(wp) :: vorticity_strength = 0.0_wp
   end type panel_data_t

end module panel_types_mod
