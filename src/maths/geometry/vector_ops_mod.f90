module vector_ops_mod
   use base_kinds_mod, only: wp
   implicit none
   private

   public :: cross_product

contains

   pure function cross_product(a, b) result(c)
      real(wp), intent(in) :: a(3), b(3)
      real(wp) :: c(3)
      c(1) = a(2)*b(3) - a(3)*b(2)
      c(2) = a(3)*b(1) - a(1)*b(3)
      c(3) = a(1)*b(2) - a(2)*b(1)
   end function cross_product

end module vector_ops_mod
