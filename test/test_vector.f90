program test_vector
   use base_kinds_mod, only: wp
   use vector_mod, only: cross_product, normalise
   implicit none

   integer :: fail_count = 0
   real(wp), parameter :: TOL = 1.0e-12_wp

   print *, "=========================================="
   print *, "   RUNNING VECTOR GEOMETRY TESTS          "
   print *, "=========================================="

   call test_cross_product_basis()
   call test_cross_product_orthogonality()
   call test_normalise_nonzero_vector()
   call test_normalise_zero_vector()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All vector tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   subroutine test_cross_product_basis()
      real(wp) :: ex(3), ey(3), ez(3), result(3)

      print *, "Testing cross_product with basis vectors..."

      ex = [1.0_wp, 0.0_wp, 0.0_wp]
      ey = [0.0_wp, 1.0_wp, 0.0_wp]
      ez = [0.0_wp, 0.0_wp, 1.0_wp]

      result = cross_product(ex, ey)
      call assert_vector_close(result, ez, "ex x ey = ez")

      result = cross_product(ey, ex)
      call assert_vector_close(result, -ez, "ey x ex = -ez")
   end subroutine test_cross_product_basis

   subroutine test_cross_product_orthogonality()
      real(wp) :: a(3), b(3), c(3)

      print *, "Testing cross_product orthogonality..."

      a = [2.0_wp, -1.0_wp, 3.0_wp]
      b = [0.5_wp, 4.0_wp, -2.0_wp]
      c = cross_product(a, b)

      call assert(abs(dot_product(c, a)) < TOL, "(a x b) is orthogonal to a")
      call assert(abs(dot_product(c, b)) < TOL, "(a x b) is orthogonal to b")
   end subroutine test_cross_product_orthogonality

   subroutine test_normalise_nonzero_vector()
      real(wp) :: v(3), unit_v(3)

      print *, "Testing normalise with non-zero vector..."

      v = [3.0_wp, 4.0_wp, 0.0_wp]
      unit_v = normalise(v)

      call assert_vector_close(unit_v, [0.6_wp, 0.8_wp, 0.0_wp], "Normalised direction is correct")
      call assert(abs(norm2(unit_v) - 1.0_wp) < TOL, "Normalised vector has unit magnitude")
   end subroutine test_normalise_nonzero_vector

   subroutine test_normalise_zero_vector()
      real(wp) :: zero_v(3), unit_v(3)

      print *, "Testing normalise with zero vector..."

      zero_v = [0.0_wp, 0.0_wp, 0.0_wp]
      unit_v = normalise(zero_v)

      call assert_vector_close(unit_v, zero_v, "Zero vector remains zero after normalise")
   end subroutine test_normalise_zero_vector

   subroutine assert(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) then
         print *, "   [FAIL] ", message
         fail_count = fail_count + 1
      else
         print *, "   [PASS] ", message
      end if
   end subroutine assert

   subroutine assert_vector_close(v1, v2, message)
      real(wp), intent(in) :: v1(3), v2(3)
      character(len=*), intent(in) :: message

      call assert(all(abs(v1 - v2) < TOL), message)
   end subroutine assert_vector_close

end program test_vector
