program test_vector
   use base_kinds_mod, only: wp
   use vector_mod, only: vector_t, operator(*), operator(.x.)
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
      type(vector_t) :: ex, ey, ez, result

      print *, "Testing cross_product with basis vectors..."

      ex = vector_t(1.0_wp, 0.0_wp, 0.0_wp)
      ey = vector_t(0.0_wp, 1.0_wp, 0.0_wp)
      ez = vector_t(0.0_wp, 0.0_wp, 1.0_wp)

      result = ex.x.ey
      call assert_vector_close(result, ez, "ex x ey = ez")

      result = ey.x.ex
      call assert_vector_close(result, (-1.0_wp)*ez, "ey x ex = -ez")
   end subroutine test_cross_product_basis

   subroutine test_cross_product_orthogonality()
      type(vector_t) :: a, b, c

      print *, "Testing cross_product orthogonality..."

      a = vector_t(2.0_wp, -1.0_wp, 3.0_wp)
      b = vector_t(0.5_wp, 4.0_wp, -2.0_wp)
      c = a.x.b

      ! Notice the overloaded dot product using (*)
      call assert(abs(c*a) < TOL, "(a x b) is orthogonal to a")
      call assert(abs(c*b) < TOL, "(a x b) is orthogonal to b")
   end subroutine test_cross_product_orthogonality

   subroutine test_normalise_nonzero_vector()
      type(vector_t) :: v, unit_v
      type(vector_t) :: expected

      print *, "Testing normalise with non-zero vector..."

      v = vector_t(3.0_wp, 4.0_wp, 0.0_wp)
      unit_v = v%normalise()
      expected = vector_t(0.6_wp, 0.8_wp, 0.0_wp)

      call assert_vector_close(unit_v, expected, "Normalised direction is correct")
      call assert(abs(unit_v%magnitude() - 1.0_wp) < TOL, "Normalised vector has unit magnitude")
   end subroutine test_normalise_nonzero_vector

   subroutine test_normalise_zero_vector()
      type(vector_t) :: zero_v, unit_v

      print *, "Testing normalise with zero vector (NaN safety check)..."

      zero_v = vector_t(0.0_wp, 0.0_wp, 0.0_wp)
      unit_v = zero_v%normalise()

      call assert_vector_close(unit_v, zero_v, "Zero vector safely returns zero without NaN")
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
      type(vector_t), intent(in) :: v1, v2
      character(len=*), intent(in) :: message

      call assert(all(abs(v1%components - v2%components) < TOL), message)
   end subroutine assert_vector_close

end program test_vector
