program test_node
   use base_kinds_mod, only: wp
   use node_mod, only: node_t
   implicit none

   integer :: fail_count = 0
   real(wp), parameter :: TOL = 1.0e-10_wp

   print *, "=========================================="
   print *, "   RUNNING NODE MODULE TESTS              "
   print *, "=========================================="

   call test_node_initialization_and_get_set()
   call test_node_inherited_methods()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All node tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   subroutine test_node_initialization_and_get_set()
      type(node_t) :: nd1, nd2

      print *, "Testing node initialization and trailing edge flag..."

      ! 1. Default initialization (should default to false)
      nd1 = node_t(id=1, x=1.0_wp, y=0.0_wp, z=0.0_wp)
      call assert(.not. nd1%marked, "Default trailing edge flag is initialized to .false.")

      ! 2. Explicit initialization via custom constructor
      nd2 = node_t(id=2, x=2.0_wp, y=0.0_wp, z=0.0_wp, trailing_edge=.true.)
      call assert(nd2%marked, "Constructor explicitly sets trailing edge flag to .true.")

      ! 3. Setter test
      call nd1%set_trailing_edge(.true.)
      call assert(nd1%get_trailing_edge(), "Setter correctly updates trailing edge flag")
   end subroutine test_node_initialization_and_get_set

   subroutine test_node_inherited_methods()
      type(node_t) :: nd1, nd2
      real(wp) :: dist

      print *, "Testing inherited point_t methods..."

      nd1 = node_t(id=1, x=0.0_wp, y=0.0_wp, z=0.0_wp, trailing_edge=.true.)
      nd2 = node_t(id=2, coords=[3.0_wp, 4.0_wp, 0.0_wp], trailing_edge=.false.)

      ! 1. Test inherited translation
      call nd1%translate(1.0_wp, 1.0_wp, 0.0_wp)
      call assert_vector_close(nd1%coordinates, [1.0_wp, 1.0_wp, 0.0_wp], "Node successfully uses inherited translate()")

      ! 2. Test inherited distance calculation
      ! Move nd1 back to origin for a clean 3-4-5 triangle calculation
      call nd1%translate(-1.0_wp, -1.0_wp, 0.0_wp)
      dist = nd1%distance_to(nd2)
      call assert(abs(dist - 5.0_wp) < TOL, "Node successfully uses inherited distance_to() with another node")

   end subroutine test_node_inherited_methods

   ! ---------------------------------------------------------
   ! Helper Subroutines
   ! ---------------------------------------------------------

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

end program test_node
