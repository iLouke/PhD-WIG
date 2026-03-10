program test_euler
   use base_kinds_mod, only: wp
   use math_constants_mod, only: PI
   use helper_mod, only: print_matrix
   use euler_mod
   implicit none

   integer :: fail_count = 0
   real(wp), parameter :: TOL = 1.0e-7_wp ! Tolerance for floating point comparisons

   print *, "=========================================="
   print *, "   RUNNING EULER DYNAMICS TESTS           "
   print *, "=========================================="

   call test_radian_degree_handling()
   call test_single_rotations()
   call test_combined_rotation()
   call test_arbitrary_rotation()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All Euler tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   subroutine test_radian_degree_handling()
      real(wp) :: R_rad(3, 3), R_deg(3, 3)

      print *, "Testing Radian vs Degree Handling..."

      R_rad = get_single_rotation_matrix(PI*0.5, 'roll', rads=.true.)
      R_deg = get_single_rotation_matrix(90.0_wp, 'roll')

      call assert_matrix_equal(R_rad, R_deg, "Radian and Degree inputs produce the same rotation matrix")
   end subroutine test_radian_degree_handling

   ! --- 1. Single Rotation Matrix Tests ---
   subroutine test_single_rotations()
      real(wp) :: R(3, 3), R_expected(3, 3)
      real(wp) :: c, s, one = 1.0_wp, zero = 0.0_wp

      print *, "Testing Single Rotations (90 degrees)..."
      c = cos(PI*0.5_wp)
      s = sin(PI*0.5_wp)

      ! A. Test Roll / X-axis (pi/2 radians)
      R = get_single_rotation_matrix(PI*0.5_wp, 'roll', rads=.true.)
      R_expected = reshape((/one, zero, zero, zero, c, s, zero, -s, c/), shape(R_expected))

      call assert_matrix_equal(R, R_expected, "Roll (X-axis) matrix matches theory")
      !   print *, "   --- Roll (X-axis) Matrix ---"
      !   call print_matrix(R)

      ! B. Test Pitch / Y-axis (90 deg)
      R = get_single_rotation_matrix(90.0_wp, 'pitch')
      R_expected = reshape((/c, zero, -s, zero, one, zero, s, zero, c/), shape(R_expected))

      call assert_matrix_equal(R, R_expected, "Pitch (Y-axis) matrix matches theory")
      !   print *, "   --- Pitch (Y-axis) Matrix ---"
      !   call print_matrix(R)
      ! C. Test Yaw / Z-axis (90 deg)
      R = get_single_rotation_matrix(90.0_wp, 'yaw')
      R_expected = reshape((/c, s, zero, -s, c, zero, zero, zero, one/), shape(R_expected))

      call assert_matrix_equal(R, R_expected, "Yaw (Z-axis) matrix matches theory")
      !   print *, "   --- Yaw (Z-axis) Matrix ---"
      !   call print_matrix(R)
   end subroutine test_single_rotations

   ! --- 2. Combined Rotation Matrix Test ---
   subroutine test_combined_rotation()
      real(wp) :: R(3, 3), R_manual(3, 3)
      real(wp) :: roll, pitch, yaw
      real(wp) :: cosg, sing, cosb, sinb, cosa, sina

      print *, "Testing Combined Extrinsic Rotation (Rx * Ry * Rz)..."

      roll = 30.0_wp
      pitch = 45.0_wp
      yaw = 60.0_wp

      sing = sind(roll)
      cosg = cosd(roll)
      sinb = sind(pitch)
      cosb = cosd(pitch)
      sina = sind(yaw)
      cosa = cosd(yaw)
      ! Get combined matrix directly
      R = get_rotation_matrix(roll, pitch, yaw)

      R_manual = reshape((/cosa*cosb, sina*cosb, -sinb, &
                           cosa*sinb*sing - sina*cosg, sina*sinb*sing + cosa*cosg, cosb*sing, &
                           cosa*sinb*cosg + sina*sing, sina*sinb*cosg - cosa*sing, cosb*cosg/), shape(R_manual))

      call assert_matrix_equal(R, R_manual, "Combined matrix matches Rx*Ry*Rz sequence")

      !   print *, "   --- Combined Matrix (R = Rx*Ry*Rz) ---"
      !   call print_matrix(R)

   end subroutine test_combined_rotation

   ! --- 3. Arbitrary Rotation Matrix Test ---
   subroutine test_arbitrary_rotation()
      real(wp) :: R_arb(3, 3), R_std(3, 3)
      real(wp) :: axis(3)

      print *, "Testing Arbitrary Axis Rotation (Rodrigues' Formula)..."

      ! If we rotate about the pure X-axis, it should exactly match a standard Roll
      axis = (/1.0_wp, 0.0_wp, 0.0_wp/)
      R_arb = get_arbitrary_rotation_matrix(45.0_wp, axis)
      R_std = get_single_rotation_matrix(45.0_wp, 'roll')
      !   print *, "   --- Arbitrary Rotation about X-axis ---"
      !   call print_matrix(R_arb)
      !   print *, "   --- Standard Roll Rotation (45 degrees) ---"
      !   call print_matrix(R_std)

      call assert_matrix_equal(R_arb, R_std, "Arbitrary rotation around X-axis matches Roll")

      ! Test with a non-unit vector to ensure the normalization logic works
      axis = (/0.0_wp, 5.0_wp, 0.0_wp/) ! Vector length is 5
      R_arb = get_arbitrary_rotation_matrix(30.0_wp, axis)
      R_std = get_single_rotation_matrix(30.0_wp, 'pitch')
      !   print *, "   --- Arbitrary Rotation about Non-Unit Y-axis ---"
      !   call print_matrix(R_arb)
      !   print *, "   --- Standard Pitch Rotation (30 degrees) ---"
      !   call print_matrix(R_std)

      call assert_matrix_equal(R_arb, R_std, "Arbitrary rotation handles non-unit vectors (Y-axis matches Pitch)")

   end subroutine test_arbitrary_rotation

   ! --- Helper: Assertion for Booleans ---
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

   ! --- Helper: Assertion for Matrices (Floating Point Safe) ---
   subroutine assert_matrix_equal(M1, M2, message)
      real(wp), intent(in) :: M1(3, 3), M2(3, 3)
      character(len=*), intent(in) :: message
      logical :: is_equal

      ! Check if all elements are within the tolerance
      is_equal = all(abs(M1 - M2) < TOL)

      if (.not. is_equal) then
         print *, "   [FAIL] ", message
         fail_count = fail_count + 1
      else
         print *, "   [PASS] ", message
      end if
   end subroutine assert_matrix_equal

end program test_euler
