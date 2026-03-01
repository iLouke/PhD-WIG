module euler_rotations_mod
   use base_kinds_mod, only: wp
   use constants_mod, only: deg2rad
   use logger_mod, only: global_logger, LOG_ERROR
   implicit none
   private

   public :: get_rotation_matrix, get_single_rotation_matrix

contains

   pure function get_single_rotation_matrix(angle, rotation, rads) result(R)
      real(wp), intent(in) :: angle
      character(len=*), intent(in) :: rotation
      logical, intent(in), optional :: rads
      real(wp) :: R(3, 3)
      real(wp) :: c, s

      if (present(rads) .and. rads) then
         ! Angle is already in radians
         c = cos(angle)
         s = sin(angle)
      else
         ! Convert angle from degrees to radians
         c = cos(angle*deg2rad)
         s = sin(angle*deg2rad)
      end if

      select case (rotation)
      case ('alpha', 'Alpha', 'ALPHA', 'z', 'Z', 'yaw', 'Yaw', 'YAW')
         R = reshape((/1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, c, -s, 0.0_wp, s, c/), shape(R))
      case ('beta', 'Beta', 'BETA', 'y', 'Y', 'pitch', 'Pitch', 'PITCH')
         R = reshape((/c, 0.0_wp, s, 0.0_wp, 1.0_wp, 0.0_wp, -s, 0.0_wp, c/), shape(R))
      case ('gamma', 'Gamma', 'GAMMA', 'x', 'X', 'roll', 'Roll', 'ROLL')
         R = reshape((/c, -s, 0.0_wp, s, c, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp/), shape(R))
      case default
         R = reshape((/1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp/), shape(R))
      end select
   end function get_single_rotation_matrix

   pure function get_rotation_matrix(alpha, beta, gamma, rads) result(R)
      real(wp), intent(in) :: alpha, beta, gamma
      logical, intent(in), optional :: rads
      real(wp), dimension(3, 3) :: R, Rx, Ry, Rz

      Rx = get_single_rotation_matrix(gamma, 'gamma', rads)
      Ry = get_single_rotation_matrix(beta, 'beta', rads)
      Rz = get_single_rotation_matrix(alpha, 'alpha', rads)
      R = matmul(Rz, matmul(Ry, Rx))

   end function get_rotation_matrix
end module euler_rotations_mod
