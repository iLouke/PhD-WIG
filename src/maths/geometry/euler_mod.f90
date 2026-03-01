module euler_mod
   use base_kinds_mod, only: wp
   use constants_mod, only: deg2rad
   use logger_mod, only: global_logger, LOG_ERROR
   implicit none
   private

   public :: get_rotation_matrix, get_single_rotation_matrix, get_arbitrary_rotation_matrix

contains

   pure function get_single_rotation_matrix(angle, rotation, rads) result(R)
      real(wp), intent(in) :: angle
      character(len=*), intent(in) :: rotation
      logical, intent(in), optional :: rads
      real(wp) :: R(3, 3)
      real(wp) :: c, s
      real(wp), parameter :: one = 1.0_wp, zero = 0.0_wp
      logical :: use_rads

      ! Safely check if rads was provided
      use_rads = .false.
      if (present(rads)) use_rads = rads

      if (use_rads) then
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
         ! Corrected Active Z-Axis Rotation
         R = reshape((/c, s, zero, -s, c, zero, zero, zero, one/), shape(R))

      case ('beta', 'Beta', 'BETA', 'y', 'Y', 'pitch', 'Pitch', 'PITCH')
         ! Corrected Active Y-Axis Rotation
         R = reshape((/c, zero, -s, zero, one, zero, s, zero, c/), shape(R))

      case ('gamma', 'Gamma', 'GAMMA', 'x', 'X', 'roll', 'Roll', 'ROLL')
         ! Corrected Active X-Axis Rotation
         R = reshape((/one, zero, zero, zero, c, s, zero, -s, c/), shape(R))

      case default
         R = reshape((/1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp/), shape(R))
      end select
   end function get_single_rotation_matrix

   pure function get_arbitrary_rotation_matrix(angle, axis, rads) result(R)
      real(wp), intent(in) :: angle
      real(wp), intent(in) :: axis(3)
      logical, intent(in), optional :: rads
      real(wp) :: R(3, 3)
      real(wp) :: c, s, t, norm
      real(wp) :: ux, uy, uz
      real(wp) :: actual_angle
      logical :: use_rads

      use_rads = .false.
      if (present(rads)) use_rads = rads

      if (use_rads) then
         actual_angle = angle
      else
         actual_angle = angle*deg2rad
      end if

      c = cos(actual_angle)
      s = sin(actual_angle)
      t = 1.0_wp - c

      norm = norm2(axis)
      if (norm > 0.0_wp) then
         ux = axis(1)/norm
         uy = axis(2)/norm
         uz = axis(3)/norm
      else
         R = reshape((/1.0_wp, 0.0_wp, 0.0_wp, &
                       0.0_wp, 1.0_wp, 0.0_wp, &
                       0.0_wp, 0.0_wp, 1.0_wp/), shape(R))
         return
      end if

      ! Rodrigues' rotation formula
      R = reshape((/t*ux*ux + c, t*ux*uy + s*uz, t*ux*uz - s*uy, &
                    t*ux*uy - s*uz, t*uy*uy + c, t*uy*uz + s*ux, &
                    t*ux*uz + s*uy, t*uy*uz - s*ux, t*uz*uz + c/), shape(R))

   end function get_arbitrary_rotation_matrix

   pure function get_rotation_matrix(roll, pitch, yaw, rads) result(R)
      real(wp), intent(in) :: roll, pitch, yaw
      logical, intent(in), optional :: rads
      real(wp), dimension(3, 3) :: R, Rx, Ry, Rz

      Rx = get_single_rotation_matrix(roll, 'roll', rads)
      Ry = get_single_rotation_matrix(pitch, 'pitch', rads)
      Rz = get_single_rotation_matrix(yaw, 'yaw', rads)
      R = matmul(Rz, matmul(Ry, Rx))

   end function get_rotation_matrix
end module euler_mod
