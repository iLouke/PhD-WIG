module euler_rotations_mod
   use base_kinds_mod, only: wp
   implicit none
   private

   public :: build_rotation_matrix_aero
   public :: extract_euler_aero_from_matrix

contains

   pure function build_rotation_matrix_aero(alpha, beta, gamma) result(R)
      real(wp), intent(in) :: alpha, beta, gamma
      real(wp) :: R(3, 3)
      real(wp) :: ca, sa, cb, sb, cg, sg

      ca = cos(alpha); sa = sin(alpha)
      cb = cos(beta); sb = sin(beta)
      cg = cos(gamma); sg = sin(gamma)

      R(1, 1) = ca*cb
      R(1, 2) = sb
      R(1, 3) = sa*cb

      R(2, 1) = -(ca*sb*cg - sa*sg)
      R(2, 2) = cb*cg
      R(2, 3) = -(sa*sb*cg - ca*sg)

      R(3, 1) = ca*sb*sg - sa*cg
      R(3, 2) = -cb*sg
      R(3, 3) = sa*sb*sg + ca*cg
   end function build_rotation_matrix_aero

   pure subroutine extract_euler_aero_from_matrix(R, alpha, beta, gamma)
      real(wp), intent(in) :: R(3, 3)
      real(wp), intent(out) :: alpha, beta, gamma

      beta = asin(max(-1.0_wp, min(1.0_wp, R(1, 2))))

      if (abs(cos(beta)) > 1.0e-10_wp) then
         alpha = atan2(R(1, 3), R(1, 1))
         gamma = atan2(-R(3, 2), R(2, 2))
      else
         alpha = atan2(-R(3, 1), R(3, 3))
         gamma = 0.0_wp
      end if
   end subroutine extract_euler_aero_from_matrix

end module euler_rotations_mod
