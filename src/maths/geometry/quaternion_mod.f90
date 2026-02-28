module quaternion_mod
   !! Quaternion Mathematics for 3D Rotations
   !!
   !! Replaces the Euler angle rotation in the legacy GEOM subroutine with
   !! a singularity-free quaternion representation. Quaternions avoid gimbal
   !! lock and provide smooth interpolation (SLERP) for dynamic simulations.
   !!
   !! Legacy mapping:
   !!   The legacy code applied Euler rotations (alpha, beta, gamma) via a
   !!   direct rotation matrix:
   !!     XX = X*cosA*cosB + Y*sinB + Z*sinA*cosB
   !!     YY = -X*(cosA*sinB*cosG - sinA*sinG) + Y*cosB*cosG
   !!          - Z*(sinA*sinB*cosG - cosA*sinG)
   !!     ZZ = X*(cosA*sinB*sinG - sinA*cosG) - Y*cosB*sinG
   !!          + Z*(sinA*sinB*sinG + cosA*cosG)
   !!   This is now handled by: q = from_euler_aero(alpha, beta, gamma)
   !!                           rotated_point = q%rotate(point)
   !!
   !! Quaternion convention: q = w + x*i + y*j + z*k  (scalar-first)
   !! Rotation: v' = q * v * q^(-1)  (active rotation of vector v)
   use base_kinds_mod, only: wp
   use vector3d_mod, only: vector3d_t, vec3, norm
   use euler_rotations_mod, only: build_rotation_matrix_aero, extract_euler_aero_from_matrix
   implicit none
   private

   public :: quaternion_t, quat_identity
   public :: from_axis_angle, from_euler_aero, from_rotation_matrix
   public :: operator(*), slerp

   ! ─── Type Definition ─────────────────────────────────────────────────
   type :: quaternion_t
      real(wp) :: w = 1.0_wp   !! Scalar part
      real(wp) :: x = 0.0_wp   !! i-component
      real(wp) :: y = 0.0_wp   !! j-component
      real(wp) :: z = 0.0_wp   !! k-component
   contains
      procedure :: conjugate => quat_conjugate
      procedure :: qnorm => quat_norm
      procedure :: normalize => quat_normalize
      procedure :: inverse => quat_inverse
      procedure :: rotate => quat_rotate_vector
      procedure :: to_rotation_matrix => quat_to_matrix
      procedure :: to_euler_aero => quat_to_euler
   end type quaternion_t

   ! ─── Operator Interfaces ─────────────────────────────────────────────
   interface operator(*)
      module procedure quat_multiply
   end interface

contains

   ! ═══════════════════════════════════════════════════════════════════════
   !                         CONSTRUCTORS
   ! ═══════════════════════════════════════════════════════════════════════

   !> Identity quaternion (no rotation)
   pure function quat_identity() result(q)
      type(quaternion_t) :: q
      q%w = 1.0_wp; q%x = 0.0_wp; q%y = 0.0_wp; q%z = 0.0_wp
   end function quat_identity

   !> Create quaternion from axis-angle representation
   !! @param axis  Unit rotation axis (will be normalized internally)
   !! @param angle Rotation angle in radians (right-hand rule)
   pure function from_axis_angle(axis, angle) result(q)
      type(vector3d_t), intent(in) :: axis
      real(wp), intent(in)         :: angle
      type(quaternion_t) :: q
      real(wp) :: half_angle, s, ax_norm
      type(vector3d_t) :: ax

      half_angle = 0.5_wp*angle
      ax_norm = norm(axis)
      if (ax_norm > 0.0_wp) then
         ax = vec3(axis%x/ax_norm, axis%y/ax_norm, axis%z/ax_norm)
      else
         ax = vec3(0.0_wp, 0.0_wp, 1.0_wp)  ! Default to Z-axis
      end if
      s = sin(half_angle)
      q%w = cos(half_angle)
      q%x = ax%x*s
      q%y = ax%y*s
      q%z = ax%z*s
   end function from_axis_angle

   !> Create quaternion from aerospace Euler angles (alpha, beta, gamma)
   !!
   !! Reproduces the EXACT rotation from the legacy GEOM subroutine.
   !! The legacy rotation matrix R is constructed from:
   !!   alpha (angle of attack)  : rotation about Y-axis
   !!   beta  (sideslip angle)   : body-frame Z-axis rotation (negated convention)
   !!   gamma (bank/roll angle)  : body-frame X-axis rotation (negated convention)
   !!
   !! The equivalent quaternion is: q = q_gamma * q_beta * q_alpha
   !! where each sub-quaternion uses the aerospace sign convention.
   !!
   !! @param alpha Angle of attack [rad]
   !! @param beta  Sideslip angle [rad]
   !! @param gamma Bank/roll angle [rad]
   pure function from_euler_aero(alpha, beta, gamma) result(q)
      real(wp), intent(in) :: alpha, beta, gamma
      type(quaternion_t) :: q
      real(wp) :: R(3, 3)

      R = build_rotation_matrix_aero(alpha, beta, gamma)

      ! Convert rotation matrix to quaternion using Shepperd's method
      ! (numerically stable for all orientations)
      q = from_rotation_matrix(R)
   end function from_euler_aero

   !> Create quaternion from a 3x3 rotation matrix
   !! Uses Shepperd's method for numerical stability.
   !!
   !! @param R  3x3 orthogonal rotation matrix (det = +1)
   pure function from_rotation_matrix(R) result(q)
      real(wp), intent(in) :: R(3, 3)
      type(quaternion_t) :: q
      real(wp) :: tr, s_val, max_diag
      integer :: i_max

      tr = R(1, 1) + R(2, 2) + R(3, 3)

      ! Find the largest component for numerical stability
      max_diag = tr  ! corresponds to w
      i_max = 0
      if (R(1, 1) > max_diag) then; max_diag = R(1, 1); i_max = 1; end if
      if (R(2, 2) > max_diag) then; max_diag = R(2, 2); i_max = 2; end if
      if (R(3, 3) > max_diag) then; max_diag = R(3, 3); i_max = 3; end if

      select case (i_max)
      case (0)  ! w is largest
         s_val = 2.0_wp*sqrt(1.0_wp + tr)
         q%w = 0.25_wp*s_val
         q%x = (R(3, 2) - R(2, 3))/s_val
         q%y = (R(1, 3) - R(3, 1))/s_val
         q%z = (R(2, 1) - R(1, 2))/s_val
      case (1)  ! x is largest
         s_val = 2.0_wp*sqrt(1.0_wp + R(1, 1) - R(2, 2) - R(3, 3))
         q%w = (R(3, 2) - R(2, 3))/s_val
         q%x = 0.25_wp*s_val
         q%y = (R(1, 2) + R(2, 1))/s_val
         q%z = (R(1, 3) + R(3, 1))/s_val
      case (2)  ! y is largest
         s_val = 2.0_wp*sqrt(1.0_wp - R(1, 1) + R(2, 2) - R(3, 3))
         q%w = (R(1, 3) - R(3, 1))/s_val
         q%x = (R(1, 2) + R(2, 1))/s_val
         q%y = 0.25_wp*s_val
         q%z = (R(2, 3) + R(3, 2))/s_val
      case (3)  ! z is largest
         s_val = 2.0_wp*sqrt(1.0_wp - R(1, 1) - R(2, 2) + R(3, 3))
         q%w = (R(2, 1) - R(1, 2))/s_val
         q%x = (R(1, 3) + R(3, 1))/s_val
         q%y = (R(2, 3) + R(3, 2))/s_val
         q%z = 0.25_wp*s_val
      end select

      ! Ensure canonical form (w >= 0)
      if (q%w < 0.0_wp) then
         q%w = -q%w; q%x = -q%x; q%y = -q%y; q%z = -q%z
      end if
   end function from_rotation_matrix

   ! ═══════════════════════════════════════════════════════════════════════
   !                        QUATERNION ALGEBRA
   ! ═══════════════════════════════════════════════════════════════════════

   !> Hamilton product: q1 * q2
   !! Non-commutative: q1*q2 /= q2*q1 in general.
   !! Composition: rotating first by q2 then by q1 is q1*q2.
   pure function quat_multiply(q1, q2) result(q)
      type(quaternion_t), intent(in) :: q1, q2
      type(quaternion_t) :: q
      q%w = q1%w*q2%w - q1%x*q2%x - q1%y*q2%y - q1%z*q2%z
      q%x = q1%w*q2%x + q1%x*q2%w + q1%y*q2%z - q1%z*q2%y
      q%y = q1%w*q2%y - q1%x*q2%z + q1%y*q2%w + q1%z*q2%x
      q%z = q1%w*q2%z + q1%x*q2%y - q1%y*q2%x + q1%z*q2%w
   end function quat_multiply

   !> Quaternion conjugate: q* = (w, -x, -y, -z)
   pure function quat_conjugate(this) result(qc)
      class(quaternion_t), intent(in) :: this
      type(quaternion_t) :: qc
      qc%w = this%w
      qc%x = -this%x
      qc%y = -this%y
      qc%z = -this%z
   end function quat_conjugate

   !> Quaternion norm: |q| = sqrt(w² + x² + y² + z²)
   pure function quat_norm(this) result(n)
      class(quaternion_t), intent(in) :: this
      real(wp) :: n
      n = sqrt(this%w**2 + this%x**2 + this%y**2 + this%z**2)
   end function quat_norm

   !> Normalize to unit quaternion
   pure function quat_normalize(this) result(qn)
      class(quaternion_t), intent(in) :: this
      type(quaternion_t) :: qn
      real(wp) :: n
      n = this%qnorm()
      if (n > 0.0_wp) then
         qn%w = this%w/n; qn%x = this%x/n
         qn%y = this%y/n; qn%z = this%z/n
      else
         qn = quat_identity()
      end if
   end function quat_normalize

   !> Quaternion inverse: q^(-1) = q* / |q|²
   !! For unit quaternions: q^(-1) = q*
   pure function quat_inverse(this) result(qi)
      class(quaternion_t), intent(in) :: this
      type(quaternion_t) :: qi
      real(wp) :: n2
      n2 = this%w**2 + this%x**2 + this%y**2 + this%z**2
      if (n2 > 0.0_wp) then
         qi%w = this%w/n2
         qi%x = -this%x/n2
         qi%y = -this%y/n2
         qi%z = -this%z/n2
      else
         qi = quat_identity()
      end if
   end function quat_inverse

   ! ═══════════════════════════════════════════════════════════════════════
   !                     ROTATION OPERATIONS
   ! ═══════════════════════════════════════════════════════════════════════

   !> Rotate a 3D vector by this quaternion: v' = q * v * q^(-1)
   !!
   !! Legacy equivalent: The coordinate transformation in GEOM that
   !! rotated grid points by (alpha, beta, gamma).
   !!
   !! Uses the optimized formula (avoids full quaternion multiplication):
   !!   t = 2 * (q_vec x v)
   !!   v' = v + w*t + q_vec x t
   pure function quat_rotate_vector(this, v) result(vr)
      class(quaternion_t), intent(in) :: this
      type(vector3d_t), intent(in)    :: v
      type(vector3d_t) :: vr
      real(wp) :: tx, ty, tz  ! t = 2 * (q_vec x v)
      real(wp) :: ux, uy, uz  ! q_vec x t

      ! t = 2 * (q_vec x v)
      tx = 2.0_wp*(this%y*v%z - this%z*v%y)
      ty = 2.0_wp*(this%z*v%x - this%x*v%z)
      tz = 2.0_wp*(this%x*v%y - this%y*v%x)

      ! u = q_vec x t
      ux = this%y*tz - this%z*ty
      uy = this%z*tx - this%x*tz
      uz = this%x*ty - this%y*tx

      ! v' = v + w*t + u
      vr%x = v%x + this%w*tx + ux
      vr%y = v%y + this%w*ty + uy
      vr%z = v%z + this%w*tz + uz
   end function quat_rotate_vector

   !> Convert unit quaternion to 3x3 rotation matrix
   !! Output: R(3,3) where v' = R * v
   pure function quat_to_matrix(this) result(R)
      class(quaternion_t), intent(in) :: this
      real(wp) :: R(3, 3)
      real(wp) :: ww, xx, yy, zz, wx, wy, wz, xy, xz, yz

      ww = this%w*this%w
      xx = this%x*this%x
      yy = this%y*this%y
      zz = this%z*this%z
      wx = this%w*this%x
      wy = this%w*this%y
      wz = this%w*this%z
      xy = this%x*this%y
      xz = this%x*this%z
      yz = this%y*this%z

      R(1, 1) = ww + xx - yy - zz
      R(1, 2) = 2.0_wp*(xy - wz)
      R(1, 3) = 2.0_wp*(xz + wy)
      R(2, 1) = 2.0_wp*(xy + wz)
      R(2, 2) = ww - xx + yy - zz
      R(2, 3) = 2.0_wp*(yz - wx)
      R(3, 1) = 2.0_wp*(xz - wy)
      R(3, 2) = 2.0_wp*(yz + wx)
      R(3, 3) = ww - xx - yy + zz
   end function quat_to_matrix

   !> Convert unit quaternion to aerospace Euler angles (alpha, beta, gamma)
   !! Inverse of from_euler_aero. Returns angles in radians.
   !!
   !! Warning: This extracts Euler angles from the legacy-convention matrix
   !! and may have singularities near beta = +/- 90° (gimbal lock).
   pure subroutine quat_to_euler(this, alpha, beta, gamma)
      class(quaternion_t), intent(in) :: this
      real(wp), intent(out) :: alpha, beta, gamma
      real(wp) :: R(3, 3)

      R = this%to_rotation_matrix()

      call extract_euler_aero_from_matrix(R, alpha, beta, gamma)
   end subroutine quat_to_euler

   ! ═══════════════════════════════════════════════════════════════════════
   !                     INTERPOLATION (SLERP)
   ! ═══════════════════════════════════════════════════════════════════════

   !> Spherical Linear Interpolation between two quaternions
   !! @param q1  Start quaternion (t=0)
   !! @param q2  End quaternion (t=1)
   !! @param t   Interpolation parameter [0, 1]
   pure function slerp(q1, q2, t) result(q)
      type(quaternion_t), intent(in) :: q1, q2
      real(wp), intent(in) :: t
      type(quaternion_t) :: q, q2_adj
      real(wp) :: dot_val, theta, sin_theta, s1, s2

      ! Compute dot product
      dot_val = q1%w*q2%w + q1%x*q2%x + q1%y*q2%y + q1%z*q2%z

      ! If dot < 0, negate q2 to take shorter path
      q2_adj = q2
      if (dot_val < 0.0_wp) then
         q2_adj%w = -q2%w; q2_adj%x = -q2%x
         q2_adj%y = -q2%y; q2_adj%z = -q2%z
         dot_val = -dot_val
      end if

      ! Clamp for numerical safety
      dot_val = min(dot_val, 1.0_wp)

      if (dot_val > 0.9995_wp) then
         ! Near-parallel: use linear interpolation
         q%w = q1%w + t*(q2_adj%w - q1%w)
         q%x = q1%x + t*(q2_adj%x - q1%x)
         q%y = q1%y + t*(q2_adj%y - q1%y)
         q%z = q1%z + t*(q2_adj%z - q1%z)
         q = q%normalize()
      else
         theta = acos(dot_val)
         sin_theta = sin(theta)
         s1 = sin((1.0_wp - t)*theta)/sin_theta
         s2 = sin(t*theta)/sin_theta
         q%w = s1*q1%w + s2*q2_adj%w
         q%x = s1*q1%x + s2*q2_adj%x
         q%y = s1*q1%y + s2*q2_adj%y
         q%z = s1*q1%z + s2*q2_adj%z
      end if
   end function slerp

end module quaternion_mod
