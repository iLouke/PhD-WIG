module vortex_mod
   !! Biot-Savart Vortex Induction
   !!
   !! Computes the induced velocity from vortex ring elements (quadrilateral
   !! or triangular panels with unit-strength vortex filaments along edges).
   !!
   !! Legacy mapping:
   !!   SUBROUTINE VORTEX -> vortex_ring_velocity()
   !!   The legacy code computes velocity at point P due to a vortex ring
   !!   with 4 segments, each using the Biot-Savart law for a finite
   !!   straight vortex filament.
   !!
   !! Physics:
   !!   For a vortex segment from A to B with unit circulation Gamma=1,
   !!   the induced velocity at point P is:
   !!     V = (1/4*pi) * (cos(theta1) + cos(theta2)) / h * (AB x AP / |AB x AP|)
   !!   where:
   !!     h = perpendicular distance from P to line AB
   !!     theta1/2 = angles subtended at A/B
   !!
   !! A core radius (SMALL) is used to regularize the singularity when
   !! the evaluation point is on or near a vortex filament.
   use base_kinds_mod, only: wp
   use constants_mod, only: PI, SMALL
   use vector3d_mod, only: vector3d_t, vec3, zero_vec3, norm, &
                           operator(-), operator(+), operator(*), &
                           operator(.dot.), operator(.cross.)
   implicit none
   private

   public :: vortex_ring_velocity
   public :: vortex_segment_velocity

contains

   !> Compute induced velocity at point P from a single vortex ring panel
   !!
   !! The vortex ring has 4 straight segments connecting corners 1→2→3→4→1,
   !! each carrying unit circulation. For triangular panels, set p4 = p3
   !! (the segment 3→4 has zero length and contributes nothing).
   !!
   !! Legacy equivalent: SUBROUTINE VORTEX (via COMMON/VVOR/)
   !!
   !! @param eval_point  Point where velocity is evaluated (GX,GY,GZ in legacy)
   !! @param p1,p2,p3,p4 Corner vertices of the vortex ring panel
   !! @return            Induced velocity vector (unit circulation)
   pure function vortex_ring_velocity(eval_point, p1, p2, p3, p4) result(vel)
      type(vector3d_t), intent(in) :: eval_point
      type(vector3d_t), intent(in) :: p1, p2, p3, p4
      type(vector3d_t) :: vel

      type(vector3d_t) :: corners(4)
      type(vector3d_t) :: seg_vel
      integer :: kk, kik

      corners(1) = p1
      corners(2) = p2
      corners(3) = p3
      corners(4) = p4

      vel = zero_vec3()

      ! Sum contributions from 4 vortex segments: 1→2, 2→3, 3→4, 4→1
      ! (Legacy: DO 80 KK=1,4 ... KIK=KK+1; IF(KK.EQ.4) KIK=1)
      do kk = 1, 4
         kik = kk + 1
         if (kk == 4) kik = 1

         seg_vel = vortex_segment_velocity(eval_point, corners(kk), corners(kik))
         vel%x = vel%x + seg_vel%x
         vel%y = vel%y + seg_vel%y
         vel%z = vel%z + seg_vel%z
      end do

   end function vortex_ring_velocity

   !> Compute induced velocity at point P from a single straight vortex segment A→B
   !!
   !! Uses the Biot-Savart law for a finite vortex filament with unit circulation.
   !!
   !! Formula:
   !!   V = (Gamma / 4*pi) * (cos(theta_A) + cos(theta_B)) / h * e_perp
   !!
   !! where:
   !!   e_perp = (AP x BP) / |AP x BP|  (unit vector in perpendicular direction)
   !!   h      = |AP x BP| / |AB|       (perpendicular distance)
   !!   cos(theta_A) = (AB . AP) / (|AB| * |AP|)
   !!   cos(theta_B) = -(AB . BP) / (|AB| * |BP|)
   !!
   !! Combining:
   !!   V = (cos(theta_A) + cos(theta_B)) / (4*pi*h) * (AB x AP) / |AB x AP|
   !!
   !! Core radius regularization: if h < SMALL, the contribution is zero
   !! (point is on the vortex line — regularized to avoid singularity).
   !!
   !! @param P  Evaluation point
   !! @param A  Start of vortex segment
   !! @param B  End of vortex segment
   !! @return   Induced velocity (unit circulation Gamma=1)
   pure function vortex_segment_velocity(P, A, B) result(vel)
      type(vector3d_t), intent(in) :: P, A, B
      type(vector3d_t) :: vel

      type(vector3d_t) :: AB, AP, BP, cross_AB_AP
      real(wp) :: ab_len, ap_len, bp_len, h
      real(wp) :: cos_theta1, cos_theta2, vp_mag
      real(wp) :: cross_mag

      vel = zero_vec3()

      ! Segment vector and distances
      AB = B - A
      AP = P - A
      BP = P - B

      ab_len = norm(AB)
      ap_len = norm(AP)
      bp_len = norm(BP)

      ! Degenerate cases: zero-length segment or point at vertex
      if (ab_len <= SMALL) return
      if (ap_len <= SMALL) return
      if (bp_len <= SMALL) return

      ! Cosines of angles at A and B
      cos_theta1 = (AB.dot.AP)/(ab_len*ap_len)
      cos_theta2 = -(AB.dot.BP)/(ab_len*bp_len)

      ! Cross product AP x BP (perpendicular direction and distance)
      cross_AB_AP = AP.cross.BP
      cross_mag = norm(cross_AB_AP)

      ! Perpendicular distance: h = |AP x BP| / |AB|
      h = cross_mag/ab_len

      ! Regularization: point too close to filament
      if (h <= SMALL) return

      ! Velocity magnitude (unit circulation, Gamma = 1)
      vp_mag = (cos_theta1 + cos_theta2)/(4.0_wp*PI*h)

      ! Direction: (AB x AP) / |AB x AP|
      ! Note: legacy uses ABPX = ABY*APZ - ABZ*APY, which is AB x AP
      vel%x = vp_mag*cross_AB_AP%x/cross_mag
      vel%y = vp_mag*cross_AB_AP%y/cross_mag
      vel%z = vp_mag*cross_AB_AP%z/cross_mag

   end function vortex_segment_velocity

end module vortex_mod
