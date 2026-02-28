module penetration_mod
   !! Wake-Body Penetration Detection and Correction
   !!
   !! Detects when wake grid points penetrate through body panels and
   !! corrects their positions to prevent non-physical wake behavior.
   !!
   !! Legacy mapping:
   !!   SUBROUTINE PENETR  ->  correct_penetration()
   !!   SUBROUTINE WAKCOR  ->  correct_wake_penetrations()
   !!   SUBROUTINE CROSS   ->  detect_cross_intersection()
   !!   SUBROUTINE WAKINT  ->  handle_wake_intersections()
   !!
   !! Algorithm (PENETR equivalent):
   !!   For each wake point, check if it has crossed any body panel by:
   !!   1. Project the wake displacement vector onto the panel's local CS
   !!   2. Check if the projection crosses the panel plane (sign change in normal component)
   !!   3. If crossing, check if the crossing point is inside the panel boundary
   !!   4. If inside, project the wake point back to the panel surface
   !!
   !! The correction loop repeats until no more penetrations are found
   !! (a wake point pushed out of one panel might penetrate another).
   use base_kinds_mod, only: wp, ip
   use vector3d_mod, only: vector3d_t, vec3, zero_vec3, norm, normalize, &
                           operator(-), operator(+), operator(*), &
                           operator(.dot.), operator(.cross.)
   use grid_mod, only: mesh_t
   use wake_mod, only: wake_t
!$ use omp_lib
   implicit none
   private

   public :: correct_wake_penetrations
   public :: handle_wake_intersections

contains

   !> Correct all wake grid points for body panel penetration
   !!
   !! Legacy equivalent: SUBROUTINE WAKCOR(NPAN, NGW, ITER)
   !! Iterates backwards through time rows and for each wake grid point,
   !! calls the penetration correction.
   !!
   !! @param mesh   Body surface mesh
   !! @param wake   Wake model (positions are modified in-place)
   !! @param iter   Current iteration count
   subroutine correct_wake_penetrations(mesh, wake, iter)
      type(mesh_t), intent(in)    :: mesh
      type(wake_t), intent(inout) :: wake
      integer(ip), intent(in)     :: iter

      integer(ip) :: j, i, ii
      type(vector3d_t) :: pos_curr, pos_next

      ! For each wake grid point, correct time rows from newest to oldest
      do j = 1, wake%n_grid_pts
         do i = 1, iter
            ii = iter + 1 - i  ! Reverse order (newest first)

            pos_curr = wake%positions(ii, j)
            pos_next = wake%positions(ii + 1, j)

            call correct_single_point(mesh, pos_curr, pos_next, &
                                      wake%source_node_ids(j))

            wake%positions(ii, j) = pos_curr
         end do
      end do
   end subroutine correct_wake_penetrations

   !> Correct a single wake point if it penetrates any body panel
   !!
   !! Legacy equivalent: SUBROUTINE PENETR(NPAN, XW, YW, ZW, XW1, YW1, ZW1, NP1, NCC)
   !!
   !! Algorithm:
   !!   1. For each body panel K:
   !!      a. Skip panels that share the source node (NCC)
   !!      b. Project both current and reference positions into panel local CS
   !!      c. Check for sign change in normal component (= plane crossing)
   !!      d. Compute crossing point in tangent/longitudinal coordinates
   !!      e. Check if crossing point is inside panel boundary (winding test)
   !!      f. If penetrating, push the current point to the panel surface
   !!   2. Repeat until no penetrations found (iterative correction)
   subroutine correct_single_point(mesh, pos, pos_ref, source_node)
      type(mesh_t), intent(in)       :: mesh
      type(vector3d_t), intent(inout) :: pos      !! Point to correct (modified)
      type(vector3d_t), intent(in)    :: pos_ref  !! Reference position (previous row)
      integer(ip), intent(in)         :: source_node  !! Body node this wake point originated from

      integer(ip) :: k, ik, kki, l, ll
      type(vector3d_t) :: pg, pg1, centroid, n_vec, t_vec, l_vec
      real(wp) :: wn, wn1, wt, wt1, wl, wl1, xc, yc
      real(wp) :: dist_val, x1_val, y1_val, z1_val
      type(vector3d_t) :: node_pos
      real(wp) :: px(4), py(4), pv(4)
      real(wp) :: xs, ys, xs1, ys1
      logical :: penetrating, inside
      integer(ip) :: n_corrections

      n_corrections = 1  ! Enter correction loop at least once

      do while (n_corrections > 0)
         n_corrections = 0

         do k = 1, mesh%n_panels
            ! Skip panels sharing the source node
            kki = mesh%panels(k)%n_nodes
            inside = .false.
            do ik = 1, kki
               if (mesh%panels(k)%node_ids(ik) == source_node) then
                  inside = .true.
                  exit
               end if
            end do
            if (inside) cycle

            ! Panel geometric data
            centroid = mesh%panels(k)%centroid
            n_vec = mesh%panels(k)%normal
            t_vec = mesh%panels(k)%tangent
            l_vec = mesh%panels(k)%longitudinal

            ! Project current position into panel local CS
            pg = pos - centroid
            wt = pg.dot.t_vec   ! Tangential
            wl = pg.dot.l_vec   ! Longitudinal
            wn = pg.dot.n_vec   ! Normal

            ! Project reference position
            pg1 = pos_ref - centroid
            wt1 = pg1.dot.t_vec
            wl1 = pg1.dot.l_vec
            wn1 = pg1.dot.n_vec

            ! Check for plane crossing: sign change in normal component
            if ((wn1*wn) > 0.0_wp .or. wn1 == wn) cycle

            ! Compute crossing point in tangent-longitudinal plane
            xc = (wn1*wt - wn*wt1)/(wn1 - wn)
            yc = (wn*wl1 - wn1*wl)/(wn - wn1)

            ! Project panel vertices into local 2D coordinates
            do l = 1, kki
               node_pos = mesh%nodes(mesh%panels(k)%node_ids(l)) - centroid
               px(l) = node_pos.dot.t_vec
               py(l) = node_pos.dot.l_vec
            end do

            ! Point-in-polygon test (winding number / cross product method)
            ! Legacy: check all edge cross products have the same sign
            do l = 1, kki
               ll = l + 1
               if (l == kki) ll = 1
               xs = px(ll) - px(l)
               ys = py(ll) - py(l)
               xs1 = px(l) - xc
               ys1 = py(l) - yc
               pv(l) = xs*ys1 - ys*xs1
            end do

            ! Check if all cross products have same sign (point is inside)
            inside = .true.
            do l = 1, kki
               ll = l + 1
               if (l == kki) ll = 1
               if (pv(l)*pv(ll) < 0.0_wp) then
                  inside = .false.
                  exit
               end if
            end do

            if (.not. inside) cycle

            ! ── Penetration detected: correct the position ──────────────
            ! Push the point back to the panel surface
            x1_val = pos%x - pos_ref%x
            y1_val = pos%y - pos_ref%y
            z1_val = pos%z - pos_ref%z
            dist_val = -(x1_val*n_vec%x + y1_val*n_vec%y + z1_val*n_vec%z)

            pos%x = pos%x + dist_val*n_vec%x
            pos%y = pos%y + dist_val*n_vec%y
            pos%z = pos%z + dist_val*n_vec%z

            n_corrections = n_corrections + 1
            exit  ! Re-check from the beginning after correction
         end do
      end do

   end subroutine correct_single_point

   !> Handle wake-wake intersections (propeller/wing wake interactions)
   !!
   !! Legacy equivalent: SUBROUTINE WAKINT(NPW, ITER) + SUBROUTINE CROSS
   !!
   !! Detects when propeller wake segments cross through wing wake panels
   !! and corrects the positions to prevent tangling.
   !!
   !! @param wake   Wake model (positions are modified in-place)
   !! @param iter   Current iteration
   subroutine handle_wake_intersections(wake, iter)
      type(wake_t), intent(inout) :: wake
      integer(ip), intent(in)     :: iter

      integer(ip) :: ii, ik, ij1, i_pan, j_pan
      type(vector3d_t) :: x1, x2, x3, x4
      type(vector3d_t) :: xx1, xx2, xx3, xx4
      type(vector3d_t) :: new_pos1, new_pos2

      if (wake%n_panels < 2) return

      ! For each time row (reverse order), check propeller wake vs wing wake
      do ii = 1, iter
         ik = iter + 1 - ii

         do i_pan = 1, wake%n_panels
            if (wake%panel_props(i_pan) /= 0) cycle  ! Skip non-wing panels

            ! Wing wake panel corners at time row ik
            call wake%get_panel_corners(ik, i_pan, x1, x2, x3, x4)

            ! Check against propeller wake panels
            do ij1 = 1, iter
               do j_pan = 1, wake%n_panels
                  if (wake%panel_props(j_pan) /= 1) cycle  ! Only propeller wake

                  ! Propeller wake grid nodes
                  call wake%get_panel_corners(iter + 1 - ij1, j_pan, xx1, xx2, xx3, xx4)

                  ! Check if segment xx3→xx2 crosses the wing wake panel
                  call detect_cross_intersection(xx3, xx2, x1, x2, x3, x4, new_pos2)

                  ! Check if segment xx4→xx1 crosses the wing wake panel
                  call detect_cross_intersection(xx4, xx1, x1, x2, x3, x4, new_pos1)

                  ! Update positions if intersection was detected
                  ! (new_pos will equal the input if no intersection)
                  wake%positions(iter + 1 - ij1, wake%panel_conn(j_pan, 1)) = new_pos1
                  wake%positions(iter + 1 - ij1, wake%panel_conn(j_pan, 2)) = new_pos2
               end do
            end do
         end do
      end do

   end subroutine handle_wake_intersections

   !> Detect and correct a line segment crossing through a quadrilateral
   !!
   !! Legacy equivalent: SUBROUTINE CROSS(XXA, YYA, ZZA, XXB, YYB, ZZB,
   !!                    X1,Y1,Z1, X2,Y2,Z2, X3,Y3,Z3, X4,Y4,Z4, ...)
   !!
   !! Tests if segment A→B crosses the quadrilateral (p1,p2,p3,p4).
   !! If so, projects B onto the plane of the quad to correct it.
   !!
   !! @param A,B          Line segment endpoints (B is the point to correct)
   !! @param p1,p2,p3,p4  Quadrilateral corners
   !! @param new_B        Corrected position of B (= B if no intersection)
   subroutine detect_cross_intersection(A, B, p1, p2, p3, p4, new_B)
      type(vector3d_t), intent(in)  :: A, B
      type(vector3d_t), intent(in)  :: p1, p2, p3, p4
      type(vector3d_t), intent(out) :: new_B

      type(vector3d_t) :: AB, xi(4), da1, da2, cross12
      real(wp) :: pr(4), pra, prb, pr13, pr24
      type(vector3d_t) :: d1, d2, panel_normal
      real(wp) :: wnorm_val, wnx, wny, wnz, abpr
      integer :: i, ii_next

      new_B = B  ! Default: no correction

      AB = B - A
      if (norm(AB) < 1.0e-15_wp) return

      xi(1) = p1; xi(2) = p2; xi(3) = p3; xi(4) = p4

      ! Compute signed volumes for each triangle face
      do i = 1, 4
         ii_next = i + 1
         if (i == 4) ii_next = 1

         da1 = xi(i) - A
         da2 = xi(ii_next) - A
         cross12 = da1.cross.da2
         pr(i) = AB.dot.cross12
         if (i == 3 .or. i == 4) pr(i) = -pr(i)
      end do

      ! Check if segment endpoints are on same side of panel plane
      pra = AB.dot. (p1 - A)
      prb = AB.dot. (p1 - B)
      if (pra*prb > 0.0_wp) return  ! Same side, no crossing

      ! Check if crossing point is inside the quadrilateral
      pr13 = pr(1)*pr(3)
      pr24 = pr(2)*pr(4)
      if (pr13 > 0.0_wp .or. pr24 > 0.0_wp) return  ! Outside

      ! Intersection detected — project B onto the panel plane
      d1 = p3 - p1
      d2 = p2 - p4
      panel_normal = d1.cross.d2
      wnorm_val = norm(panel_normal)

      if (wnorm_val < 1.0e-15_wp) return

      wnx = panel_normal%x/wnorm_val
      wny = panel_normal%y/wnorm_val
      wnz = panel_normal%z/wnorm_val

      abpr = wnx*AB%x + wny*AB%y + wnz*AB%z
      new_B%x = B%x - wnx*abpr
      new_B%y = B%y - wny*abpr
      new_B%z = B%z - wnz*abpr

   end subroutine detect_cross_intersection

end module penetration_mod
