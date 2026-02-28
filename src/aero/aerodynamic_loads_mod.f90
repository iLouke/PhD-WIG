module aerodynamic_loads_mod
   !! Aerodynamic Force and Moment Computation
   !!
   !! Computes lift, drag, and side force using the Kutta-Joukowski theorem
   !! applied to the vortex lattice panel edges. Also computes pressure
   !! coefficients for specific panel types (e.g., propeller blades).
   !!
   !! Legacy mapping:
   !!   SUBROUTINE AIRLOAD1 -> compute_forces()
   !!   SUBROUTINE CPAIP    -> compute_pressure_coefficients()
   !!
   !! Physics (Kutta-Joukowski for panel methods):
   !!   For each shared edge between adjacent panels with different
   !!   circulation strengths, the vortex line carries a net circulation
   !!   delta_gamma = gamma_i - gamma_j. The force on this segment is:
   !!     dF = rho * delta_gamma * (V_local x dl)
   !!   where dl is the edge vector and V_local includes wake-induced velocity.
   !!
   !!   Lift  = force component in Y (after sign adjustments)
   !!   Drag  = force component in X (including wake-induced drag)
   !!   Side  = force component in Z
   !!
   !! OpenMP: The outer loop over panels is parallelized with reductions
   !! for the force accumulators.
   use base_kinds_mod, only: wp, ip
   use vector3d_mod, only: vector3d_t, vec3, zero_vec3, norm, &
                           operator(-), operator(+), operator(*), &
                           operator(.dot.), operator(.cross.)
   use grid_mod, only: mesh_t
   use wake_mod, only: wake_t
   use vortex_mod, only: vortex_ring_velocity
!$ use omp_lib
   implicit none
   private

   public :: compute_forces
   public :: compute_pressure_coefficients
   public :: aero_forces_t

   ! ─── Result Type ──────────────────────────────────────────────────────
   type :: aero_forces_t
      real(wp) :: lift = 0.0_wp    !! Total lift force [N]
      real(wp) :: drag = 0.0_wp    !! Total drag force [N]
      real(wp) :: side = 0.0_wp    !! Total side force [N]
      real(wp) :: cl = 0.0_wp    !! Lift coefficient (if reference area known)
      real(wp) :: cd = 0.0_wp    !! Drag coefficient
      real(wp) :: cy = 0.0_wp    !! Side force coefficient
   end type aero_forces_t

contains

   !> Compute aerodynamic forces on the body
   !!
   !! Legacy equivalent: SUBROUTINE AIRLOAD1(NPAN, VINIT, RHO, ALIFT, DRAG, SIDE, ...)
   !!
   !! Algorithm:
   !!   For each pair of adjacent panels (i, j) sharing a common edge:
   !!     1. Find the two shared node indices
   !!     2. If both shared nodes are trailing edge (MARK/=0), skip
   !!     3. Compute edge vector dl = node2 - node1 (with orientation adjustment)
   !!     4. delta_gamma = gamma_i - gamma_j
   !!     5. Compute wake-induced velocity at panel i centroid
   !!     6. Accumulate forces:
   !!        - Lift += rho * delta_gamma * Vinit * dl_y / 2
   !!        - Side += rho * delta_gamma * Vinit * dl_z / 2
   !!        - Drag += rho * delta_gamma * (V_wake_x * dl_y - V_wake_y * dl_x)
   !!
   !!   For symmetric configurations (nsym=1), the force is doubled by
   !!   repeating with mirrored Y coordinates.
   !!
   !! @param mesh    Surface mesh
   !! @param wake    Wake model
   !! @param gamma   Vortex strengths on body panels
   !! @param vinit   Freestream velocity [m/s]
   !! @param rho     Air density [kg/m³]
   !! @param iter    Current iteration
   !! @param nsym    Symmetry flag
   !! @param ngrnd   Ground effect flag
   !! @return        Aerodynamic forces structure
   function compute_forces(mesh, wake, gamma, vinit, rho, iter, nsym, ngrnd) &
      result(forces)
      type(mesh_t), intent(in) :: mesh
      type(wake_t), intent(in) :: wake
      real(wp), intent(in)     :: gamma(:)
      real(wp), intent(in)     :: vinit, rho
      integer(ip), intent(in)  :: iter, nsym, ngrnd
      type(aero_forces_t) :: forces

      integer(ip) :: i, j, ik, jk, kki, kkj, kp, kn_flag
      integer(ip) :: ip_shared(2)
      real(wp) :: dx, dy, dz, roc, rc
      type(vector3d_t) :: v_wake
      real(wp) :: lift_acc, drag_acc, side_acc

      lift_acc = 0.0_wp
      drag_acc = 0.0_wp
      side_acc = 0.0_wp

      ! Main loop: iterate over kn_flag = 0 (direct) and 1 (Y-mirror)
      do kn_flag = 0, nsym

         ! Loop over all panel pairs
         do i = 1, mesh%n_panels
            kki = mesh%panels(i)%n_nodes

            do j = 1, mesh%n_panels
               if (j == i) cycle
               kkj = mesh%panels(j)%n_nodes

               ! Find shared edge (two common nodes)
               kp = 0
               ip_shared = 0
               outer: do ik = 1, kki
                  do jk = 1, kkj
                     if (mesh%panels(i)%node_ids(ik) == &
                         mesh%panels(j)%node_ids(jk)) then
                        kp = kp + 1
                        ip_shared(kp) = ik
                        if (kp == 2) exit outer
                        exit  ! Move to next ik
                     end if
                  end do
               end do outer

               if (kp /= 2) cycle  ! Not adjacent

               ! Skip if both shared nodes are trailing edge
               if (mesh%boundary_marks(mesh%panels(i)%node_ids(ip_shared(1))) /= 0 .and. &
                   mesh%boundary_marks(mesh%panels(i)%node_ids(ip_shared(2))) /= 0) cycle

               ! Edge vector: node2 - node1
               dx = mesh%nodes(mesh%panels(i)%node_ids(ip_shared(2)))%x - &
                    mesh%nodes(mesh%panels(i)%node_ids(ip_shared(1)))%x
               dy = mesh%nodes(mesh%panels(i)%node_ids(ip_shared(2)))%y - &
                    mesh%nodes(mesh%panels(i)%node_ids(ip_shared(1)))%y
               dz = mesh%nodes(mesh%panels(i)%node_ids(ip_shared(2)))%z - &
                    mesh%nodes(mesh%panels(i)%node_ids(ip_shared(1)))%z

               if (kn_flag == 1) dy = -dy  ! Y-mirror

               ! Orientation adjustment (legacy sign conventions)
               if (abs(ip_shared(2) - ip_shared(1)) == (kki - 1)) then
                  if (ip_shared(2) > ip_shared(1)) then
                     dy = -dy; dz = -dz
                  end if
               else
                  if (ip_shared(2) < ip_shared(1)) then
                     dy = -dy; dz = -dz
                  end if
               end if

               ! Net circulation magnitude
               roc = rho*(gamma(i) - gamma(j))*vinit/2.0_wp
               rc = rho*(gamma(i) - gamma(j))

               if (kn_flag == 1) roc = -roc

               ! Force contributions
               side_acc = side_acc - roc*dz
               lift_acc = lift_acc + roc*dy

               ! Wake-induced drag (requires wake velocity at panel centroid)
               if (iter > 0) then
                  call wake%compute_induced_velocity(iter, mesh%panels(i)%centroid, &
                                                     nsym, ngrnd, v_wake)
                  drag_acc = drag_acc - rc*(v_wake%x*dy - v_wake%y*dx)
               end if

            end do  ! j
         end do  ! i
      end do  ! kn_flag

      forces%lift = lift_acc
      forces%drag = drag_acc
      forces%side = side_acc

   end function compute_forces

   !> Compute pressure coefficients for specific panels (e.g., propeller)
   !!
   !! Legacy equivalent: SUBROUTINE CPAIP
   !!
   !! For panels with property_id == 2 (propeller/special):
   !!   1. Evaluate total velocity at a point offset from the panel surface
   !!      (1.3 * normal distance — legacy convention for near-field accuracy)
   !!   2. Cp = 1 - |V_total|² / Vinit²
   !!
   !! @param mesh    Surface mesh
   !! @param wake    Wake model
   !! @param gamma   Vortex strengths
   !! @param vinit   Freestream velocity
   !! @param iter    Current iteration
   !! @param nsym    Symmetry flag
   !! @param ngrnd   Ground effect flag
   !! @param cp      Output pressure coefficients (size n_panels)
   subroutine compute_pressure_coefficients(mesh, wake, gamma, vinit, &
                                            iter, nsym, ngrnd, cp)
      type(mesh_t), intent(in) :: mesh
      type(wake_t), intent(in) :: wake
      real(wp), intent(in)     :: gamma(:), vinit
      integer(ip), intent(in)  :: iter, nsym, ngrnd
      real(wp), intent(out)    :: cp(:)

      integer(ip) :: i, ik, ng, na
      type(vector3d_t) :: eval_pt, v_body, v_wake, v_total
      type(vector3d_t) :: p1, p2, p3, p4, v_ind
      real(wp) :: vr_sq, sign_factor

      cp = 0.0_wp

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP PRIVATE(i, eval_pt, v_body, v_wake, v_total, ik, ng, na, &
      !$OMP         p1, p2, p3, p4, v_ind, vr_sq, sign_factor) &
      !$OMP SCHEDULE(DYNAMIC)
      do i = 1, mesh%n_panels
         if (mesh%panels(i)%property_id /= 2) cycle

         ! Evaluation point offset from panel surface (legacy: 1.3 * normal)
         eval_pt%x = mesh%panels(i)%centroid%x + 1.3_wp*mesh%panels(i)%normal%x
         eval_pt%y = mesh%panels(i)%centroid%y + 1.3_wp*mesh%panels(i)%normal%y
         eval_pt%z = mesh%panels(i)%centroid%z + 1.3_wp*mesh%panels(i)%normal%z

         ! Body-induced velocity
         v_body = zero_vec3()
         do ng = 0, ngrnd
            do ik = 1, mesh%n_panels
               do na = 0, nsym
                  p1 = mesh%nodes(mesh%panels(ik)%node_ids(1))
                  p2 = mesh%nodes(mesh%panels(ik)%node_ids(2))
                  p3 = mesh%nodes(mesh%panels(ik)%node_ids(3))
                  p4 = mesh%nodes(mesh%panels(ik)%node_ids(4))

                  if (na == 1) then
                     p1%y = -p1%y; p2%y = -p2%y
                     p3%y = -p3%y; p4%y = -p4%y
                  end if
                  if (ng == 1) then
                     p1%z = -p1%z; p2%z = -p2%z
                     p3%z = -p3%z; p4%z = -p4%z
                  end if

                  v_ind = vortex_ring_velocity(eval_pt, p1, p2, p3, p4)
                  sign_factor = real((-1)**na, wp)*real((-1)**ng, wp)

                  v_body%x = v_body%x + v_ind%x*gamma(ik)*sign_factor
                  v_body%y = v_body%y + v_ind%y*gamma(ik)*sign_factor
                  v_body%z = v_body%z + v_ind%z*gamma(ik)*sign_factor
               end do
            end do
         end do

         ! Wake-induced velocity
         v_wake = zero_vec3()
         if (iter > 0) then
            call wake%compute_induced_velocity(iter, eval_pt, nsym, ngrnd, v_wake)
         end if

         ! Total velocity = body + wake + freestream
         v_total%x = v_body%x + v_wake%x + vinit
         v_total%y = v_body%y + v_wake%y
         v_total%z = v_body%z + v_wake%z

         vr_sq = v_total%x**2 + v_total%y**2 + v_total%z**2
         cp(i) = 1.0_wp - vr_sq/(vinit**2)
      end do
      !$OMP END PARALLEL DO

   end subroutine compute_pressure_coefficients

end module aerodynamic_loads_mod
