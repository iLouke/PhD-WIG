module influence_mod
   !! Aerodynamic Influence Coefficient (AIC) Matrix
   !!
   !! Assembles the linear system A * gamma = b for the vortex lattice method:
   !!   A(i,j) = normal velocity at panel i due to unit vortex ring on panel j
   !!   b(i)   = -(freestream + wake) . n_i (no-penetration RHS)
   !!
   !! Legacy mapping:
   !!   SUBROUTINE VORCALC -> build_aic_matrix() + build_rhs()
   !!   A(I,J) array       -> aic_matrix(:,:)
   !!   B(II) array        -> rhs_vector(:)
   !!
   !! OpenMP parallelization:
   !!   The AIC matrix assembly is O(N²) — each row i is independent and
   !!   requires N Biot-Savart evaluations. The outer loop over i is
   !!   parallelized with OpenMP PARALLEL DO.
   !!
   !! Symmetry & ground effect:
   !!   Each panel influence includes contributions from:
   !!     - Direct panel (always)
   !!     - Y-mirrored panel (if nsym=1)
   !!     - Z-mirrored panel (if ngrnd=1)
   !!     - Both Y+Z mirrored (if nsym=1 AND ngrnd=1)
   use base_kinds_mod, only: wp, ip
   use vector3d_mod, only: vector3d_t, vec3, zero_vec3, &
                           operator(+), operator(*), operator(.dot.)
   use grid_mod, only: mesh_t
   use vortex_mod, only: vortex_ring_velocity
   use wake_mod, only: wake_t
   use aero_solver_mod, only: aero_linsys_t
!$ use omp_lib
   implicit none
   private

   public :: build_aic_matrix
   public :: build_rhs
   public :: solve_system

contains

   !> Build the Aerodynamic Influence Coefficient matrix
   !!
   !! Legacy equivalent: The ITER==0 branch of SUBROUTINE VORCALC
   !!
   !! For each panel pair (i,j), computes the normal component of
   !! velocity induced at panel i's collocation point by a unit-strength
   !! vortex ring on panel j:
   !!   A(i,j) = V_induced(at_panel_i, by_panel_j) . n_i
   !!
   !! Includes symmetry and ground-effect mirror contributions.
   !!
   !! OpenMP: The outer loop (i) is parallelized. Each thread computes
   !! a full row of the AIC matrix independently.
   !!
   !! @param mesh  Surface mesh with computed panel geometry
   !! @param nsym  Y-symmetry flag (0 or 1)
   !! @param ngrnd Ground effect flag (0 or 1)
   !! @param A     Output AIC matrix (n_panels x n_panels), allocated by caller
   subroutine build_aic_matrix(mesh, nsym, ngrnd, A)
      type(mesh_t), intent(in) :: mesh
      integer(ip), intent(in)  :: nsym, ngrnd
      real(wp), intent(out)    :: A(:, :)

      integer(ip) :: i, j, na, ng, npan
      type(vector3d_t) :: eval_pt, p1, p2, p3, p4, v_ind, normal_i
      real(wp) :: sign_factor

      npan = mesh%n_panels
      A = 0.0_wp

      ! ── AIC Assembly (OpenMP parallelized over rows) ────────────────
      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP PRIVATE(i, j, na, ng, eval_pt, normal_i, p1, p2, p3, p4, v_ind, sign_factor) &
      !$OMP SCHEDULE(DYNAMIC)
      do i = 1, npan
         eval_pt = mesh%panels(i)%centroid
         normal_i = mesh%panels(i)%normal

         ! Loop over symmetry states: ground (ng=0,1) then Y-mirror (na=0,1)
         do ng = 0, ngrnd
            do j = 1, npan
               do na = 0, nsym

                  ! Get panel j corners
                  p1 = mesh%nodes(mesh%panels(j)%node_ids(1))
                  p2 = mesh%nodes(mesh%panels(j)%node_ids(2))
                  p3 = mesh%nodes(mesh%panels(j)%node_ids(3))
                  p4 = mesh%nodes(mesh%panels(j)%node_ids(4))

                  ! Apply Y-mirror (symmetry)
                  if (na == 1) then
                     p1%y = -p1%y; p2%y = -p2%y
                     p3%y = -p3%y; p4%y = -p4%y
                  end if

                  ! Apply Z-mirror (ground effect)
                  if (ng == 1) then
                     p1%z = -p1%z; p2%z = -p2%z
                     p3%z = -p3%z; p4%z = -p4%z
                  end if

                  ! Biot-Savart induced velocity
                  v_ind = vortex_ring_velocity(eval_pt, p1, p2, p3, p4)

                  ! Sign factor: (-1)^na * (-1)^ng
                  sign_factor = real((-1)**na, wp)*real((-1)**ng, wp)

                  ! Normal component: A(i,j) += (V . n_i) * sign
                  A(i, j) = A(i, j) + (v_ind.dot.normal_i)*sign_factor

               end do  ! na (symmetry)
            end do  ! j (influencing panel)
         end do  ! ng (ground)
      end do  ! i (influenced panel)
      !$OMP END PARALLEL DO

   end subroutine build_aic_matrix

   !> Build the Right-Hand Side vector for the no-penetration condition
   !!
   !! Legacy equivalent: The b(II) computation in VORCALC (after label 25)
   !!
   !! RHS = -(V_freestream + V_wake) . n_i for each panel i
   !!
   !! The freestream velocity is [Vinit, 0, 0] for panels with property /= 14.
   !! Panels with property_id == 14 have zero freestream (legacy KPV=0).
   !!
   !! @param mesh    Surface mesh
   !! @param wake    Wake model (may have zero panels for first iteration)
   !! @param vinit   Freestream velocity magnitude [m/s]
   !! @param iter    Current iteration (0 = no wake contribution)
   !! @param nsym    Y-symmetry flag
   !! @param ngrnd   Ground effect flag
   !! @param vaip    Additional induced velocity component (legacy VAIP)
   !! @param rhs     Output RHS vector (size n_panels)
   subroutine build_rhs(mesh, wake, vinit, iter, nsym, ngrnd, vaip, rhs)
      type(mesh_t), intent(in) :: mesh
      type(wake_t), intent(in) :: wake
      real(wp), intent(in)     :: vinit
      integer(ip), intent(in)  :: iter, nsym, ngrnd
      real(wp), intent(in)     :: vaip
      real(wp), intent(out)    :: rhs(:)

      integer(ip) :: ii, npan
      type(vector3d_t) :: eval_pt, normal_i, v_wake
      real(wp) :: v_free_x, kpv_factor

      npan = mesh%n_panels

      !$OMP PARALLEL DO DEFAULT(SHARED) &
      !$OMP PRIVATE(ii, eval_pt, normal_i, v_wake, v_free_x, kpv_factor) &
      !$OMP SCHEDULE(STATIC)
      do ii = 1, npan
         eval_pt = mesh%panels(ii)%centroid
         normal_i = mesh%panels(ii)%normal

         ! Freestream factor: 0 for property_id==14, 1 otherwise
         ! Legacy: KPV=1; IF(IPROP(II).EQ.14) KPV=0
         kpv_factor = 1.0_wp
         if (mesh%panels(ii)%property_id == 14) kpv_factor = 0.0_wp

         v_free_x = vinit*kpv_factor

         ! Wake-induced velocity at this panel's collocation point
         v_wake = zero_vec3()
         if (iter > 0) then
            call wake%compute_induced_velocity(iter, eval_pt, nsym, ngrnd, v_wake)
         end if

         ! RHS: -(V_free + V_wake) . n
         ! Legacy: B(II) = -(VINIT*KPV+VXV)*ANX(II) - VYV*ANY(II) - VZV*ANZ(II) + VAIP
         rhs(ii) = -((v_free_x + v_wake%x)*normal_i%x + &
                     v_wake%y*normal_i%y + &
                     v_wake%z*normal_i%z) + vaip
      end do
      !$OMP END PARALLEL DO

   end subroutine build_rhs

   !> Solve the complete AIC system: factorize (if needed) and back-substitute
   !!
   !! Legacy equivalent: CALL SVDBK(NPAN, ITER) in VORCALC
   !!
   !! Workflow:
   !!   - iter==0: Build AIC matrix, factorize (LU), solve
   !!   - iter>0:  Reuse LU factors, solve with new RHS only
   !!
   !! @param solver  Linear system solver (stores LU factors)
   !! @param mesh    Surface mesh
   !! @param wake    Wake model
   !! @param vinit   Freestream velocity
   !! @param iter    Current iteration
   !! @param nsym    Symmetry flag
   !! @param ngrnd   Ground effect flag
   !! @param vaip    Additional velocity component
   !! @param gamma   Output: vortex strengths (solution vector)
   !! @param info    LAPACK status
   subroutine solve_system(solver, mesh, wake, vinit, iter, nsym, ngrnd, &
                           vaip, gamma, info)
      type(aero_linsys_t), intent(inout) :: solver
      type(mesh_t), intent(in)           :: mesh
      type(wake_t), intent(in)           :: wake
      real(wp), intent(in)               :: vinit
      integer(ip), intent(in)            :: iter, nsym, ngrnd
      real(wp), intent(in)               :: vaip
      real(wp), intent(out)              :: gamma(:)
      integer, intent(out)               :: info

      real(wp), allocatable :: A(:, :), rhs(:)
      integer(ip) :: npan

      npan = mesh%n_panels

      ! First iteration: build and factorize AIC matrix
      if (.not. solver%is_ready()) then
         allocate (A(npan, npan))
         call build_aic_matrix(mesh, nsym, ngrnd, A)
         call solver%factorize(A, info)
         deallocate (A)
         if (info /= 0) return
      end if

      ! Build RHS for current iteration
      allocate (rhs(npan))
      call build_rhs(mesh, wake, vinit, iter, nsym, ngrnd, vaip, rhs)

      ! Solve: gamma = A^(-1) * rhs
      call solver%solve(rhs, info)

      ! Copy solution
      gamma(1:npan) = rhs(1:npan)
      deallocate (rhs)

   end subroutine solve_system

end module influence_mod
