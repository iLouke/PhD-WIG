program test_rk4
   use base_kinds_mod, only: wp
   use constants_mod, only: G_ACCEL
   use rigid_body_mod
   use helper_mod, only: real_to_char
   implicit none

   integer :: fail_count = 0

   print *, "=========================================="
   print *, "   RUNNING RK4 INTEGRATION TESTS           "
   print *, "=========================================="

   call test_constant_acceleration()
   call test_energy_conservation()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All RK4 tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   ! --- Test 1: Constant Acceleration (Free Fall) ---
   subroutine test_constant_acceleration()
      type(rigid_body_t) :: test_body
      real(wp), parameter :: dt = 0.01_wp
      real(wp), parameter :: g = G_ACCEL
      real(wp), parameter :: inertia_id(3, 3) = reshape([1.0_wp, 0.0_wp, 0.0_wp, &
                                                         0.0_wp, 1.0_wp, 0.0_wp, &
                                                         0.0_wp, 0.0_wp, 1.0_wp], shape=[3, 3])
      real(wp) :: z0, v0, a, t_final
      real(wp) :: z_analytical, z_numerical, error, err_rel
      real(wp) :: pos_tmp(3)
      integer :: n_steps, i
      logical :: pass

      print *, "Testing RK4 Integrator..."

      z0 = 100.0_wp
      v0 = 10.0_wp
      a = -g
      t_final = 2.0_wp

      call test_body%init(mass=1.0_wp, inertia_tensor=inertia_id, init_pos=[0.0_wp, 0.0_wp, z0])

      n_steps = int(t_final/dt)

      ! Apply impulse for initial velocity
      call test_body%add_force([0.0_wp, 0.0_wp, v0/dt], [0.0_wp, 0.0_wp, 0.0_wp])
      call test_body%step(dt)

      ! Apply gravity for remaining steps
      do i = 2, n_steps
         call test_body%add_force([0.0_wp, 0.0_wp, -g], [0.0_wp, 0.0_wp, 0.0_wp])
         call test_body%step(dt)
      end do

      pos_tmp = test_body%get_pos()
      z_numerical = pos_tmp(3)

      ! Analytical: z(t) = z0 + v0*t + 0.5*a*t^2
      z_analytical = z0 + v0*t_final + 0.5_wp*a*t_final*t_final

      error = abs(z_numerical - z_analytical)
      err_rel = error/abs(z_analytical)

      pass = (err_rel < 0.01_wp)
      call assert(pass, "Constant acceleration (rel_err="//trim(real_to_char(err_rel*100.0_wp))//"%)")

   end subroutine test_constant_acceleration

   ! --- Test 2: Energy Conservation ---
   subroutine test_energy_conservation()
      type(rigid_body_t) :: test_body
      real(wp), parameter :: dt = 0.01_wp
      real(wp), parameter :: inertia_id(3, 3) = reshape([1.0_wp, 0.0_wp, 0.0_wp, &
                                                         0.0_wp, 1.0_wp, 0.0_wp, &
                                                         0.0_wp, 0.0_wp, 1.0_wp], shape=[3, 3])
      real(wp) :: energy_ref, energy_rel_err, lin_mom_rel_err, ang_mom_rel_err
      real(wp) :: lin_mom_ref(3), ang_mom_ref(3)
      real(wp) :: force_impulse(3)
      logical :: pass
      integer :: n_steps, i

      call test_body%init(mass=1.0_wp, inertia_tensor=inertia_id, init_pos=[0.0_wp, 0.0_wp, 0.0_wp])

      ! Apply single impulse
      force_impulse = [1.0_wp, 0.0_wp, 0.0_wp]
      call test_body%add_force(force_impulse, [0.0_wp, 0.0_wp, 0.0_wp])
      call test_body%step(dt)

      ! Capture reference energy/momentum
      call test_body%get_energy_momentum(energy_ref, lin_mom_ref, ang_mom_ref)

      ! Free flight for many steps (no forces/moments)
      n_steps = 500
      do i = 1, n_steps
         call test_body%step(dt)
      end do

      ! Check conservation
      call test_body%check_energy_momentum(energy_ref, lin_mom_ref, ang_mom_ref, &
                                           energy_rel_err, lin_mom_rel_err, ang_mom_rel_err, pass)

      call assert(pass, "Energy/momentum conservation (E_err="//trim(real_to_char(energy_rel_err*100.0_wp))//"%)")

   end subroutine test_energy_conservation

   ! --- Helper: Assertion ---
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

end program test_rk4
