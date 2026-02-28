module rigid_body_mod
   use base_kinds_mod, only: wp, ip
   use constants_mod, only: G_ACCEL
   use vector_ops_mod, only: cross_product
   use lapack_linalg_mod, only: inv
   implicit none
   private

   public :: rigid_body_t

   ! State Vector Indices
   integer, parameter :: IDX_POS = 1
   integer, parameter :: IDX_VEL = 4
   integer, parameter :: IDX_QUAT = 7
   integer, parameter :: IDX_RAT = 11
   integer, parameter :: STATE_SIZE = 13

   type :: rigid_body_t
      private
      ! --- Physical Properties ---
      real(wp) :: mass
      real(wp) :: inertia(3, 3)
      real(wp) :: inv_inertia(3, 3)

      ! --- State Vectors (ENU Frame: Z is UP) ---
      real(wp) :: pos_earth(3) = 0.0_wp ! x, y, z
      real(wp) :: vel_body(3) = 0.0_wp ! u, v, w
      real(wp) :: quat(4) = [1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp] ! q0, q1, q2, q3 (body->earth)
      real(wp) :: ang_vel_body(3) = 0.0_wp ! p, q, r

      ! --- Forces ---
      real(wp) :: force_sum(3)
      real(wp) :: moment_sum(3)

   contains
      procedure :: init => rigid_body_init
      procedure :: add_force => rigid_body_add_force
      procedure :: add_moment => rigid_body_add_moment
      procedure :: add_gravity => rigid_body_add_gravity
      procedure :: reset_forces => rigid_body_reset
      procedure :: step => rigid_body_integrate_rk4

      ! Getters
      procedure :: get_pos => rigid_body_get_pos
      procedure :: get_vel => rigid_body_get_vel
      procedure :: get_euler => rigid_body_get_euler
      procedure :: get_rates => rigid_body_get_rates
      procedure :: get_energy_momentum => rigid_body_get_energy_momentum
      procedure :: check_energy_momentum => rigid_body_check_energy_momentum

      ! Internals
      procedure, private :: get_derivatives
      procedure, private :: quat_to_rot
      procedure, private :: normalize_quat
      procedure, private :: pack_state
      procedure, private :: unpack_state
   end type rigid_body_t

contains

   ! --- Initialization ---
   subroutine rigid_body_init(this, mass, inertia_tensor, init_pos)
      class(rigid_body_t), intent(inout) :: this
      real(wp), intent(in) :: mass
      real(wp), intent(in) :: inertia_tensor(3, 3)
      real(wp), intent(in), optional :: init_pos(3)
      integer(ip) :: status

      this%mass = mass
      this%inertia = inertia_tensor
      call inv(this%inertia, this%inv_inertia, status)

      if (status /= 0) error stop "[RIGID BODY] Singular Inertia Matrix"
      if (present(init_pos)) this%pos_earth = init_pos
      call this%reset_forces()
   end subroutine rigid_body_init

   ! --- Force Management ---
   subroutine rigid_body_add_force(this, force, loc)
      class(rigid_body_t), intent(inout) :: this
      real(wp), intent(in) :: force(3), loc(3)
      this%force_sum = this%force_sum + force
      this%moment_sum = this%moment_sum + cross_product(loc, force)
   end subroutine rigid_body_add_force

   subroutine rigid_body_add_moment(this, moment)
      class(rigid_body_t), intent(inout) :: this
      real(wp), intent(in) :: moment(3)
      this%moment_sum = this%moment_sum + moment
   end subroutine rigid_body_add_moment

   subroutine rigid_body_add_gravity(this, gravity_earth)
      class(rigid_body_t), intent(inout) :: this
      real(wp), intent(in), optional :: gravity_earth(3)
      real(wp) :: g_e(3)
      real(wp) :: R_be(3, 3)

      if (present(gravity_earth)) then
         g_e = gravity_earth
      else
         g_e = [0.0_wp, 0.0_wp, -G_ACCEL]
      end if

      R_be = this%quat_to_rot(this%quat)
      this%force_sum = this%force_sum + matmul(transpose(R_be), this%mass*g_e)
   end subroutine rigid_body_add_gravity

   subroutine rigid_body_reset(this)
      class(rigid_body_t), intent(inout) :: this
      this%force_sum = 0.0_wp
      this%moment_sum = 0.0_wp
   end subroutine rigid_body_reset

   ! =========================================================
   ! RK4 Integration Engine
   ! =========================================================
   subroutine rigid_body_integrate_rk4(this, dt)
      class(rigid_body_t), intent(inout) :: this
      real(wp), intent(in) :: dt
      real(wp) :: y(STATE_SIZE), k1(STATE_SIZE), k2(STATE_SIZE)
      real(wp) :: k3(STATE_SIZE), k4(STATE_SIZE), y_temp(STATE_SIZE)

      call this%pack_state(y)

      k1 = this%get_derivatives(y)
      y_temp = y + (0.5_wp*dt*k1)
      k2 = this%get_derivatives(y_temp)
      y_temp = y + (0.5_wp*dt*k2)
      k3 = this%get_derivatives(y_temp)
      y_temp = y + (dt*k3)
      k4 = this%get_derivatives(y_temp)

      y = y + (dt/6.0_wp)*(k1 + 2.0_wp*k2 + 2.0_wp*k3 + k4) ! O(dt^5) local truncation error

      call this%unpack_state(y)
      call this%reset_forces()
   end subroutine rigid_body_integrate_rk4

   ! =========================================================
   ! Physics Core (Derivatives)
   ! =========================================================
   function get_derivatives(this, state) result(dstate)
      class(rigid_body_t), intent(in) :: this
      real(wp), intent(in) :: state(STATE_SIZE)
      real(wp) :: dstate(STATE_SIZE)

      ! Locals
      real(wp) :: v_b(3), w_b(3), q(4)
      real(wp) :: accel(3), alpha(3)
      real(wp) :: term_gyr(3), term_mom(3)
      real(wp) :: R_be(3, 3)
      real(wp) :: q0, q1, q2, q3

      ! Unpack
      v_b = state(IDX_VEL:IDX_VEL + 2)
      q = state(IDX_QUAT:IDX_QUAT + 3)
      w_b = state(IDX_RAT:IDX_RAT + 2)

      ! 1. Translational: a = F/m - (w x v)
      accel = (this%force_sum/this%mass) - cross_product(w_b, v_b)

      ! 2. Rotational: alpha = I_inv * (M - w x Iw)
      term_gyr = cross_product(w_b, matmul(this%inertia, w_b))
      term_mom = this%moment_sum - term_gyr
      alpha = matmul(this%inv_inertia, term_mom)

      ! 3. Kinematics (Quaternion Rates)
      q0 = q(1); q1 = q(2); q2 = q(3); q3 = q(4)
      dstate(IDX_QUAT) = -0.5_wp*(q1*w_b(1) + q2*w_b(2) + q3*w_b(3))
      dstate(IDX_QUAT + 1) = 0.5_wp*(q0*w_b(1) + q2*w_b(3) - q3*w_b(2))
      dstate(IDX_QUAT + 2) = 0.5_wp*(q0*w_b(2) + q3*w_b(1) - q1*w_b(3))
      dstate(IDX_QUAT + 3) = 0.5_wp*(q0*w_b(3) + q1*w_b(2) - q2*w_b(1))

      ! 4. Position Rates (R_be * v_b)
      ! Note: This matrix converts Body(Forward-Left-Up) to Earth(East-North-Up)
      R_be = this%quat_to_rot(q)
      dstate(IDX_POS:IDX_POS + 2) = matmul(R_be, v_b)

      dstate(IDX_VEL:IDX_VEL + 2) = accel
      dstate(IDX_RAT:IDX_RAT + 2) = alpha
   end function get_derivatives

   ! --- Packing Helpers ---
   subroutine pack_state(this, y)
      class(rigid_body_t), intent(in) :: this
      real(wp), intent(out) :: y(STATE_SIZE)
      y(IDX_POS:IDX_POS + 2) = this%pos_earth
      y(IDX_VEL:IDX_VEL + 2) = this%vel_body
      y(IDX_QUAT:IDX_QUAT + 3) = this%quat
      y(IDX_RAT:IDX_RAT + 2) = this%ang_vel_body
   end subroutine pack_state

   subroutine unpack_state(this, y)
      class(rigid_body_t), intent(inout) :: this
      real(wp), intent(in) :: y(STATE_SIZE)
      this%pos_earth = y(IDX_POS:IDX_POS + 2)
      this%vel_body = y(IDX_VEL:IDX_VEL + 2)
      this%quat = y(IDX_QUAT:IDX_QUAT + 3)
      call this%normalize_quat(this%quat)
      this%ang_vel_body = y(IDX_RAT:IDX_RAT + 2)
   end subroutine unpack_state

   ! --- Getters ---
   function rigid_body_get_pos(this) result(res)
      class(rigid_body_t), intent(in) :: this
      real(wp) :: res(3)
      res = this%pos_earth
   end function rigid_body_get_pos

   function rigid_body_get_vel(this) result(res)
      class(rigid_body_t), intent(in) :: this
      real(wp) :: res(3)
      res = this%vel_body
   end function rigid_body_get_vel

   function rigid_body_get_euler(this) result(res)
      class(rigid_body_t), intent(in) :: this
      real(wp) :: res(3)
      real(wp) :: R_be(3, 3)
      real(wp) :: theta

      R_be = this%quat_to_rot(this%quat)

      theta = -asin(max(-1.0_wp, min(1.0_wp, R_be(3, 1))))
      res(1) = atan2(R_be(3, 2), R_be(3, 3))
      res(2) = theta
      res(3) = atan2(R_be(2, 1), R_be(1, 1))
   end function rigid_body_get_euler

   function rigid_body_get_rates(this) result(res)
      class(rigid_body_t), intent(in) :: this
      real(wp) :: res(3)
      res = this%ang_vel_body
   end function rigid_body_get_rates

   subroutine rigid_body_get_energy_momentum(this, energy, lin_mom_earth, ang_mom_earth)
      class(rigid_body_t), intent(in) :: this
      real(wp), intent(out) :: energy
      real(wp), intent(out) :: lin_mom_earth(3)
      real(wp), intent(out) :: ang_mom_earth(3)
      real(wp) :: R_be(3, 3)
      real(wp) :: v_e(3)
      real(wp) :: h_b(3)

      R_be = this%quat_to_rot(this%quat)
      v_e = matmul(R_be, this%vel_body)
      lin_mom_earth = this%mass*v_e

      h_b = matmul(this%inertia, this%ang_vel_body)
      ang_mom_earth = matmul(R_be, h_b)

      energy = 0.5_wp*this%mass*dot_product(this%vel_body, this%vel_body) + &
               0.5_wp*dot_product(this%ang_vel_body, h_b)
   end subroutine rigid_body_get_energy_momentum

   subroutine rigid_body_check_energy_momentum(this, energy_ref, lin_mom_ref, ang_mom_ref, &
                                               rel_err_energy, rel_err_lin, rel_err_ang, ok, tol)
      class(rigid_body_t), intent(in) :: this
      real(wp), intent(in) :: energy_ref
      real(wp), intent(in) :: lin_mom_ref(3)
      real(wp), intent(in) :: ang_mom_ref(3)
      real(wp), intent(out) :: rel_err_energy
      real(wp), intent(out) :: rel_err_lin
      real(wp), intent(out) :: rel_err_ang
      logical, intent(out) :: ok
      real(wp), intent(in), optional :: tol
      real(wp) :: energy
      real(wp) :: lin_mom(3)
      real(wp) :: ang_mom(3)
      real(wp) :: tol_use
      real(wp) :: denom

      call this%get_energy_momentum(energy, lin_mom, ang_mom)

      tol_use = 1.0e-6_wp
      if (present(tol)) tol_use = tol

      denom = max(abs(energy_ref), tiny(energy_ref))
      rel_err_energy = abs(energy - energy_ref)/denom

      denom = max(sqrt(dot_product(lin_mom_ref, lin_mom_ref)), tiny(lin_mom_ref(1)))
      rel_err_lin = sqrt(dot_product(lin_mom - lin_mom_ref, lin_mom - lin_mom_ref))/denom

      denom = max(sqrt(dot_product(ang_mom_ref, ang_mom_ref)), tiny(ang_mom_ref(1)))
      rel_err_ang = sqrt(dot_product(ang_mom - ang_mom_ref, ang_mom - ang_mom_ref))/denom

      ok = (rel_err_energy <= tol_use) .and. (rel_err_lin <= tol_use) .and. (rel_err_ang <= tol_use)
   end subroutine rigid_body_check_energy_momentum

   function quat_to_rot(this, q) result(R_be)
      class(rigid_body_t), intent(in) :: this
      real(wp), intent(in) :: q(4)
      real(wp) :: R_be(3, 3)
      real(wp) :: q0, q1, q2, q3

      q0 = q(1); q1 = q(2); q2 = q(3); q3 = q(4)

      R_be(1, 1) = 1.0_wp - 2.0_wp*(q2*q2 + q3*q3)
      R_be(1, 2) = 2.0_wp*(q1*q2 - q0*q3)
      R_be(1, 3) = 2.0_wp*(q1*q3 + q0*q2)

      R_be(2, 1) = 2.0_wp*(q1*q2 + q0*q3)
      R_be(2, 2) = 1.0_wp - 2.0_wp*(q1*q1 + q3*q3)
      R_be(2, 3) = 2.0_wp*(q2*q3 - q0*q1)

      R_be(3, 1) = 2.0_wp*(q1*q3 - q0*q2)
      R_be(3, 2) = 2.0_wp*(q2*q3 + q0*q1)
      R_be(3, 3) = 1.0_wp - 2.0_wp*(q1*q1 + q2*q2)
   end function quat_to_rot

   subroutine normalize_quat(this, q)
      class(rigid_body_t), intent(in) :: this
      real(wp), intent(inout) :: q(4)
      real(wp) :: qn

      qn = sqrt(dot_product(q, q))
      if (qn <= tiny(qn)) then
         q = [1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp]
      else
         q = q/qn
      end if
   end subroutine normalize_quat

end module rigid_body_mod
