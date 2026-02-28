module atmosphere_mod
   use base_kinds_mod, only: wp
   use logger_mod, only: global_logger, LOG_INFO
   implicit none
   private

   public :: Air

   ! ISA / thermodynamic constants
   real(wp), parameter :: GAMMA = 1.4_wp
   real(wp), parameter :: R_GAS = 287.05_wp          ! [J/(kg.K)]
   real(wp), parameter :: P_SL = 101325.0_wp         ! [Pa]
   real(wp), parameter :: T_SL = 288.15_wp           ! [K]
   real(wp), parameter :: MU_SL = 1.7894e-5_wp       ! [Pa.s]
   real(wp), parameter :: T_S = 110.4_wp             ! [K] Sutherland constant

   type :: Air
      real(wp) :: altitude       = 0.0_wp  ! [m]
      real(wp) :: temperature    = 0.0_wp  ! [K]
      real(wp) :: pressure       = 0.0_wp  ! [Pa]
      real(wp) :: density        = 0.0_wp  ! [kg/m^3]
      real(wp) :: viscosity      = 0.0_wp  ! [Pa.s]
      real(wp) :: speed_of_sound = 0.0_wp  ! [m/s]
   contains
      procedure :: update => atm_update
      procedure :: display => atm_display
   end type Air

contains

   subroutine atm_update(self, altitude)
      class(Air), intent(inout) :: self
      real(wp), intent(in) :: altitude

      self%altitude = altitude
      self%temperature = calc_temperature(altitude)
      self%pressure = calc_pressure(altitude)
      self%density = calc_density(self%pressure, self%temperature)
      self%viscosity = calc_viscosity(self%temperature)

      if (self%density > 0.0_wp) then
         self%speed_of_sound = sqrt(GAMMA*self%pressure/self%density)
      else
         self%speed_of_sound = 0.0_wp
      end if
   end subroutine atm_update

   subroutine atm_display(self)
      class(Air), intent(in) :: self
      character(len=256) :: line

      call global_logger%msg(LOG_INFO, "[ATMOSPHERE (ISA)]")

      write (line, '(A30, F15.4, 1X, A)') "   Altitude:", self%altitude, "m"
      call global_logger%msg(LOG_INFO, trim(line))

      write (line, '(A30, F15.4, 1X, A)') "   Temperature:", self%temperature, "K"
      call global_logger%msg(LOG_INFO, trim(line))

      write (line, '(A30, ES15.4, 1X, A)') "   Pressure:", self%pressure, "Pa"
      call global_logger%msg(LOG_INFO, trim(line))

      write (line, '(A30, F15.4, 1X, A)') "   Density:", self%density, "kg/m^3"
      call global_logger%msg(LOG_INFO, trim(line))

      write (line, '(A30, ES15.4, 1X, A)') "   Dyn. Viscosity:", self%viscosity, "Pa.s"
      call global_logger%msg(LOG_INFO, trim(line))

      write (line, '(A30, F15.4, 1X, A)') "   Speed of Sound:", self%speed_of_sound, "m/s"
      call global_logger%msg(LOG_INFO, trim(line))
      call global_logger%msg(LOG_INFO, "")
   end subroutine atm_display

   pure function calc_temperature(alt) result(T)
      real(wp), intent(in) :: alt
      real(wp) :: T, z, term1, term2

      z = alt*1.0e-3_wp
      if (z < 47.0_wp) then
         term1 = exp(35.75_wp - 3.25_wp*z)
         term2 = exp(-3.0_wp + 0.0003_wp*z**3)
         T = 216.65_wp + 2.0_wp*log(1.0_wp + term1 + term2)
      else
         T = 216.65_wp
      end if
   end function calc_temperature

   pure function calc_pressure(alt) result(P)
      real(wp), intent(in) :: alt
      real(wp) :: P, z, frac, expo

      z = alt*1.0e-3_wp
      frac = (0.0015_wp*z**2)/(1.0_wp - 0.018_wp*z + 0.0011_wp*z**2)
      expo = exp(-0.118_wp*z - frac)
      P = P_SL*expo
   end function calc_pressure

   pure function calc_density(P, T) result(rho)
      real(wp), intent(in) :: P, T
      real(wp) :: rho

      if (T > 0.0_wp) then
         rho = P/(R_GAS*T)
      else
         rho = 0.0_wp
      end if
   end function calc_density

   pure function calc_viscosity(T) result(mu)
      real(wp), intent(in) :: T
      real(wp) :: mu

      if (T > 0.0_wp) then
         mu = MU_SL*((T/T_S)**1.5_wp)*((T_SL + T_S)/(T + T_S))
      else
         mu = 0.0_wp
      end if
   end function calc_viscosity

end module atmosphere_mod
