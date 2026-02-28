module wake_types_mod
   use base_kinds_mod, only: wp, ip
   use point_types_mod, only: point_t
   implicit none
   private

   public :: wake_panel_t
   public :: wake_particle_t
   public :: wake_data_t

   type :: wake_panel_t
      integer(ip) :: id = 0_ip
      type(point_t) :: points(4)
      real(wp) :: vorticity_strength = 0.0_wp
   end type wake_panel_t

   type :: wake_particle_t
      integer(ip) :: id = 0_ip
      type(point_t) :: position
      real(wp) :: vorticity(3) = 0.0_wp
      real(wp) :: core_radius = 0.0_wp
      logical :: active = .false.
   end type wake_particle_t

   type :: wake_data_t
      type(wake_panel_t), allocatable :: wake_panels(:)
      type(wake_particle_t), allocatable :: wake_particles(:)
      integer(ip) :: n_wake_panels = 0_ip
      integer(ip) :: n_wake_particles = 0_ip
   end type wake_data_t

end module wake_types_mod
