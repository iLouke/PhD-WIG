module mesh_types_mod
   use base_kinds_mod, only: wp, ip
   use point_types_mod, only: point_t
   use panel_types_mod, only: panel_data_t
   implicit none
   private

   public :: mesh_data_t

   type :: mesh_data_t
      type(point_t), allocatable :: points(:)
      type(panel_data_t), allocatable :: panels(:)
      integer(ip) :: n_points = 0_ip
      integer(ip) :: n_panels = 0_ip
      !> Center of Gravity (for force/moment calculations)
      real(wp) :: cg(3) = 0.0_wp
      !> Center of Pressure (for aerodynamic moment calculations)
      real(wp) :: cp(3) = 0.0_wp
      !> Center of Buoyancy (for hydrostatic calculations, if needed)
      real(wp) :: cb(3) = 0.0_wp
      !> Reference dimensions for non-dimensionalization
      real(wp) :: aspect_ratio = 0.0_wp
      real(wp) :: wing_area = 0.0_wp
      real(wp) :: chord = 0.0_wp
      real(wp) :: span = 0.0_wp
      real(wp) :: ref_area = 0.0_wp
      real(wp) :: ref_length = 0.0_wp
      real(wp) :: ref_span = 0.0_wp
   end type mesh_data_t

end module mesh_types_mod
