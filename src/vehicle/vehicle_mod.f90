module vehicle_mod
   use base_kinds_mod, only: wp, ip
   use mesh_mod, only: mesh_t
   use vehicle_config_mod, only: vehicle_config_t
   implicit none
   private
   public :: vehicle_t

   !> Two constructor overloads:
   !>   vehicle_t("path/to/vehicle.toml")  -- parses TOML and builds mesh
   !>   vehicle_t(config)                  -- builds mesh from pre-parsed config
   interface vehicle_t
      module procedure vehicle_constructor_from_toml
      module procedure vehicle_constructor_from_config
   end interface vehicle_t

   type :: vehicle_t
      integer(ip) :: id = 0_ip
      character(len=50) :: name = ""
      integer(ip) :: num_nodes = 0_ip
      integer(ip) :: num_panels = 0_ip
      type(mesh_t) :: mesh
      ! type(wake_t) :: wake
      ! type(state_t) :: state
   contains
      procedure :: rotate_origin => vehicle_rotate_origin
      procedure :: translate => vehicle_translate
   end type vehicle_t

contains

   !> Parse a vehicle TOML file and delegate to the config-based constructor.
   function vehicle_constructor_from_toml(filename) result(new_vehicle)
      type(vehicle_t) :: new_vehicle
      character(len=*), intent(in) :: filename
      type(vehicle_config_t) :: config

      call config%parse(filename)
      new_vehicle = vehicle_t(config)
   end function vehicle_constructor_from_toml

   !> Canonical constructor: drives the full vehicle -> mesh -> panels -> nodes chain.
   function vehicle_constructor_from_config(config) result(new_vehicle)
      type(vehicle_t) :: new_vehicle
      type(vehicle_config_t), intent(in) :: config

      new_vehicle%name = config%name
      new_vehicle%num_nodes = config%points_number
      new_vehicle%num_panels = config%panels_number

      new_vehicle%mesh = mesh_t(config%points_number, &
                                trim(config%points_filename), &
                                config%panels_number, &
                                trim(config%panels_filename))
   end function vehicle_constructor_from_config

   !> Apply a rigid-body rotation about the origin to the entire vehicle mesh.
   subroutine vehicle_rotate_origin(this, angle, axis, rads)
      class(vehicle_t), intent(inout) :: this
      real(wp), intent(in) :: angle
      real(wp), intent(in) :: axis(3)
      logical, intent(in), optional :: rads

      call this%mesh%rotate_origin(angle, axis, rads)
   end subroutine vehicle_rotate_origin

   !> Translate the entire vehicle mesh by a displacement vector.
   subroutine vehicle_translate(this, vector)
      class(vehicle_t), intent(inout) :: this
      real(wp), intent(in) :: vector(3)

      call this%mesh%translate(vector)
   end subroutine vehicle_translate

end module vehicle_mod
