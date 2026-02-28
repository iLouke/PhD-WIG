module symmetry_mod
   !! Symmetry and Ground Effect Operations
   !!
   !! Provides mirror-image operations for:
   !!   1. Y-plane symmetry (NSYM=1): reflects across Y=0 plane
   !!   2. Ground effect (NGRND=1): reflects across Z=0 plane
   !!
   !! Legacy mapping:
   !!   The legacy code implemented symmetry inline within VORCALC, VELWAK,
   !!   VELPAN, AIRLOAD1, and CPAIP by:
   !!     - Y-symmetry: Y1=-Y1, Y2=-Y2, ... (mirror Y coordinates)
   !!     - Ground effect: Z1=-Z1, Z2=-Z2, ... (mirror Z coordinates)
   !!     - Sign factor: (-1)^NA for symmetry, (-1)^NG for ground
   !!
   !! In the modern code, these are encapsulated as pure functions.
   use base_kinds_mod, only: wp
   use vector3d_mod, only: vector3d_t, vec3
   implicit none
   private

   public :: mirror_y, mirror_z
   public :: symmetry_sign

   ! ─── Symmetry mode flags ─────────────────────────────────────────────
   integer, parameter, public :: SYM_NONE = 0  !! No symmetry
   integer, parameter, public :: SYM_Y = 1  !! Y-plane mirror (lateral symmetry)
   integer, parameter, public :: GND_NONE = 0  !! No ground effect
   integer, parameter, public :: GND_MIRROR = 1  !! Ground mirror (Z-plane)

contains

   !> Mirror a point across the Y=0 plane (negate Y component)
   !!
   !! Legacy: Y1=-Y1, Y2=-Y2, Y3=-Y3, Y4=-Y4
   !! Used for lateral symmetry — the symmetric half-model has
   !! mirrored Y coordinates.
   pure function mirror_y(v) result(vm)
      type(vector3d_t), intent(in) :: v
      type(vector3d_t) :: vm
      vm%x = v%x
      vm%y = -v%y
      vm%z = v%z
   end function mirror_y

   !> Mirror a point across the Z=0 plane (negate Z component)
   !!
   !! Legacy: Z1=-Z1, Z2=-Z2, Z3=-Z3, Z4=-Z4
   !! Used for ground effect — the image system below the ground plane
   !! has mirrored Z coordinates.
   pure function mirror_z(v) result(vm)
      type(vector3d_t), intent(in) :: v
      type(vector3d_t) :: vm
      vm%x = v%x
      vm%y = v%y
      vm%z = -v%z
   end function mirror_z

   !> Compute sign factor for symmetry/ground contributions
   !!
   !! Legacy: (-1)^NA for symmetry, (-1)^NG for ground
   !! Both factors are applied multiplicatively to the induced velocity.
   !!
   !! @param is_mirror  .true. if this is a mirror image contribution
   !! @return           +1.0 for direct, -1.0 for mirror
   pure function symmetry_sign(is_mirror) result(s)
      logical, intent(in) :: is_mirror
      real(wp) :: s
      if (is_mirror) then
         s = -1.0_wp
      else
         s = 1.0_wp
      end if
   end function symmetry_sign

end module symmetry_mod
