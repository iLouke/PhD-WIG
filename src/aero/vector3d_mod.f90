module vector3d_mod
   !! 3D Vector Type with Operator Overloading
   !!
   !! Provides a clean mathematical abstraction for 3D vector operations,
   !! replacing the scattered X(), Y(), Z() arrays in the legacy code.
   !!
   !! Legacy mapping:
   !!   X(I), Y(I), Z(I)         -> vec%x, vec%y, vec%z
   !!   ANX(I), ANY(I), ANZ(I)   -> normal%x, normal%y, normal%z
   !!   GX(I), GY(I), GZ(I)     -> centroid%x, centroid%y, centroid%z
   !!
   !! Supports: +, -, *, /, .dot., .cross., norm, normalize, distance
   use base_kinds_mod, only: wp
   implicit none
   private

   public :: vector3d_t, vec3
   public :: operator(+), operator(-), operator(*), operator(/)
   public :: operator(.dot.), operator(.cross.)
   public :: norm, normalize, distance, zero_vec3

   ! ─── Type Definition ─────────────────────────────────────────────────
   type :: vector3d_t
      real(wp) :: x = 0.0_wp
      real(wp) :: y = 0.0_wp
      real(wp) :: z = 0.0_wp
   contains
      procedure :: magnitude => vec3_magnitude
      procedure :: unit => vec3_unit
      procedure :: to_array => vec3_to_array
   end type vector3d_t

   ! ─── Operator Interfaces ─────────────────────────────────────────────
   interface operator(+)
      module procedure vec3_add
   end interface

   interface operator(-)
      module procedure vec3_sub
      module procedure vec3_negate
   end interface

   interface operator(*)
      module procedure vec3_scalar_mul_r   ! scalar * vec
      module procedure vec3_scalar_mul_l   ! vec * scalar
   end interface

   interface operator(/)
      module procedure vec3_scalar_div
   end interface

   interface operator(.dot.)
      module procedure vec3_dot
   end interface

   interface operator(.cross.)
      module procedure vec3_cross
   end interface

   ! ─── Standalone Function Interfaces ──────────────────────────────────
   interface norm
      module procedure vec3_norm
   end interface

   interface normalize
      module procedure vec3_normalize
   end interface

   interface distance
      module procedure vec3_distance
   end interface

contains

   ! ─── Constructor ─────────────────────────────────────────────────────
   !> Create a vector3d_t from three components
   pure function vec3(x, y, z) result(v)
      real(wp), intent(in) :: x, y, z
      type(vector3d_t) :: v
      v%x = x; v%y = y; v%z = z
   end function vec3

   !> Return the zero vector
   pure function zero_vec3() result(v)
      type(vector3d_t) :: v
      v%x = 0.0_wp; v%y = 0.0_wp; v%z = 0.0_wp
   end function zero_vec3

   ! ─── Arithmetic Operators ────────────────────────────────────────────
   pure function vec3_add(a, b) result(c)
      type(vector3d_t), intent(in) :: a, b
      type(vector3d_t) :: c
      c%x = a%x + b%x
      c%y = a%y + b%y
      c%z = a%z + b%z
   end function vec3_add

   pure function vec3_sub(a, b) result(c)
      type(vector3d_t), intent(in) :: a, b
      type(vector3d_t) :: c
      c%x = a%x - b%x
      c%y = a%y - b%y
      c%z = a%z - b%z
   end function vec3_sub

   pure function vec3_negate(a) result(c)
      type(vector3d_t), intent(in) :: a
      type(vector3d_t) :: c
      c%x = -a%x; c%y = -a%y; c%z = -a%z
   end function vec3_negate

   pure function vec3_scalar_mul_r(s, a) result(c)
      real(wp), intent(in) :: s
      type(vector3d_t), intent(in) :: a
      type(vector3d_t) :: c
      c%x = s*a%x; c%y = s*a%y; c%z = s*a%z
   end function vec3_scalar_mul_r

   pure function vec3_scalar_mul_l(a, s) result(c)
      type(vector3d_t), intent(in) :: a
      real(wp), intent(in) :: s
      type(vector3d_t) :: c
      c%x = a%x*s; c%y = a%y*s; c%z = a%z*s
   end function vec3_scalar_mul_l

   pure function vec3_scalar_div(a, s) result(c)
      type(vector3d_t), intent(in) :: a
      real(wp), intent(in) :: s
      type(vector3d_t) :: c
      c%x = a%x/s; c%y = a%y/s; c%z = a%z/s
   end function vec3_scalar_div

   ! ─── Dot Product ─────────────────────────────────────────────────────
   !> Dot product: a . b
   pure function vec3_dot(a, b) result(d)
      type(vector3d_t), intent(in) :: a, b
      real(wp) :: d
      d = a%x*b%x + a%y*b%y + a%z*b%z
   end function vec3_dot

   ! ─── Cross Product ───────────────────────────────────────────────────
   !> Cross product: a x b
   !! Legacy equivalent: XX = Y13*Z42 - Z13*Y42, etc. (ANALGEO)
   pure function vec3_cross(a, b) result(c)
      type(vector3d_t), intent(in) :: a, b
      type(vector3d_t) :: c
      c%x = a%y*b%z - a%z*b%y
      c%y = a%z*b%x - a%x*b%z
      c%z = a%x*b%y - a%y*b%x
   end function vec3_cross

   ! ─── Norm / Normalize / Distance ─────────────────────────────────────
   !> Euclidean norm: |v|
   pure function vec3_norm(v) result(n)
      type(vector3d_t), intent(in) :: v
      real(wp) :: n
      n = sqrt(v%x**2 + v%y**2 + v%z**2)
   end function vec3_norm

   !> Return unit vector (normalized)
   pure function vec3_normalize(v) result(u)
      type(vector3d_t), intent(in) :: v
      type(vector3d_t) :: u
      real(wp) :: n
      n = sqrt(v%x**2 + v%y**2 + v%z**2)
      if (n > 0.0_wp) then
         u = v/n
      else
         u = vector3d_t(0.0_wp, 0.0_wp, 0.0_wp)
      end if
   end function vec3_normalize

   !> Distance between two points
   pure function vec3_distance(a, b) result(d)
      type(vector3d_t), intent(in) :: a, b
      real(wp) :: d
      d = norm(a - b)
   end function vec3_distance

   ! ─── Type-bound Procedures ───────────────────────────────────────────
   !> Magnitude (same as norm but as method)
   pure function vec3_magnitude(this) result(m)
      class(vector3d_t), intent(in) :: this
      real(wp) :: m
      m = sqrt(this%x**2 + this%y**2 + this%z**2)
   end function vec3_magnitude

   !> Unit vector (same as normalize but as method)
   pure function vec3_unit(this) result(u)
      class(vector3d_t), intent(in) :: this
      type(vector3d_t) :: u
      u = normalize(this)
   end function vec3_unit

   !> Convert to rank-1 array [x, y, z]
   pure function vec3_to_array(this) result(arr)
      class(vector3d_t), intent(in) :: this
      real(wp) :: arr(3)
      arr = [this%x, this%y, this%z]
   end function vec3_to_array

end module vector3d_mod
