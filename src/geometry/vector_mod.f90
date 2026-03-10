module vector_mod
   use base_kinds_mod, only: wp
   implicit none
   private

   public :: vector_t
   ! Export overloaded operators so other modules can use standard math symbols
   public :: operator(+), operator(-), operator(*)
   public :: operator(.x.)

   interface vector_t
      module procedure constructor_xyz
      module procedure constructor_array
   end interface

   type :: vector_t
      real(wp) :: components(3) = 0.0_wp
   contains
      procedure :: normalise => vector_normalise
      procedure :: magnitude => vector_magnitude
      procedure :: rotate => vector_rotate
   end type vector_t

   ! --- Operator Overloading Interfaces ---
   interface operator(+)
      module procedure vector_add
   end interface

   interface operator(-)
      module procedure vector_sub
   end interface

   interface operator(*)
      module procedure vector_scalar_mult
      module procedure scalar_vector_mult
      module procedure vector_dot_product
   end interface

   interface operator(.x.)
      module procedure vector_cross_product
   end interface

contains

   ! --- Constructors ---
   pure function constructor_xyz(x, y, z) result(v)
      real(wp), intent(in) :: x, y, z
      type(vector_t) :: v
      v%components = [x, y, z]
   end function constructor_xyz

   pure function constructor_array(arr) result(v)
      real(wp), intent(in) :: arr(3)
      type(vector_t) :: v
      v%components = arr
   end function constructor_array

   ! --- Type-Bound Procedures ---
   pure function vector_magnitude(this) result(mag)
      class(vector_t), intent(in) :: this
      real(wp) :: mag
      mag = norm2(this%components)
   end function vector_magnitude

   pure function vector_normalise(this) result(unit_v)
      class(vector_t), intent(in) :: this
      type(vector_t) :: unit_v
      real(wp) :: mag

      mag = this%magnitude()
      ! Safety check to prevent division by zero (NaN generation)
      if (mag > 1.0e-14_wp) then
         unit_v%components = this%components/mag
      else
         unit_v%components = 0.0_wp
      end if
   end function vector_normalise

   pure subroutine vector_rotate(this, R)
      class(vector_t), intent(inout) :: this
      real(wp), intent(in) :: R(3, 3)
      this%components = matmul(R, this%components)
   end subroutine vector_rotate

   ! --- Operator Implementations ---
   pure function vector_add(v1, v2) result(v_sum)
      type(vector_t), intent(in) :: v1, v2
      type(vector_t) :: v_sum
      v_sum%components = v1%components + v2%components
   end function vector_add

   pure function vector_sub(v1, v2) result(v_diff)
      type(vector_t), intent(in) :: v1, v2
      type(vector_t) :: v_diff
      v_diff%components = v1%components - v2%components
   end function vector_sub

   pure function vector_scalar_mult(v, scalar) result(v_scaled)
      type(vector_t), intent(in) :: v
      real(wp), intent(in) :: scalar
      type(vector_t) :: v_scaled
      v_scaled%components = v%components*scalar
   end function vector_scalar_mult

   pure function scalar_vector_mult(scalar, v) result(v_scaled)
      real(wp), intent(in) :: scalar
      type(vector_t), intent(in) :: v
      type(vector_t) :: v_scaled
      v_scaled%components = scalar*v%components
   end function scalar_vector_mult

   pure function vector_dot_product(v1, v2) result(dot)
      type(vector_t), intent(in) :: v1, v2
      real(wp) :: dot
      dot = dot_product(v1%components, v2%components) ! Using Fortran intrinsic
   end function vector_dot_product

   pure function vector_cross_product(v1, v2) result(cross)
      type(vector_t), intent(in) :: v1, v2
      type(vector_t) :: cross
      cross%components(1) = v1%components(2)*v2%components(3) - v1%components(3)*v2%components(2)
      cross%components(2) = v1%components(3)*v2%components(1) - v1%components(1)*v2%components(3)
      cross%components(3) = v1%components(1)*v2%components(2) - v1%components(2)*v2%components(1)
   end function vector_cross_product

end module vector_mod
