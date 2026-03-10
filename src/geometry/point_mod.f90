module point_mod
   use base_kinds_mod, only: wp, ip
   implicit none

   private
   public :: point_t

   type :: point_t
      integer(ip) :: id = 0
      real(wp) :: coordinates(3) = 0.0_wp ! Default initialization
   contains
      procedure :: rotate => point_rotate
      procedure :: scale => point_scale
      procedure :: distance_to => point_distance_to
      procedure :: distance_to_origin => point_distance_to_origin
      procedure :: midpoint => point_midpoint
      procedure, private :: point_translate_xyz
      procedure, private :: point_translate_vector
      generic :: translate => point_translate_xyz, point_translate_vector
      procedure :: reflect_axis => point_reflect_axis
      procedure :: reflect_point => point_reflect_point
      procedure :: get_coordinates => point_get_coordinates
      procedure :: set_coordinates => point_set_coordinates
   end type point_t

   interface point_t
      module procedure point_constructor_xyz
      module procedure point_constructor_array
   end interface point_t

contains

   ! --- Constructor ---
   pure function point_constructor_xyz(id, x, y, z) result(new_point)
      integer(ip), intent(in), optional :: id
      real(wp), intent(in), optional :: x, y, z
      type(point_t) :: new_point

      if (present(id)) new_point%id = id
      if (present(x)) new_point%coordinates(1) = x
      if (present(y)) new_point%coordinates(2) = y
      if (present(z)) new_point%coordinates(3) = z
   end function point_constructor_xyz

   pure function point_constructor_array(id, coords) result(new_point)
      integer(ip), intent(in), optional :: id
      real(wp), intent(in) :: coords(3)
      type(point_t) :: new_point

      if (present(id)) new_point%id = id
      new_point%coordinates = coords
   end function point_constructor_array

   pure subroutine point_translate_xyz(this, dx, dy, dz)
      class(point_t), intent(inout) :: this
      real(wp), intent(in) :: dx, dy, dz
      this%coordinates = this%coordinates + [dx, dy, dz]
   end subroutine point_translate_xyz

   pure subroutine point_translate_vector(this, translation_vector)
      class(point_t), intent(inout) :: this
      real(wp), intent(in) :: translation_vector(3)
      this%coordinates = this%coordinates + translation_vector
   end subroutine point_translate_vector

   pure subroutine point_rotate(this, RotationMatrix)
      class(point_t), intent(inout) :: this
      real(wp), intent(in) :: RotationMatrix(3, 3)
      this%coordinates = matmul(RotationMatrix, this%coordinates)
   end subroutine point_rotate

   pure subroutine point_scale(this, scale_factor)
      class(point_t), intent(inout) :: this
      real(wp), intent(in) :: scale_factor
      this%coordinates = this%coordinates*scale_factor
   end subroutine point_scale

   pure subroutine point_reflect_axis(this, axis)
      class(point_t), intent(inout) :: this
      character(len=*), intent(in) :: axis

      select case (axis)
      case ('x', 'X')
         this%coordinates(2:3) = -this%coordinates(2:3)
      case ('y', 'Y')
         this%coordinates(1) = -this%coordinates(1)
         this%coordinates(3) = -this%coordinates(3)
      case ('z', 'Z')
         this%coordinates(1:2) = -this%coordinates(1:2)
      case default
         error stop "point_reflect_axis: Invalid axis provided."
      end select
   end subroutine point_reflect_axis

   pure subroutine point_reflect_point(this, point)
      class(point_t), intent(inout) :: this
      real(wp), intent(in) :: point(3)
      this%coordinates = 2.0_wp*point - this%coordinates
   end subroutine point_reflect_point

   pure function point_distance_to(this, other) result(dist)
      class(point_t), intent(in) :: this, other
      real(wp) :: dist
      ! Vectorized subtraction is much cleaner and faster
      dist = norm2(this%coordinates - other%coordinates)
   end function point_distance_to

   pure function point_distance_to_origin(this) result(dist)
      class(point_t), intent(in) :: this
      real(wp) :: dist
      dist = norm2(this%coordinates)
   end function point_distance_to_origin

   pure function point_midpoint(this, other) result(mid)
      class(point_t), intent(in) :: this, other
      type(point_t) :: mid
      mid%coordinates = 0.5_wp*(this%coordinates + other%coordinates)
   end function point_midpoint

   pure subroutine point_get_coordinates(this, x, y, z)
      class(point_t), intent(in) :: this
      real(wp), intent(out) :: x, y, z
      x = this%coordinates(1)
      y = this%coordinates(2)
      z = this%coordinates(3)
   end subroutine point_get_coordinates

   pure subroutine point_set_coordinates(this, x, y, z)
      class(point_t), intent(inout) :: this
      real(wp), intent(in) :: x, y, z
      this%coordinates = [x, y, z]
   end subroutine point_set_coordinates

end module point_mod
