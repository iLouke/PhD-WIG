module panel_mod
   use base_kinds_mod, only: wp, ip
   use math_utils_mod, only: normalise
   use math_constants_mod, only: SMALL, deg2rad
   implicit none

   public :: panel_t

   type :: panel_t
      real(wp) :: node(4, 3)     ! 4 nodes, 3 coordinates each
      real(wp) :: center_point(3)
      real(wp) :: normal(3)
      real(wp) :: area
      real(wp) :: max_diagonal   ! For far field criteria
      logical  :: is_triangle    ! If p3 == p4, it's a triangle panel
   contains
      procedure :: initialize => panel_initialize
      procedure :: rotate => panel_rotate
      procedure :: translate => panel_translate
   end type panel_t

contains
   pure subroutine panel_initialize(this, p1, p2, p3, p4, cp, normal, area)
      use math_utils_mod, only: cross_product
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: p1(3), p2(3), p3(3), p4(3)
      real(wp), intent(in) :: cp(3), normal(3), area

      this%node(1, :) = p1
      this%node(2, :) = p2
      this%node(3, :) = p3
      this%node(4, :) = p4
      this%center_point = cp
      this%normal = normalise(normal) ! Ensure normal is a unit vector
      this%area = area
      this%is_triangle = (norm2(p3 - p4) < SMALL) ! If p3 and p4 are the same point, it's a triangle
      ! Calculate max diagonal length for far field criteria
      if (this%is_triangle) then
         this%max_diagonal = max(norm2(p1 - p2), norm2(p2 - p3), norm2(p3 - p1)) ! For triangle, use longest edge as "diagonal"
      else
         this%max_diagonal = max(norm2(p1 - p3), norm2(p2 - p4)) ! For quadrilateral, use longest diagonal
      end if
      ! Check if area is positive and greater that zero, else calculate it
      if (area > SMALL) then
         this%area = area
      else
         if (this%is_triangle) then
            this%area = 0.5_wp*norm2(cross_product(p2 - p1, p3 - p1)) ! Area of triangle
         else
            this%area = 0.5_wp*norm2(cross_product(p2 - p1, p4 - p1)) + &
                        0.5_wp*norm2(cross_product(p3 - p2, p4 - p2)) ! Area of quadrilateral as sum of two triangles
         end if
      end if
   end subroutine panel_initialize

   subroutine panel_rotate(this, angle, axis, rads)
      use euler_mod, only: get_arbitrary_rotation_matrix
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: angle
      real(wp), intent(in) :: axis(3)
      logical, intent(in), optional :: rads
      real(wp) :: R(3, 3)
      integer :: i

      R = get_arbitrary_rotation_matrix(angle, axis, rads)

      ! Apply rotation to each node
      do i = 1, 4
         this%node(i, :) = matmul(R, this%node(i, :))
      end do

      ! Update center point and normal vector
      this%center_point = matmul(R, this%center_point)
      this%normal = matmul(R, this%normal)
   end subroutine panel_rotate

   pure subroutine panel_translate(this, translation_vector)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: translation_vector(3)
      integer :: i
      do i = 1, 4
         this%node(i, :) = this%node(i, :) + translation_vector(:)
      end do

      this%center_point = this%center_point + translation_vector(:)
   end subroutine panel_translate

end module panel_mod
