module panel_mod
   use base_kinds_mod, only: wp, ip
   use constants_mod, only: SMALL
   use math_utils_mod, only: normalise
   implicit none

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
   subroutine panel_initialize(this, p1, p2, p3, p4, cp, normal, area)
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
      this%is_triangle = (norm2(p3 - p4) < SMALL)
      ! Calculate max diagonal length for far field criteria
      if (this%is_triangle) then
         this%max_diagonal = max(norm2(p1 - p2), norm2(p2 - p3), norm2(p3 - p1)) ! For triangle, use longest edge as "diagonal"
      else
         this%max_diagonal = max(norm2(p1 - p4), norm2(p2 - p3)) ! For quadrilateral, use longest diagonal
      end if
   end subroutine panel_initialize

   subroutine panel_rotate(this, angle, axis)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: angle
      character(len=*), intent(in) :: axis

      ! Placeholder for rotation logic
   end subroutine panel_rotate

   subroutine panel_translate(this, translation_vector)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: translation_vector(3)
      integer :: i
      do i = 1, 4
         this%node(i, :) = this%node(i, :) + translation_vector(:)
      end do

      this%center_point = this%center_point + translation_vector(:)
   end subroutine panel_translate

end module panel_mod
