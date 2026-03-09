module panel_mod
   use base_kinds_mod, only: wp, ip
   use math_constants_mod, only: SMALL, deg2rad
   use point_mod, only: point_t
   use node_mod, only: node_t
   use vector_mod, only: vector_t, operator(*), operator(.cross.)
   implicit none

   private
   public :: panel_t

   ! Interface for a custom structure constructor
   interface panel_t
      module procedure panel_constructor
   end interface panel_t

   type :: panel_t
      type(node_t) :: nodes(4)
      type(point_t) :: center_point
      type(vector_t) :: normal
      real(wp) :: area = 0.0_wp
      real(wp) :: max_diagonal = 0.0_wp
      logical :: is_triangle = .false.
   contains
      procedure :: rotate => panel_rotate
      procedure :: translate => panel_translate
   end type panel_t

contains

   ! --- Constructor ---
   pure function panel_constructor(n1, n2, n3, n4, cp, normal_in, area) result(new_panel)
      type(node_t), intent(in) :: n1, n2, n3, n4
      type(point_t), intent(in) :: cp
      type(vector_t), intent(in) :: normal_in
      real(wp), intent(in) :: area
      type(panel_t) :: new_panel

      type(vector_t) :: v12, v13, v14, v23, v24, cross1, cross2

      new_panel%nodes = [n1, n2, n3, n4]
      new_panel%center_point = cp
      new_panel%normal = normal_in%normalise()

      ! Detect if it's a triangle by checking distance between node 3 and 4
      new_panel%is_triangle = (n3%distance_to(n4) < SMALL)

      ! Calculate max diagonal length
      if (new_panel%is_triangle) then
         new_panel%max_diagonal = max(n1%distance_to(n2), n2%distance_to(n3), n3%distance_to(n1))
      else
         new_panel%max_diagonal = max(n1%distance_to(n3), n2%distance_to(n4))
      end if

      ! Assign or auto-compute area using vector operations
      if (area > SMALL) then
         new_panel%area = area
      else
         v12 = vector_t(n2%coordinates - n1%coordinates)
         v13 = vector_t(n3%coordinates - n1%coordinates)
         v14 = vector_t(n4%coordinates - n1%coordinates)
         v23 = vector_t(n3%coordinates - n2%coordinates)
         v24 = vector_t(n4%coordinates - n2%coordinates)

         if (new_panel%is_triangle) then
            cross1 = v12.cross.v13
            new_panel%area = 0.5_wp*cross1%magnitude()
         else
            cross1 = v12.cross.v14
            cross2 = v23.cross.v24
            new_panel%area = 0.5_wp*cross1%magnitude() + &
                             0.5_wp*cross2%magnitude()
         end if
      end if
   end function panel_constructor

   ! --- Type-Bound Procedures ---
   pure subroutine panel_rotate(this, angle, axis, rads)
      use euler_mod, only: get_arbitrary_rotation_matrix
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: angle
      real(wp), intent(in) :: axis(3)
      logical, intent(in), optional :: rads
      real(wp) :: R(3, 3)
      integer :: i

      R = get_arbitrary_rotation_matrix(angle, axis, rads)

      ! Delegate rotation to the underlying objects
      do i = 1, 4
         call this%nodes(i)%rotate(R)
      end do
      call this%center_point%rotate(R)
      call this%normal%rotate(R)
   end subroutine panel_rotate

   pure subroutine panel_translate(this, translation_vector)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: translation_vector(3)
      real(wp) :: dx, dy, dz
      integer :: i

      dx = translation_vector(1)
      dy = translation_vector(2)
      dz = translation_vector(3)

      ! Delegate translation to the underlying objects
      do i = 1, 4
         call this%nodes(i)%translate(dx, dy, dz)
      end do
      call this%center_point%translate(dx, dy, dz)
      ! Note: Normal vectors dictate direction, not position, so they are not translated.
   end subroutine panel_translate

end module panel_mod
