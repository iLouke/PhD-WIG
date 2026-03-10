module panel_mod
   use base_kinds_mod, only: wp, ip
   use math_constants_mod, only: SMALL, deg2rad
   use point_mod, only: point_t
   use node_mod, only: node_t
   use vector_mod, only: vector_t, operator(*), operator(.x.)
   implicit none

   private
   public :: panel_t

   interface panel_t
      module procedure panel_constructor
   end interface panel_t

   type :: panel_t
      integer(ip) :: id = 0
      type(node_t) :: nodes(4)
      type(point_t) :: center_point
      type(vector_t) :: normal
      real(wp) :: area = 0.0_wp
      real(wp) :: max_diagonal = 0.0_wp
      logical :: is_triangle = .false.
   contains
      procedure :: rotate_origin => panel_rotate_origin
      procedure :: translate => panel_translate
   end type panel_t

contains

   ! --- Constructor ---
   pure function panel_constructor(id, nodes, cp, normal_in, area) result(new_panel)
      integer(ip), intent(in) :: id
      type(node_t), intent(in) :: nodes(4)
      type(point_t), intent(in) :: cp
      type(vector_t), intent(in) :: normal_in
      real(wp), intent(in) :: area
      type(panel_t) :: new_panel

      type(vector_t) :: v12, v13, v14, v23, v24, cross1, cross2

      new_panel%nodes = nodes
      new_panel%center_point = cp
      new_panel%id = id
      new_panel%normal = normal_in%normalise()

      ! Detect if it's a triangle by checking distance between node 3 and 4
      new_panel%is_triangle = (nodes(3)%distance_to(nodes(4)) < SMALL)

      ! Calculate max diagonal length
      if (new_panel%is_triangle) then
        new_panel%max_diagonal = max(nodes(1)%distance_to(nodes(2)), nodes(2)%distance_to(nodes(3)), nodes(3)%distance_to(nodes(1)))
      else
         new_panel%max_diagonal = max(nodes(1)%distance_to(nodes(3)), nodes(2)%distance_to(nodes(4)))
      end if

      ! Assign or auto-compute area using vector operations
      if (area > SMALL) then
         new_panel%area = area
      else
         v12 = vector_t(nodes(2)%coordinates - nodes(1)%coordinates)
         v13 = vector_t(nodes(3)%coordinates - nodes(1)%coordinates)
         v14 = vector_t(nodes(4)%coordinates - nodes(1)%coordinates)
         v23 = vector_t(nodes(3)%coordinates - nodes(2)%coordinates)
         v24 = vector_t(nodes(4)%coordinates - nodes(2)%coordinates)

         if (new_panel%is_triangle) then
            cross1 = v12.x.v13
            new_panel%area = 0.5_wp*cross1%magnitude()
         else
            cross1 = v12.x.v14
            cross2 = v23.x.v24
            new_panel%area = 0.5_wp*cross1%magnitude() + &
                             0.5_wp*cross2%magnitude()
         end if
      end if
   end function panel_constructor

   ! --- Type-Bound Procedures ---
   pure subroutine panel_rotate_origin(this, angle, axis, rads)
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
   end subroutine panel_rotate_origin

   pure subroutine panel_rotate_remote(this, point, vector, angle, rads)
      use euler_mod, only: get_arbitrary_rotation_matrix
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: point(3)
      real(wp), intent(in) :: angle
      real(wp), intent(in) :: vector(3)
      logical, intent(in), optional :: rads
      real(wp) :: R(3, 3)
      integer :: i

      R = get_arbitrary_rotation_matrix(angle, vector, rads)

      ! Translate to origin, rotate, then translate back
      call this%translate(-point)
      call this%center_point%translate(-point)
      ! Delegate rotation to the underlying objects
      do i = 1, 4
         call this%nodes(i)%rotate(R)
      end do
      call this%center_point%rotate(R)
      call this%normal%rotate(R)
   end subroutine panel_rotate_remote

   pure subroutine panel_translate(this, vector)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: vector(3)
      integer :: i

      ! Delegate translation to the underlying objects
      do i = 1, 4
         call this%nodes(i)%translate(vector)
      end do
      call this%center_point%translate(vector)
   end subroutine panel_translate

end module panel_mod
