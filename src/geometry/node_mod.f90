module node_mod
   use base_kinds_mod, only: wp, ip
   use point_mod, only: point_t
   implicit none

   private
   public :: node_t

   interface node_t
      module procedure node_constructor
   end interface node_t

   ! Define node_t as an extension of point_t
   type, extends(point_t) :: node_t
      logical :: is_trailing_edge = .false. ! Default to false
   contains
      procedure :: set_trailing_edge => node_set_trailing_edge
      procedure :: get_trailing_edge => node_get_trailing_edge
   end type node_t

contains

   ! --- Constructor ---
   pure function node_constructor(id, x, y, z, is_te) result(new_node)
      integer(ip), intent(in), optional :: id
      real(wp), intent(in), optional :: x, y, z
      logical, intent(in), optional :: is_te
      type(node_t) :: new_node

      ! Set inherited components
      if (present(id)) new_node%id = id
      if (present(x)) new_node%coordinates(1) = x
      if (present(y)) new_node%coordinates(2) = y
      if (present(z)) new_node%coordinates(3) = z

      ! Set the new child component
      if (present(is_te)) new_node%is_trailing_edge = is_te
   end function node_constructor

   ! --- Type-Bound Procedures ---
   pure subroutine node_set_trailing_edge(this, is_te)
      class(node_t), intent(inout) :: this
      logical, intent(in) :: is_te
      this%is_trailing_edge = is_te
   end subroutine node_set_trailing_edge

   pure function node_get_trailing_edge(this) result(is_te)
      class(node_t), intent(in) :: this
      logical :: is_te
      is_te = this%is_trailing_edge
   end function node_get_trailing_edge

end module node_mod
