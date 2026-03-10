module mesh_mod
   use base_kinds_mod, only: wp, ip
   use panel_mod, only: panel_t
   use node_mod, only: node_t
   use point_mod, only: point_t
   use vector_mod, only: vector_t
   implicit none
   private

   public :: mesh_t

   interface mesh_t
      module procedure mesh_constructor
   end interface mesh_t

   type :: mesh_t
      integer(ip) :: num_nodes = 0
      integer(ip) :: num_panels = 0
      type(panel_t), allocatable :: panels(:)
   contains
      procedure :: rotate_origin => mesh_rotate_origin
      procedure :: translate => mesh_translate
   end type mesh_t

contains

   function mesh_constructor(points, points_filename, panels, panels_filename) result(new_mesh)
      type(mesh_t) :: new_mesh
      integer(ip), intent(in) :: points, panels
      character(len=*), intent(in) :: points_filename, panels_filename

      integer :: i, io_stat
      integer :: points_unit, panels_unit

      integer(ip) :: node_id, mark
      real(wp) :: x, y, z
      type(node_t), allocatable :: temp_nodes(:)

      integer(ip) :: panel_id, n1, n2, n3, n4
      real(wp) :: nx, ny, nz, cx, cy, cz, area
      type(node_t) :: p_nodes(4)

      if (points < 0_ip .or. panels < 0_ip) then
         error stop "mesh_constructor: number of points/panels must be non-negative."
      end if

      new_mesh%num_nodes = points
      new_mesh%num_panels = panels

      allocate (temp_nodes(points))
      allocate (new_mesh%panels(panels))

      open (newunit=points_unit, file=trim(points_filename), status='old', action='read', iostat=io_stat)
      if (io_stat /= 0) error stop "mesh_constructor: failed to open points file."

      do i = 1, points
         read (points_unit, *, iostat=io_stat) node_id, x, y, z, mark
         if (io_stat /= 0) then
            close (points_unit)
            error stop "mesh_constructor: invalid point entry."
         end if

         temp_nodes(i) = node_t(id=node_id, x=x, y=y, z=z, trailing_edge=(mark == 1))
      end do
      close (points_unit)

      open (newunit=panels_unit, file=trim(panels_filename), status='old', action='read', iostat=io_stat)
      if (io_stat /= 0) error stop "mesh_constructor: failed to open panels file."

      do i = 1, panels
         read (panels_unit, *, iostat=io_stat) panel_id, n1, n2, n3, n4, nx, ny, nz, cx, cy, cz, area
         if (io_stat /= 0) then
            close (panels_unit)
            error stop "mesh_constructor: invalid panel entry."
         end if

         p_nodes(1) = find_node(temp_nodes, n1)
         p_nodes(2) = find_node(temp_nodes, n2)
         p_nodes(3) = find_node(temp_nodes, n3)
         p_nodes(4) = find_node(temp_nodes, n4)

         new_mesh%panels(i) = panel_t(id=panel_id, &
                                      nodes=p_nodes, &
                                      cp=point_t(x=cx, y=cy, z=cz), &
                                      normal_in=vector_t(nx, ny, nz), &
                                      area=area)
      end do
      close (panels_unit)

   end function mesh_constructor

   pure function find_node(nodes_array, target_id) result(found)
      type(node_t), intent(in) :: nodes_array(:)
      integer(ip), intent(in) :: target_id
      type(node_t) :: found
      integer :: j

      if (target_id >= 1 .and. target_id <= size(nodes_array)) then
         if (nodes_array(target_id)%id == target_id) then
            found = nodes_array(target_id)
            return
         end if
      end if

      do j = 1, size(nodes_array)
         if (nodes_array(j)%id == target_id) then
            found = nodes_array(j)
            return
         end if
      end do

      error stop "mesh_mod: Node ID not found in temporary points array."
   end function find_node

   !> Rotate the entire mesh about the origin by 'angle' degrees (or radians if rads=.true.)
   !> along the given unit axis vector.
   subroutine mesh_rotate_origin(this, angle, axis, rads)
      class(mesh_t), intent(inout) :: this
      real(wp), intent(in) :: angle
      real(wp), intent(in) :: axis(3)
      logical, intent(in), optional :: rads
      integer :: i

      if (.not. allocated(this%panels)) return
      do i = 1, this%num_panels
         call this%panels(i)%rotate_origin(angle, axis, rads)
      end do
   end subroutine mesh_rotate_origin

   !> Translate the entire mesh by the given displacement vector.
   subroutine mesh_translate(this, vector)
      class(mesh_t), intent(inout) :: this
      real(wp), intent(in) :: vector(3)
      integer :: i

      if (.not. allocated(this%panels)) return
      do i = 1, this%num_panels
         call this%panels(i)%translate(vector)
      end do
   end subroutine mesh_translate

end module mesh_mod
