module mesh_mod
   use base_kinds_mod, only: wp, ip
   use panel_mod, only: panel_t
   implicit none

   type :: mesh_t
      integer :: num_panels
      type(panel_t), allocatable :: panels(:)
   contains
      procedure :: init => mesh_init
   end type mesh_t

contains

   subroutine mesh_init(this, num_panels)
      class(mesh_t), intent(inout) :: this
      integer, intent(in) :: num_panels

      this%num_panels = num_panels
      allocate (this%panels(num_panels))
   end subroutine mesh_init

end module mesh_mod
