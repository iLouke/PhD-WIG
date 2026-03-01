module panel_mod
   use base_kinds_mod, only: wp, ip
   implicit none

   type :: panel_t
      real(wp) :: node(4, 3) ! 4 nodes, 3 coordinates each
      real(wp) :: normal(3)
      real(wp) :: area
      real(wp) :: max_diagonal ! For far field criteria

   contains
      procedure :: rotate => panel_rotate
      procedure :: translate => panel_translate
   end type panel_t

contains
   subroutine panel_rotate(this, angle, axis)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: angle
      character(len=*), intent(in) :: axis

      ! Placeholder for rotation logic
   end subroutine panel_rotate

   subroutine panel_translate(this, translation_vector)
      class(panel_t), intent(inout) :: this
      real(wp), intent(in) :: translation_vector(3)

      ! Placeholder for translation logic
   end subroutine panel_translate

end module panel_mod
