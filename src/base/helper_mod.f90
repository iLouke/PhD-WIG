module helper_mod
   use base_kinds_mod
   implicit none
   private

   public :: real_to_char

contains

   !> Converts a real number to a string
   !> Usage: trim(real_to_char(my_val))
   function real_to_char(val, fmt) result(str)
      real(wp), intent(in) :: val
      character(len=*), intent(in), optional :: fmt
      character(len=32) :: str
      character(len=32) :: actual_fmt

      if (present(fmt)) then
         actual_fmt = fmt
      else
         actual_fmt = '(G12.6)' ! General format (scientific or fixed based on size)
      end if

      write (str, actual_fmt) val
      str = adjustl(str) ! Remove leading spaces
   end function real_to_char

end module helper_mod
