module helper_mod
   use base_kinds_mod
   implicit none
   private

   public :: real_to_char, int_to_char, count_rows, print_matrix

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

   !> Converts an integer number to a string
   !> Usage: trim(int_to_char(my_val))
   function int_to_char(val, fmt) result(str)
      integer(ip), intent(in) :: val
      character(len=*), intent(in), optional :: fmt
      character(len=32) :: str
      character(len=32) :: actual_fmt

      if (present(fmt)) then
         actual_fmt = fmt
      else
         actual_fmt = '(I12)' ! Integer format (fixed width)
      end if

      write (str, actual_fmt) val
      str = adjustl(str) ! Remove leading spaces
   end function int_to_char

   function count_rows(filename) result(num_rows)
      character(len=*), intent(in) :: filename
      integer(ip) :: num_rows
      integer :: iunit, ios
      character(len=256) :: line

      num_rows = 0_ip
      open (newunit=iunit, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         num_rows = -1_ip ! Indicate error with -1
         return
      end if

      do
         read (iunit, '(A)', iostat=ios) line
         if (ios /= 0) exit
         num_rows = num_rows + 1_ip
      end do

      close (iunit)
   end function count_rows

   ! --- Helper: Print Matrix cleanly ---
   subroutine print_matrix(M)
      real(wp), intent(in) :: M(3, 3)
      integer :: i

      ! Print row by row with a fixed 10.4 format
      do i = 1, 3
         write (*, "(3(F10.4))") M(i, :)
      end do
      print *, "" ! Empty line for spacing
   end subroutine print_matrix

end module helper_mod
