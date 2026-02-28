module lapack_linalg_mod
   use base_kinds_mod, only: wp, ip
   implicit none
   private

   public :: solve_linear_system
   public :: factorize_matrix
   public :: solve_pre_factorized
   public :: solve_svd_least_squares
   public :: invert_matrix
   public :: inv

   interface inv
      module procedure invert_matrix
   end interface

   interface
      subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
         import :: wp
         integer, intent(in) :: n, nrhs, lda, ldb
         real(wp), intent(inout) :: a(lda, *)
         integer, intent(out) :: ipiv(*)
         real(wp), intent(inout) :: b(ldb, *)
         integer, intent(out) :: info
      end subroutine dgesv

      subroutine dgetrf(m, n, a, lda, ipiv, info)
         import :: wp
         integer, intent(in) :: m, n, lda
         real(wp), intent(inout) :: a(lda, *)
         integer, intent(out) :: ipiv(*)
         integer, intent(out) :: info
      end subroutine dgetrf

      subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
         import :: wp
         character, intent(in) :: trans
         integer, intent(in) :: n, nrhs, lda, ldb
         real(wp), intent(in) :: a(lda, *)
         integer, intent(in) :: ipiv(*)
         real(wp), intent(inout) :: b(ldb, *)
         integer, intent(out) :: info
      end subroutine dgetrs

      subroutine dgelsd(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, &
                        work, lwork, iwork, info)
         import :: wp
         integer, intent(in) :: m, n, nrhs, lda, ldb, lwork
         real(wp), intent(inout) :: a(lda, *)
         real(wp), intent(inout) :: b(ldb, *)
         real(wp), intent(out) :: s(*)
         real(wp), intent(in) :: rcond
         integer, intent(out) :: rank
         real(wp), intent(out) :: work(*)
         integer, intent(out) :: iwork(*)
         integer, intent(out) :: info
      end subroutine dgelsd

      subroutine dgetri(n, a, lda, ipiv, work, lwork, info)
         import :: wp
         integer, intent(in) :: n, lda, lwork
         real(wp), intent(inout) :: a(lda, *)
         integer, intent(in) :: ipiv(*)
         real(wp), intent(inout) :: work(*)
         integer, intent(out) :: info
      end subroutine dgetri
   end interface

contains

   subroutine solve_linear_system(A, B, info)
      real(wp), intent(inout) :: A(:, :)
      real(wp), intent(inout) :: B(:)
      integer, intent(out) :: info
      integer :: n
      integer, allocatable :: ipiv(:)

      n = size(A, 1)
      allocate (ipiv(n))
      call dgesv(n, 1, A, n, ipiv, B, n, info)
      deallocate (ipiv)
   end subroutine solve_linear_system

   subroutine factorize_matrix(A, ipiv, info)
      real(wp), intent(inout) :: A(:, :)
      integer, intent(out) :: ipiv(:)
      integer, intent(out) :: info
      integer :: n

      n = size(A, 1)
      call dgetrf(n, n, A, n, ipiv, info)
   end subroutine factorize_matrix

   subroutine solve_pre_factorized(A_lu, ipiv, B, info)
      real(wp), intent(in) :: A_lu(:, :)
      integer, intent(in) :: ipiv(:)
      real(wp), intent(inout) :: B(:)
      integer, intent(out) :: info
      integer :: n

      n = size(A_lu, 1)
      call dgetrs('N', n, 1, A_lu, n, ipiv, B, n, info)
   end subroutine solve_pre_factorized

   subroutine solve_svd_least_squares(A, B, rcond, info)
      real(wp), intent(inout) :: A(:, :)
      real(wp), intent(inout) :: B(:)
      real(wp), intent(in) :: rcond
      integer, intent(out) :: info
      integer :: n, lwork, rank
      real(wp), allocatable :: s(:), work(:)
      integer, allocatable :: iwork(:)
      real(wp) :: work_query(1)
      integer :: iwork_query(1)

      n = size(A, 1)
      allocate (s(n))

      call dgelsd(n, n, 1, A, n, B, n, s, rcond, rank, &
                  work_query, -1, iwork_query, info)

      lwork = int(work_query(1))
      allocate (work(lwork))
      allocate (iwork(iwork_query(1)))

      call dgelsd(n, n, 1, A, n, B, n, s, rcond, rank, &
                  work, lwork, iwork, info)

      deallocate (s, work, iwork)
   end subroutine solve_svd_least_squares

   subroutine invert_matrix(A, A_inv, status)
      real(wp), intent(in) :: A(:, :)
      real(wp), intent(out) :: A_inv(:, :)
      integer(ip), intent(out) :: status

      integer :: n, info, lwork
      integer, allocatable :: ipiv(:)
      real(wp), allocatable :: work(:)
      real(wp) :: work_query(1)

      n = size(A, 1)
      if (size(A, 2) /= n) then
         status = -1_ip
         return
      end if

      A_inv = A
      allocate (ipiv(n))

      call dgetrf(n, n, A_inv, n, ipiv, info)
      if (info /= 0) then
         status = int(info, ip)
         deallocate (ipiv)
         return
      end if

      call dgetri(n, A_inv, n, ipiv, work_query, -1, info)
      lwork = max(1, int(work_query(1)))
      allocate (work(lwork))

      call dgetri(n, A_inv, n, ipiv, work, lwork, info)
      status = int(info, ip)

      deallocate (work, ipiv)
   end subroutine invert_matrix

end module lapack_linalg_mod
