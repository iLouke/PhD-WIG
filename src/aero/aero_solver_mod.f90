module aero_solver_mod
   !! Linear Algebra Solver for Aerodynamic Systems
   !!
   !! Wraps LAPACK/FlexiBLAS routines for solving the AIC (Aerodynamic
   !! Influence Coefficient) linear system: A * gamma = b
   !!
   !! Legacy mapping:
   !!   SUBROUTINE SVDCMP  ->  Replaced by LAPACK DGETRF (LU factorization)
   !!   SUBROUTINE SVDBK   ->  Replaced by LAPACK DGETRS (LU back-substitution)
   !!   FUNCTION PYTHAG     ->  No longer needed (LAPACK handles internally)
   !!
   !! The legacy code used a hand-coded SVD (Singular Value Decomposition)
   !! with a small-value threshold for regularization. The modern version
   !! uses LAPACK's LU factorization (DGETRF/DGETRS) which is:
   !!   - Much faster (O(2/3 N³) vs O(4 N³) for SVD)
   !!   - Numerically robust (partial pivoting)
   !!   - Optimized by FlexiBLAS backend (OpenBLAS, MKL, etc.)
   !!
   !! For the iterative UVLM workflow:
   !!   1. First iteration: factorize AIC matrix (DGETRF) — stored for reuse
   !!   2. Each iteration: solve with new RHS (DGETRS) — very fast
   !!
   !! An SVD fallback (DGELSD) is provided for ill-conditioned systems.
   use base_kinds_mod, only: wp, ip
   implicit none
   private

   public :: aero_linsys_t

   ! ─── LAPACK Interface Block ──────────────────────────────────────────
   interface
      ! LU Factorization: A = P * L * U
      subroutine dgetrf(m, n, a, lda, ipiv, info)
         import :: wp
         integer, intent(in)    :: m, n, lda
         real(wp), intent(inout) :: a(lda, *)
         integer, intent(out)   :: ipiv(*)
         integer, intent(out)   :: info
      end subroutine dgetrf

      ! Solve using LU factors: A * X = B (A already factorized)
      subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
         import :: wp
         character, intent(in)  :: trans
         integer, intent(in)    :: n, nrhs, lda, ldb
         real(wp), intent(in)   :: a(lda, *)
         integer, intent(in)    :: ipiv(*)
         real(wp), intent(inout) :: b(ldb, *)
         integer, intent(out)   :: info
      end subroutine dgetrs

      ! One-shot LU solve: A * X = B
      subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
         import :: wp
         integer, intent(in)    :: n, nrhs, lda, ldb
         real(wp), intent(inout) :: a(lda, *)
         integer, intent(out)   :: ipiv(*)
         real(wp), intent(inout) :: b(ldb, *)
         integer, intent(out)   :: info
      end subroutine dgesv

      ! SVD Least-Squares solve: min |A*X - B| (for ill-conditioned systems)
      subroutine dgelsd(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, &
                        work, lwork, iwork, info)
         import :: wp
         integer, intent(in)    :: m, n, nrhs, lda, ldb, lwork
         real(wp), intent(inout) :: a(lda, *)
         real(wp), intent(inout) :: b(ldb, *)
         real(wp), intent(out)  :: s(*)
         real(wp), intent(in)   :: rcond
         integer, intent(out)   :: rank
         real(wp), intent(out)  :: work(*)
         integer, intent(out)   :: iwork(*)
         integer, intent(out)   :: info
      end subroutine dgelsd
   end interface

   ! ─── Type Definition ─────────────────────────────────────────────────
   type :: aero_linsys_t
      private
      integer(ip) :: n = 0                          !! System size
      real(wp), allocatable :: A_lu(:, :)            !! LU-factorized AIC matrix
      integer, allocatable  :: ipiv(:)               !! Pivot indices from DGETRF
      logical :: factorized = .false.                !! Has the matrix been factorized?
   contains
      procedure :: factorize => linsys_factorize
      procedure :: solve => linsys_solve
      procedure :: solve_svd => linsys_solve_svd
      procedure :: is_ready => linsys_is_ready
      procedure :: get_size => linsys_get_size
      procedure :: destroy => linsys_destroy
   end type aero_linsys_t

contains

   !> Factorize the AIC matrix using LAPACK DGETRF (LU decomposition)
   !!
   !! Legacy equivalent: CALL SVDCMP(N) in SVDBK
   !! The factorized matrix is stored internally for repeated solves.
   !!
   !! @param A    The AIC influence matrix (N x N). DESTROYED on output
   !!             (overwritten by LU factors, so pass a copy if needed).
   !! @param info LAPACK status: 0=success, >0=singular, <0=argument error
   subroutine linsys_factorize(this, A, info)
      class(aero_linsys_t), intent(inout) :: this
      real(wp), intent(inout) :: A(:, :)
      integer, intent(out) :: info
      integer :: n

      n = size(A, 1)
      this%n = n

      ! Allocate storage
      if (allocated(this%A_lu)) deallocate (this%A_lu)
      if (allocated(this%ipiv)) deallocate (this%ipiv)
      allocate (this%A_lu(n, n))
      allocate (this%ipiv(n))

      ! Copy matrix (DGETRF overwrites input)
      this%A_lu = A

      ! LU factorize via LAPACK
      call dgetrf(n, n, this%A_lu, n, this%ipiv, info)

      this%factorized = (info == 0)
   end subroutine linsys_factorize

   !> Solve the linear system A * x = b using pre-computed LU factors
   !!
   !! Legacy equivalent: The solve portion of SVDBK
   !! This is called every iteration with a new RHS (wake contribution changes).
   !!
   !! @param b    Right-hand side vector (overwritten with solution x on output)
   !! @param info LAPACK status: 0=success
   subroutine linsys_solve(this, b, info)
      class(aero_linsys_t), intent(in) :: this
      real(wp), intent(inout) :: b(:)
      integer, intent(out) :: info

      if (.not. this%factorized) then
         info = -999  ! Not factorized yet
         return
      end if

      ! Solve using LU factors: A_lu * x = b
      call dgetrs('N', this%n, 1, this%A_lu, this%n, this%ipiv, b, this%n, info)
   end subroutine linsys_solve

   !> SVD least-squares solve (fallback for ill-conditioned systems)
   !!
   !! Uses LAPACK DGELSD with threshold-based rank truncation.
   !! This is closer to what the legacy SVD code did with WMIN thresholding.
   !!
   !! @param A     The AIC matrix (N x N). DESTROYED on output.
   !! @param b     RHS vector (overwritten with solution on output)
   !! @param rcond Reciprocal condition number threshold (legacy: 1e-12)
   !! @param info  LAPACK status
   subroutine linsys_solve_svd(this, A, b, rcond, info)
      class(aero_linsys_t), intent(inout) :: this
      real(wp), intent(inout) :: A(:, :)
      real(wp), intent(inout) :: b(:)
      real(wp), intent(in)    :: rcond
      integer, intent(out)    :: info

      integer :: n, lwork, rank
      real(wp), allocatable :: s(:), work(:)
      integer, allocatable  :: iwork(:)
      real(wp) :: work_query(1)
      integer  :: iwork_query(1)

      n = size(A, 1)
      this%n = n
      allocate (s(n))

      ! Workspace query
      call dgelsd(n, n, 1, A, n, b, n, s, rcond, rank, &
                  work_query, -1, iwork_query, info)

      lwork = int(work_query(1))
      allocate (work(lwork))
      allocate (iwork(iwork_query(1)))

      ! Actual solve
      call dgelsd(n, n, 1, A, n, b, n, s, rcond, rank, &
                  work, lwork, iwork, info)

      deallocate (s, work, iwork)
      this%factorized = .false.  ! SVD does not store factors for reuse
   end subroutine linsys_solve_svd

   !> Check if the system has been factorized and is ready for solves
   pure function linsys_is_ready(this) result(ready)
      class(aero_linsys_t), intent(in) :: this
      logical :: ready
      ready = this%factorized
   end function linsys_is_ready

   !> Get system size
   pure function linsys_get_size(this) result(n)
      class(aero_linsys_t), intent(in) :: this
      integer(ip) :: n
      n = this%n
   end function linsys_get_size

   !> Clean up
   subroutine linsys_destroy(this)
      class(aero_linsys_t), intent(inout) :: this
      if (allocated(this%A_lu)) deallocate (this%A_lu)
      if (allocated(this%ipiv)) deallocate (this%ipiv)
      this%factorized = .false.
      this%n = 0
   end subroutine linsys_destroy

end module aero_solver_mod
