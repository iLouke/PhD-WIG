module math_utils_mod
    use base_kinds_mod, only: wp, ip
    implicit none
    private
    
    public :: cross_product
    public :: solve_linear_system
    public :: factorize_matrix
    public :: solve_pre_factorized

    ! --- LAPACK INTERFACE BLOCK ---
    interface
        ! One-shot solver (LU + Solve)
        subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
            import :: wp
            integer, intent(in) :: n, nrhs, lda, ldb
            real(wp), intent(inout) :: a(lda, *)
            integer, intent(out) :: ipiv(*)
            real(wp), intent(inout) :: b(ldb, *)
            integer, intent(out) :: info
        end subroutine dgesv

        ! LU Factorization only
        subroutine dgetrf(m, n, a, lda, ipiv, info)
            import :: wp
            integer, intent(in) :: m, n, lda
            real(wp), intent(inout) :: a(lda, *)
            integer, intent(out) :: ipiv(*)
            integer, intent(out) :: info
        end subroutine dgetrf

        ! Solve using existing LU factors
        subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
            import :: wp
            character, intent(in) :: trans
            integer, intent(in) :: n, nrhs, lda, ldb
            real(wp), intent(in) :: a(lda, *)
            integer, intent(in) :: ipiv(*)
            real(wp), intent(inout) :: b(ldb, *)
            integer, intent(out) :: info
        end subroutine dgetrs
    end interface

contains

    !> Computes the cross product of two 3D vectors: c = a x b
    pure function cross_product(a, b) result(c)
        real(wp), intent(in) :: a(3), b(3)
        real(wp) :: c(3)
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)
    end function cross_product

    !> ONE-SHOT SOLVE (A * X = B)
    !> Good for simple steady-state VLM steps.
    subroutine solve_linear_system(A, B, info)
        real(wp), intent(inout) :: A(:,:)
        real(wp), intent(inout) :: B(:)
        integer, intent(out)    :: info
        integer :: n
        integer, allocatable :: ipiv(:)

        n = size(A, 1)
        allocate(ipiv(n))
        call dgesv(n, 1, A, n, ipiv, B, n, info)
        deallocate(ipiv)
    end subroutine solve_linear_system

    !> STEP 1: FACTORIZE (A = L * U)
    !> Call this once when the geometry/AIC matrix is generated.
    !> Note: ipiv should be stored in Body or Solver object
    !> ipiv is essential for the system solution
    subroutine factorize_matrix(A, ipiv, info)
        real(wp), intent(inout) :: A(:,:)      !< Matrix A (overwritten by LU factors)
        integer, intent(out)    :: ipiv(:)     !< Pivot indices (must be sized N)
        integer, intent(out)    :: info        !< Status (0 = Success)
        integer :: n
        
        n = size(A, 1)
        call dgetrf(n, n, A, n, ipiv, info)
    end subroutine factorize_matrix

    !> STEP 2: SOLVE PRE-FACTORIZED (L * U * X = B)
    !> Call this multiple times with new RHS vectors B.
    subroutine solve_pre_factorized(A_lu, ipiv, B, info)
        real(wp), intent(in)    :: A_lu(:,:)   !< Pre-factorized LU matrix
        integer, intent(in)     :: ipiv(:)     !< Pivot indices from Step 1
        real(wp), intent(inout) :: B(:)        !< RHS vector (overwritten by solution X)
        integer, intent(out)    :: info        !< Status
        integer :: n
        
        n = size(A_lu, 1)
        call dgetrs('N', n, 1, A_lu, n, ipiv, B, n, info)
    end subroutine solve_pre_factorized

end module math_utils_mod
