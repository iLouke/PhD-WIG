module math_utils_mod
   use base_kinds_mod, only: wp, ip
   use vector_ops_mod, only: cross_product
   use lapack_linalg_mod, only: solve_linear_system, factorize_matrix, &
                                solve_pre_factorized, solve_svd_least_squares, &
                                invert_matrix, inv
   implicit none
   private

   public :: cross_product
   public :: solve_linear_system
   public :: factorize_matrix
   public :: solve_pre_factorized
   public :: solve_svd_least_squares
   public :: invert_matrix
   public :: inv

end module math_utils_mod
