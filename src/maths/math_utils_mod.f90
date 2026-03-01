module math_utils_mod
   use constants_mod
   use euler_mod
   use vector_mod
   use lapack_linalg_mod
   implicit none
   private

   public :: PI
   public :: get_rotation_matrix, get_single_rotation_matrix
   public :: cross_product, normalise
   public :: solve_linear_system, factorize_matrix, solve_pre_factorized, &
             solve_svd_least_squares, invert_matrix, inv

end module math_utils_mod
