module math_utils_mod
   use base_kinds_mod, only: wp, ip
   use constants_mod, only: deg2rad
   use euler_rotations_mod, only: get_rotation_matrix, get_single_rotation_matrix
   use vector_mod, only: cross_product
   use lapack_linalg_mod, only: solve_linear_system, factorize_matrix, &
                                solve_pre_factorized, solve_svd_least_squares, &
                                invert_matrix, inv
   implicit none
   private

   public :: deg2rad
   public :: get_rotation_matrix, get_single_rotation_matrix
   public :: cross_product
   public :: solve_linear_system, factorize_matrix, solve_pre_factorized, &
             solve_svd_least_squares, invert_matrix, inv

end module math_utils_mod
