module math_utils_mod
   use euler_mod
   use vector_mod
   use lapack_linalg_mod
   implicit none
   private

   ! Rotation Matrix Utilities
   public :: get_rotation_matrix
   public :: get_single_rotation_matrix
   public :: get_arbitrary_rotation_matrix
   ! Vector Utilities
   public :: cross_product, normalise
   ! Linear Algebra Utilities
   public :: solve_linear_system
   public :: factorize_matrix
   public :: solve_pre_factorized
   public :: solve_svd_least_squares
   public :: invert_matrix, inv

end module math_utils_mod
