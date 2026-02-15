module base_kinds_mod
    use iso_fortran_env, only: real64, real128, int32, int64
    implicit none
    private
    public :: wp, ip

    ! Working Precision: real64 (Double). Change to real128 for Quad.
    integer, parameter :: wp = real64
    ! Integer Precision: 32-bit standard, 64-bit for massive meshes.
    integer, parameter :: ip = int32
    
end module base_kinds_mod
