module vehicle_mod
   use base_kinds_mod, only: wp, ip
   ! use mesh_mod, only: mesh_t
   ! use wake_mod, only: wake_t
   ! use dynamics_mod, only: state_t
   implicit none

   type :: vehicle_t
       integer :: id
      character(len=20) :: name
      ! type(mesh_t) :: mesh
      ! type(wake_t) :: wake
      ! type(state_t) :: state
   end type vehicle_t

end module vehicle_mod
