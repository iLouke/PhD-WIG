module types_mod
   use point_types_mod, only: point_t
   use panel_types_mod, only: panel_data_t
   use mesh_types_mod, only: mesh_data_t
   use wake_types_mod, only: wake_panel_t, wake_particle_t, wake_data_t
   implicit none
   private

   public :: point_t
   public :: panel_data_t
   public :: mesh_data_t
   public :: wake_panel_t
   public :: wake_particle_t
   public :: wake_data_t

end module types_mod
