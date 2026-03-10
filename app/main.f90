program phd
   use base_kinds_mod, only: wp, ip
   use sim_config_mod, only: sim_config_t
   use vehicle_config_mod, only: vehicle_config_t
   use vehicle_mod, only: vehicle_t
   use system_utils_mod, only: ensure_directory_exists
   implicit none

   type(sim_config_t) :: sim
   type(vehicle_config_t) :: vcfg
   type(vehicle_t) :: veh

   integer :: i, out_unit, dir_stat
   integer(ip) :: n_print

   ! Pitch (nose-up) rotation: 5 degrees about the Y-axis
   real(wp), parameter :: PITCH_DEG = 5.0_wp
   real(wp), parameter :: PITCH_AXIS(3) = [0.0_wp, 1.0_wp, 0.0_wp]

   ! ---------------------------------------------------------------
   ! 1. Load top-level simulation configuration
   ! ---------------------------------------------------------------
   print '(a)', "============================================"
   print '(a)', "   WIG Panel Method Solver — Geometry Init"
   print '(a)', "============================================"

   call sim%parse("input.toml")

   print '(a,a)', "Simulation : ", trim(sim%title)
   print '(a,f6.3)', "Time step  : ", sim%dt
   print '(a,i0)', "Vehicles   : ", sim%num_vehicles

   if (sim%num_vehicles < 1) then
      error stop "No vehicles defined in input.toml."
   end if

   ! ---------------------------------------------------------------
   ! 2. Load the first vehicle config and build the full hierarchy
   !    vehicle_t -> mesh_t -> panel_t(4 x node_t)
   ! ---------------------------------------------------------------
   print '(/,a,a)', "[Init] Loading vehicle config: ", trim(sim%vehicle_files(1))
   call vcfg%parse(sim%vehicle_files(1))

   print '(a,a)', "  Name   : ", trim(vcfg%name)
   print '(a,i0)', "  Nodes  : ", vcfg%points_number
   print '(a,i0)', "  Panels : ", vcfg%panels_number

   ! This single call drives the entire chain:
   !   vehicle_constructor_from_config
   !     -> mesh_t(points_filename, panels_filename)
   !         -> reads .dat files -> builds panel_t array -> stores node_t objects
   veh = vehicle_t(vcfg)

   print '(a)', "[OK] Full geometry hierarchy built."

   ! ---------------------------------------------------------------
   ! 3. Print sample panel/node coordinates — BEFORE rotation
   ! ---------------------------------------------------------------
   n_print = min(3_ip, veh%mesh%num_panels)

   print '(/,a)', "--- Sample panel geometry (pre-rotation) ---"
   do i = 1, n_print
      associate (p => veh%mesh%panels(i))
         print '(a,i0,a,f8.4)', "  Panel ", p%id, "  area=", p%area
         print '(a,3f10.4)', "    N1: ", p%nodes(1)%coordinates
         print '(a,3f10.4)', "    N2: ", p%nodes(2)%coordinates
         print '(a,3f10.4)', "    N3: ", p%nodes(3)%coordinates
         print '(a,3f10.4)', "    N4: ", p%nodes(4)%coordinates
         print '(a,3f10.4)', "    CP: ", p%center_point%coordinates
         print '(a,3f10.4)', "    Nv: ", p%normal%components
      end associate
   end do

   ! ---------------------------------------------------------------
   ! 4. Apply a 5-degree pitch (nose-up) rotation to the entire mesh
   ! ---------------------------------------------------------------
   print '(/,a,f4.1,a)', "[Rotate] Pitching vehicle by ", PITCH_DEG, " deg about Y-axis..."
   call veh%rotate_origin(PITCH_DEG, PITCH_AXIS)
   print '(a)', "[OK] Rotation applied to all panels and nodes."

   ! ---------------------------------------------------------------
   ! 5. Print same panels AFTER rotation to visually verify transform
   ! ---------------------------------------------------------------
   print '(/,a)', "--- Sample panel geometry (post-rotation) ---"
   do i = 1, n_print
      associate (p => veh%mesh%panels(i))
         print '(a,i0,a,f8.4)', "  Panel ", p%id, "  area=", p%area
         print '(a,3f10.4)', "    N1: ", p%nodes(1)%coordinates
         print '(a,3f10.4)', "    N2: ", p%nodes(2)%coordinates
         print '(a,3f10.4)', "    N3: ", p%nodes(3)%coordinates
         print '(a,3f10.4)', "    N4: ", p%nodes(4)%coordinates
         print '(a,3f10.4)', "    CP: ", p%center_point%coordinates
         print '(a,3f10.4)', "    Nv: ", p%normal%components
      end associate
   end do

   ! ---------------------------------------------------------------
   ! 6. Export the full rotated geometry to a structured text file
   ! ---------------------------------------------------------------
   call ensure_directory_exists("output", dir_stat)

   open (newunit=out_unit, file="output/geometry_rotated.dat", &
         status='replace', action='write')

   write (out_unit, '(a,a)') "# Vehicle: ", trim(veh%name)
   write (out_unit, '(a,f5.1,a)') "# Pitch rotation: ", PITCH_DEG, " degrees (Y-axis)"
   write (out_unit, '(a,i0)') "# Total panels: ", veh%mesh%num_panels
   write (out_unit, '(a)') "#"
   write (out_unit, '(a)') "# panel_id  node_slot  node_id      x           y           z"

   do i = 1, veh%mesh%num_panels
      associate (p => veh%mesh%panels(i))
         write (out_unit, '(2i8,i10,3f14.6)') p%id, 1, p%nodes(1)%id, p%nodes(1)%coordinates
         write (out_unit, '(2i8,i10,3f14.6)') p%id, 2, p%nodes(2)%id, p%nodes(2)%coordinates
         write (out_unit, '(2i8,i10,3f14.6)') p%id, 3, p%nodes(3)%id, p%nodes(3)%coordinates
         write (out_unit, '(2i8,i10,3f14.6)') p%id, 4, p%nodes(4)%id, p%nodes(4)%coordinates
         ! Blank line between panels for readability
         write (out_unit, '(a)') ""
      end associate
   end do
   close (out_unit)

   print '(/,a)', "[OK] Exported rotated geometry -> output/geometry_rotated.dat"
   print '(a)', "============================================"

end program phd
