program test_panel
   use base_kinds_mod, only: wp, ip
   use point_mod, only: point_t
   use node_mod, only: node_t
   use vector_mod, only: vector_t
   use panel_mod, only: panel_t
   use plotting_mod, only: plotter_t
   implicit none

   integer :: fail_count = 0
   real(wp), parameter :: TOL = 1.0e-10_wp

   print *, "=========================================="
   print *, "   RUNNING PANEL MODULE TESTS             "
   print *, "=========================================="

   call test_panel_initialize_quadrilateral()
   call test_panel_initialize_triangle()
   call test_panel_auto_area_computation()
   call test_panel_rotate_and_translate()
   call test_panel_rotation_visualization_output()
   call test_panel_translation_visualization_output()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All panel tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   subroutine test_panel_initialize_quadrilateral()
      type(panel_t) :: panel
      type(node_t) :: n1, n2, n3, n4
      type(point_t) :: cp
      type(vector_t) :: normal_vec

      print *, "Testing panel initialization (quadrilateral)..."

      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=1.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      cp = point_t(x=0.5_wp, y=0.5_wp, z=0.0_wp)
      normal_vec = vector_t(0.0_wp, 0.0_wp, 2.0_wp)

      ! Περνάμε το ID και τους κόμβους ως πίνακα
      panel = panel_t(1_ip, [n1, n2, n3, n4], cp, normal_vec, 1.0_wp)

      call assert(.not. panel%is_triangle, "Quadrilateral panel is not flagged as triangle")
      call assert(abs(panel%area - 1.0_wp) < TOL, "Quadrilateral area stored correctly")
      call assert_vector_close(panel%normal%components, [0.0_wp, 0.0_wp, 1.0_wp], "Normal is normalized")
      call assert(abs(panel%max_diagonal - sqrt(2.0_wp)) < TOL, "Quadrilateral max diagonal is correct")
   end subroutine test_panel_initialize_quadrilateral

   subroutine test_panel_initialize_triangle()
      type(panel_t) :: panel
      type(node_t) :: n1, n2, n3, n4
      type(point_t) :: cp
      type(vector_t) :: normal_vec

      print *, "Testing panel initialization (triangle)..."

      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = n3
      cp = point_t(x=1.0_wp/3.0_wp, y=1.0_wp/3.0_wp, z=0.0_wp)
      normal_vec = vector_t(0.0_wp, 0.0_wp, 1.0_wp)

      panel = panel_t(2_ip, [n1, n2, n3, n4], cp, normal_vec, 0.5_wp)

      call assert(panel%is_triangle, "Triangle panel is detected when n3 == n4")
      call assert(abs(panel%max_diagonal - sqrt(2.0_wp)) < TOL, "Triangle max edge length is correct")
   end subroutine test_panel_initialize_triangle

   subroutine test_panel_auto_area_computation()
      type(panel_t) :: panel
      type(node_t) :: n1, n2, n3, n4
      type(point_t) :: cp
      type(vector_t) :: normal_vec

      print *, "Testing panel automatic area computation for non-positive input..."

      ! Quadrilateral unit square
      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=1.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      cp = point_t(x=0.5_wp, y=0.5_wp, z=0.0_wp)
      normal_vec = vector_t(0.0_wp, 0.0_wp, 1.0_wp)

      panel = panel_t(3_ip, [n1, n2, n3, n4], cp, normal_vec, 0.0_wp)
      call assert(abs(panel%area - 1.0_wp) < TOL, "Quadrilateral area is auto-computed when input area is zero")

      ! Right triangle
      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = n3
      cp = point_t(x=1.0_wp/3.0_wp, y=1.0_wp/3.0_wp, z=0.0_wp)

      panel = panel_t(4_ip, [n1, n2, n3, n4], cp, normal_vec, -1.0_wp)
      call assert(abs(panel%area - 0.5_wp) < TOL, "Triangle area is auto-computed when input area is negative")
   end subroutine test_panel_auto_area_computation

   subroutine test_panel_rotate_and_translate()
      type(panel_t) :: panel
      type(node_t) :: n1, n2, n3, n4
      type(point_t) :: cp
      type(vector_t) :: normal_vec

      print *, "Testing panel rotation and translation..."

      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=1.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      cp = point_t(x=0.5_wp, y=0.5_wp, z=0.0_wp)
      normal_vec = vector_t(0.0_wp, 0.0_wp, 1.0_wp)

      panel = panel_t(5_ip, [n1, n2, n3, n4], cp, normal_vec, 1.0_wp)

      ! Άλλαξε σε rotate_origin
      call panel%rotate_origin(90.0_wp, [0.0_wp, 0.0_wp, 1.0_wp])
      call assert_vector_close(panel%nodes(2)%coordinates, [0.0_wp, 1.0_wp, 0.0_wp], "Rotation maps node 2 correctly")
      call assert_vector_close(panel%center_point%coordinates, [-0.5_wp, 0.5_wp, 0.0_wp], "Rotation updates center point")
      call assert_vector_close(panel%normal%components, [0.0_wp, 0.0_wp, 1.0_wp], "Rotation preserves normal for z-axis rotation")

      call panel%translate([1.0_wp, 2.0_wp, 0.5_wp])
      call assert_vector_close(panel%nodes(2)%coordinates, [1.0_wp, 3.0_wp, 0.5_wp], "Translation updates node coordinates")
      call assert_vector_close(panel%center_point%coordinates, [0.5_wp, 2.5_wp, 0.5_wp], "Translation updates center point")
   end subroutine test_panel_rotate_and_translate

   subroutine test_panel_rotation_visualization_output()
      type(panel_t) :: panel
      type(plotter_t) :: plt
      type(node_t) :: n1, n2, n3, n4
      type(point_t) :: cp
      type(vector_t) :: normal_vec
      real(wp) :: x_initial(5), y_initial(5), x(5), y(5), cx_initial(1), cy_initial(1), cx(1), cy(1)
      logical :: file_exists
      character(len=*), parameter :: plot_file = "output/test_rotation_panel.png"

      print *, "Testing panel rotational visualization output..."

      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=1.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      cp = point_t(x=0.5_wp, y=0.5_wp, z=0.0_wp)
      normal_vec = vector_t(0.0_wp, 0.0_wp, 1.0_wp)

      panel = panel_t(6_ip, [n1, n2, n3, n4], cp, normal_vec, 1.0_wp)

      x_initial = [panel%nodes(1)%coordinates(1), panel%nodes(2)%coordinates(1), panel%nodes(3)%coordinates(1), panel%nodes(4)%coordinates(1), panel%nodes(1)%coordinates(1)]
      y_initial = [panel%nodes(1)%coordinates(2), panel%nodes(2)%coordinates(2), panel%nodes(3)%coordinates(2), panel%nodes(4)%coordinates(2), panel%nodes(1)%coordinates(2)]
      cx_initial = [panel%center_point%coordinates(1)]
      cy_initial = [panel%center_point%coordinates(2)]

      ! Άλλαξε σε rotate_origin
      call panel%rotate_origin(45.0_wp, [0.0_wp, 0.0_wp, 1.0_wp])

      x = [panel%nodes(1)%coordinates(1), panel%nodes(2)%coordinates(1), panel%nodes(3)%coordinates(1), panel%nodes(4)%coordinates(1), panel%nodes(1)%coordinates(1)]
      y = [panel%nodes(1)%coordinates(2), panel%nodes(2)%coordinates(2), panel%nodes(3)%coordinates(2), panel%nodes(4)%coordinates(2), panel%nodes(1)%coordinates(2)]
      cx = [panel%center_point%coordinates(1)]
      cy = [panel%center_point%coordinates(2)]

      call plt%figure(figsize=[8, 6])
      call plt%title("Panel Outline (XY Projection)")
      call plt%xlabel("x")
      call plt%ylabel("y")
      call plt%grid(.true.)
      call plt%add(x_initial, y_initial, "initial panel", style="--", linewidth=2, color=[0.5_wp, 0.5_wp, 0.5_wp])
      call plt%add(cx_initial, cy_initial, "initial center point", style="o", markersize=8, color=[0.9_wp, 0.2_wp, 0.2_wp])
      call plt%add(x, y, "rotated panel", style="-", linewidth=2, color=[0.0_wp, 0.4_wp, 0.8_wp])
      call plt%add(cx, cy, "center point", style="o", markersize=8, color=[0.9_wp, 0.2_wp, 0.2_wp])
      call plt%save(plot_file)

      inquire (file=plot_file, exist=file_exists)
      call assert(file_exists, "Panel visualization written to '"//plot_file//"'")
   end subroutine test_panel_rotation_visualization_output

   subroutine test_panel_translation_visualization_output()
      type(panel_t) :: panel
      type(plotter_t) :: plt
      type(node_t) :: n1, n2, n3, n4
      type(point_t) :: cp
      type(vector_t) :: normal_vec
      real(wp) :: x_initial(5), y_initial(5), x(5), y(5), cx_initial(1), cy_initial(1), cx(1), cy(1)
      logical :: file_exists
      character(len=*), parameter :: plot_file = "output/test_translation_panel.png"

      print *, "Testing panel translation visualization output..."

      n1 = node_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      n2 = node_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      n3 = node_t(x=1.0_wp, y=1.0_wp, z=0.0_wp)
      n4 = node_t(x=0.0_wp, y=1.0_wp, z=0.0_wp)
      cp = point_t(x=0.5_wp, y=0.5_wp, z=0.0_wp)
      normal_vec = vector_t(0.0_wp, 0.0_wp, 1.0_wp)

      panel = panel_t(7_ip, [n1, n2, n3, n4], cp, normal_vec, 1.0_wp)

      x_initial = [panel%nodes(1)%coordinates(1), panel%nodes(2)%coordinates(1), panel%nodes(3)%coordinates(1), panel%nodes(4)%coordinates(1), panel%nodes(1)%coordinates(1)]
      y_initial = [panel%nodes(1)%coordinates(2), panel%nodes(2)%coordinates(2), panel%nodes(3)%coordinates(2), panel%nodes(4)%coordinates(2), panel%nodes(1)%coordinates(2)]
      cx_initial = [panel%center_point%coordinates(1)]
      cy_initial = [panel%center_point%coordinates(2)]

      call panel%translate([0.5_wp, 0.2_wp, 0.0_wp])

      x = [panel%nodes(1)%coordinates(1), panel%nodes(2)%coordinates(1), panel%nodes(3)%coordinates(1), panel%nodes(4)%coordinates(1), panel%nodes(1)%coordinates(1)]
      y = [panel%nodes(1)%coordinates(2), panel%nodes(2)%coordinates(2), panel%nodes(3)%coordinates(2), panel%nodes(4)%coordinates(2), panel%nodes(1)%coordinates(2)]
      cx = [panel%center_point%coordinates(1)]
      cy = [panel%center_point%coordinates(2)]

      call plt%figure(figsize=[8, 6])
      call plt%title("Panel Outline (XY Projection)")
      call plt%xlabel("x")
      call plt%ylabel("y")
      call plt%grid(.true.)
      call plt%add(x_initial, y_initial, "initial panel", style="--", linewidth=2, color=[0.5_wp, 0.5_wp, 0.5_wp])
      call plt%add(cx_initial, cy_initial, "initial center point", style="o", markersize=8, color=[0.9_wp, 0.2_wp, 0.2_wp])
      call plt%add(x, y, "translated panel", style="-", linewidth=2, color=[0.0_wp, 0.4_wp, 0.8_wp])
      call plt%add(cx, cy, "center point", style="o", markersize=8, color=[0.9_wp, 0.2_wp, 0.2_wp])
      call plt%save(plot_file)

      inquire (file=plot_file, exist=file_exists)
      call assert(file_exists, "Panel visualization written to '"//plot_file//"'")
   end subroutine test_panel_translation_visualization_output

   subroutine assert(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) then
         print *, "   [FAIL] ", message
         fail_count = fail_count + 1
      else
         print *, "   [PASS] ", message
      end if
   end subroutine assert

   subroutine assert_vector_close(v1, v2, message)
      real(wp), intent(in) :: v1(3), v2(3)
      character(len=*), intent(in) :: message

      call assert(all(abs(v1 - v2) < TOL), message)
   end subroutine assert_vector_close

end program test_panel
