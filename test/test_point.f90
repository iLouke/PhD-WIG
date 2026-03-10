program test_point
   use base_kinds_mod, only: wp
   use point_mod, only: point_t
   use plotting_mod, only: plotter_t
   implicit none

   integer :: fail_count = 0
   real(wp), parameter :: TOL = 1.0e-10_wp

   print *, "=========================================="
   print *, "   RUNNING POINT MODULE TESTS             "
   print *, "=========================================="

   call test_point_initialization_and_get_set()
   call test_point_translation()
   call test_point_rotation()
   call test_point_scaling()
   call test_point_reflection()
   call test_point_distances_and_midpoint()
   call test_point_visualization_output()

   print *, "=========================================="
   if (fail_count == 0) then
      print *, " [SUCCESS] All point tests passed."
   else
      print *, " [FAILURE]", fail_count, "tests failed."
      error stop 1
   end if

contains

   subroutine test_point_initialization_and_get_set()
      type(point_t) :: pt
      real(wp) :: x, y, z

      print *, "Testing point initialization, getters, and setters..."

      ! Test custom constructor
      pt = point_t(id=1, x=1.0_wp, y=2.0_wp, z=3.0_wp)
      call assert(pt%id == 1, "Constructor sets ID correctly")
      call assert_vector_close(pt%coordinates, [1.0_wp, 2.0_wp, 3.0_wp], "Constructor sets coordinates correctly")

      ! Test getter
      call pt%get_coordinates(x, y, z)
      call assert_vector_close([x, y, z], [1.0_wp, 2.0_wp, 3.0_wp], "Getter retrieves coordinates accurately")

      ! Test setter
      call pt%set_coordinates(4.0_wp, 5.0_wp, 6.0_wp)
      call assert_vector_close(pt%coordinates, [4.0_wp, 5.0_wp, 6.0_wp], "Setter updates coordinates properly")
   end subroutine test_point_initialization_and_get_set

   subroutine test_point_translation()
      type(point_t) :: pt

      print *, "Testing point translation..."

      pt = point_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      call pt%translate(1.5_wp, -2.0_wp, 3.0_wp)

      call assert_vector_close(pt%coordinates, [1.5_wp, -2.0_wp, 3.0_wp], "Translation from origin [xyz]")
      pt = point_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      call pt%translate([1.5_wp, -2.0_wp, 3.0_wp])

      call assert_vector_close(pt%coordinates, [1.5_wp, -2.0_wp, 3.0_wp], "Translation from origin [vector]")
   end subroutine test_point_translation

   subroutine test_point_rotation()
      type(point_t) :: pt
      real(wp) :: rot_matrix(3, 3)

      print *, "Testing point rotation..."

      ! Create a 90-degree counter-clockwise rotation matrix around the Z-axis
      rot_matrix(1, :) = [0.0_wp, -1.0_wp, 0.0_wp]
      rot_matrix(2, :) = [1.0_wp, 0.0_wp, 0.0_wp]
      rot_matrix(3, :) = [0.0_wp, 0.0_wp, 1.0_wp]

      pt = point_t(x=1.0_wp, y=0.0_wp, z=0.0_wp)
      call pt%rotate(rot_matrix)

      call assert_vector_close(pt%coordinates, [0.0_wp, 1.0_wp, 0.0_wp], "Point rotated 90 degrees around Z-axis")
   end subroutine test_point_rotation

   subroutine test_point_scaling()
      type(point_t) :: pt

      print *, "Testing point scaling..."

      pt = point_t(x=2.0_wp, y=-3.0_wp, z=4.0_wp)
      call pt%scale(2.0_wp)

      call assert_vector_close(pt%coordinates, [4.0_wp, -6.0_wp, 8.0_wp], "Point coordinates scaled correctly")
   end subroutine test_point_scaling

   subroutine test_point_reflection()
      type(point_t) :: pt

      print *, "Testing point axis reflection..."

      ! Reflect across X-axis (Y and Z should flip)
      pt = point_t(x=1.0_wp, y=2.0_wp, z=3.0_wp)
      call pt%reflect_axis('x')
      call assert_vector_close(pt%coordinates, [1.0_wp, -2.0_wp, -3.0_wp], "Reflection across X-axis")

      ! Reflect across Y-axis (X and Z should flip)
      pt = point_t(x=1.0_wp, y=2.0_wp, z=3.0_wp)
      call pt%reflect_axis('Y')
      call assert_vector_close(pt%coordinates, [-1.0_wp, 2.0_wp, -3.0_wp], "Reflection across Y-axis")

      ! Reflect across Z-axis (X and Y should flip)
      pt = point_t(x=1.0_wp, y=2.0_wp, z=3.0_wp)
      call pt%reflect_axis('Z')
      call assert_vector_close(pt%coordinates, [-1.0_wp, -2.0_wp, 3.0_wp], "Reflection across Z-axis")
   end subroutine test_point_reflection

   subroutine test_point_distances_and_midpoint()
      type(point_t) :: pt1, pt2, mid_pt

      print *, "Testing distance and midpoint logic..."

      pt1 = point_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      pt2 = point_t(x=3.0_wp, y=4.0_wp, z=0.0_wp)

      call assert(abs(pt2%distance_to_origin() - 5.0_wp) < TOL, "Distance to origin calculation (3-4-5 triangle)")
      call assert(abs(pt1%distance_to(pt2) - 5.0_wp) < TOL, "Distance between two points")

      mid_pt = pt1%midpoint(pt2)
      call assert_vector_close(mid_pt%coordinates, [1.5_wp, 2.0_wp, 0.0_wp], "Midpoint coordinates calculated correctly")
   end subroutine test_point_distances_and_midpoint

   subroutine test_point_visualization_output()
      type(point_t) :: pt
      type(plotter_t) :: plt
      real(wp) :: px_init(1), py_init(1), px_trans(1), py_trans(1)
      logical :: file_exists
      character(len=*), parameter :: plot_file = "output/test_translation_point.png"

      print *, "Testing point visualization output..."

      pt = point_t(x=0.0_wp, y=0.0_wp, z=0.0_wp)
      px_init = [pt%coordinates(1)]
      py_init = [pt%coordinates(2)]

      ! Translate the point
      call pt%translate(2.0_wp, 2.0_wp, 0.0_wp)
      px_trans = [pt%coordinates(1)]
      py_trans = [pt%coordinates(2)]

      call plt%figure(figsize=[8, 6])
      call plt%title("Point Translation (XY Projection)")
      call plt%xlabel("x")
      call plt%ylabel("y")
      call plt%grid(.true.)
      call plt%add(px_init, py_init, "Initial Point", style="o", markersize=10, color=[0.5_wp, 0.5_wp, 0.5_wp])
      call plt%add(px_trans, py_trans, "Translated Point", style="x", markersize=10, color=[0.0_wp, 0.4_wp, 0.8_wp])
      call plt%save(plot_file)

      inquire (file=plot_file, exist=file_exists)
      call assert(file_exists, "Point visualization written to '"//plot_file//"'")
   end subroutine test_point_visualization_output

   ! ---------------------------------------------------------
   ! Helper Subroutines
   ! ---------------------------------------------------------

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

end program test_point
