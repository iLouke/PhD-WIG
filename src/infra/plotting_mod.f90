module plotting_mod
   use base_kinds_mod, only: wp
   use logger_mod, only: global_logger, LOG_INFO, LOG_WARN, LOG_ERROR
   implicit none
   private
   public :: plotter_t

   ! Module-level counter for unique plotter instances
   integer, save :: global_plotter_id = 0
   logical, save :: gnuplot_checked = .false.
   logical, save :: gnuplot_available = .false.

   type :: plotter_t
      private
      integer :: instance_id = -1
      integer :: script_unit = -1
      character(len=256) :: script_file = "output/plot_script.gp"
      character(len=4096) :: plot_cmd = ""
      character(len=4096) :: settings = ""
      integer :: series_count = 0
      ! 3D-specific fields
      logical :: is_3d = .false.
      character(len=8192) :: splot_cmd = ""
      integer :: splot_count = 0
   contains
      ! 2D plotting
      procedure :: figure => plotter_figure
      procedure :: title => plotter_title
      procedure :: xlabel => plotter_xlabel
      procedure :: ylabel => plotter_ylabel
      procedure :: grid => plotter_grid
      procedure :: xrange => plotter_xrange
      procedure :: yrange => plotter_yrange
      procedure :: add => plotter_add_series
      procedure :: render => plotter_render
      procedure :: save => plotter_save
      ! 3D plotting
      procedure :: figure_3d => plotter_figure_3d
      procedure :: zlabel => plotter_zlabel
      procedure :: view => plotter_view
      procedure :: add_panels_3d => plotter_add_panels_3d
      procedure :: render_3d => plotter_render_3d
      procedure :: save_3d => plotter_save_3d
   end type plotter_t

contains

   subroutine plotter_figure(this)
      class(plotter_t), intent(inout) :: this
      character(len=10) :: id_str

      ! Check gnuplot availability once per program run
      if (.not. gnuplot_checked) then
         call check_gnuplot_available()
      end if

      ! Assign unique instance ID
      global_plotter_id = global_plotter_id + 1
      this%instance_id = global_plotter_id

      ! Ensure directory exists (Linux/Unix command)
      call execute_command_line("mkdir -p output")

      ! Create unique script filename for this instance
      write (id_str, '(I0)') this%instance_id
      this%script_file = "output/plot_script_"//trim(id_str)//".gp"

      open (newunit=this%script_unit, file=this%script_file, status='replace')

      write (this%script_unit, '(A)') "set term qt size 900,600 font 'Arial,11'"
      write (this%script_unit, '(A)') "set key top right box opaque"

      this%plot_cmd = ""
      this%settings = ""
      this%series_count = 0
   end subroutine plotter_figure

   subroutine plotter_title(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      write (this%script_unit, '(A,A,A)') "set title '", trim(text), "'"
      this%settings = trim(this%settings)//"set title '"//trim(text)//"'"//new_line('a')
   end subroutine plotter_title

   subroutine plotter_xlabel(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      write (this%script_unit, '(A,A,A)') "set xlabel '", trim(text), "'"
      this%settings = trim(this%settings)//"set xlabel '"//trim(text)//"'"//new_line('a')
   end subroutine plotter_xlabel

   subroutine plotter_ylabel(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      write (this%script_unit, '(A,A,A)') "set ylabel '", trim(text), "'"
      this%settings = trim(this%settings)//"set ylabel '"//trim(text)//"'"//new_line('a')
   end subroutine plotter_ylabel

   subroutine plotter_grid(this, enable)
      class(plotter_t), intent(inout) :: this
      logical, intent(in) :: enable
      if (enable) then
         write (this%script_unit, '(A)') "set grid"
         this%settings = trim(this%settings)//"set grid"//new_line('a')
      else
         write (this%script_unit, '(A)') "unset grid"
         this%settings = trim(this%settings)//"unset grid"//new_line('a')
      end if
   end subroutine plotter_grid

   subroutine plotter_xrange(this, min_val, max_val)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in) :: min_val, max_val
      character(len=256) :: range_str
      write (this%script_unit, '(A,F12.4,A,F12.4,A)') "set xrange [", min_val, ":", max_val, "]"
      write (range_str, '(A,F12.4,A,F12.4,A)') "set xrange [", min_val, ":", max_val, "]"
      this%settings = trim(this%settings)//trim(range_str)//new_line('a')
   end subroutine plotter_xrange  ! <--- Fixed Label

   subroutine plotter_yrange(this, min_val, max_val)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in) :: min_val, max_val
      character(len=256) :: range_str
      write (this%script_unit, '(A,F12.4,A,F12.4,A)') "set yrange [", min_val, ":", max_val, "]"
      write (range_str, '(A,F12.4,A,F12.4,A)') "set yrange [", min_val, ":", max_val, "]"
      this%settings = trim(this%settings)//trim(range_str)//new_line('a')
   end subroutine plotter_yrange  ! <--- Fixed Label

   subroutine plotter_add_series(this, x, y, label, style)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in)            :: x(:), y(:)
      character(len=*), intent(in)    :: label
      character(len=*), intent(in)    :: style

      integer :: d_unit, i
      character(len=256) :: filename
      character(len=10) :: id_str, num_str

      this%series_count = this%series_count + 1
      write (id_str, '(I0)') this%instance_id
      write (num_str, '(I0)') this%series_count

      ! Save data to output/data_plotN_M.dat (N=instance, M=series)
      filename = "output/data_plot"//trim(id_str)//"_"//trim(num_str)//".dat"

      open (newunit=d_unit, file=filename, status='replace')
      do i = 1, size(x)
         write (d_unit, *) x(i), y(i)
      end do
      close (d_unit)

      if (this%series_count > 1) then
         this%plot_cmd = trim(this%plot_cmd)//", "
      end if

      ! The script refers to the file by its relative path 'output/data_N.dat'
      this%plot_cmd = trim(this%plot_cmd)//" '"//trim(filename)//"' "// &
                      "title '"//trim(label)//"' with "//trim(style)

   end subroutine plotter_add_series

   subroutine plotter_render(this)
      class(plotter_t), intent(inout) :: this

      if (this%series_count > 0) then
         write (this%script_unit, '(A, A)') "plot ", trim(this%plot_cmd)
      end if

      ! Keep window open and interactive until user closes it
      write (this%script_unit, '(A)') "pause mouse close"
      close (this%script_unit)

      if (.not. gnuplot_available) then
         call global_logger%msg(LOG_WARN, "[PLOTTER] Gnuplot not available - skipping render")
         call global_logger%msg(LOG_INFO, "[PLOTTER] Data saved to: "//trim(this%script_file))
         return
      end if

      ! Launch in background so program doesn't block
      call execute_command_line("gnuplot "//trim(this%script_file)//" &")
   end subroutine plotter_render

   subroutine plotter_save(this, filename)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: filename

      integer :: png_unit
      character(len=256) :: png_script
      character(len=10) :: id_str

      if (.not. gnuplot_available) then
         call global_logger%msg(LOG_ERROR, "[PLOTTER] Cannot save PNG - gnuplot not available")
         return
      end if

      if (this%series_count == 0) then
         call global_logger%msg(LOG_WARN, "[PLOTTER] No data to save")
         return
      end if

      ! Create a PNG-specific script
      write (id_str, '(I0)') this%instance_id
      png_script = "output/plot_png_"//trim(id_str)//".gp"

      open (newunit=png_unit, file=png_script, status='replace')

      ! Set PNG terminal and output
      write (png_unit, '(A)') "set term pngcairo size 900,600 font 'Arial,11'"
      write (png_unit, '(A,A,A)') "set output '", trim(filename), "'"
      write (png_unit, '(A)') "set key top right box opaque"

      ! Write all accumulated settings
      if (len_trim(this%settings) > 0) then
         write (png_unit, '(A)') trim(this%settings)
      end if

      ! Write plot command
      write (png_unit, '(A, A)') "plot ", trim(this%plot_cmd)

      close (png_unit)

      ! Execute gnuplot to generate PNG
      call execute_command_line("gnuplot "//trim(png_script))
      call global_logger%msg(LOG_INFO, "[PLOTTER] Plot saved to: "//trim(filename))
   end subroutine plotter_save

   subroutine check_gnuplot_available()
      integer :: exit_code

      gnuplot_checked = .true.

      ! Try to check if gnuplot exists using 'command -v'
      call execute_command_line("command -v gnuplot > /dev/null 2>&1", &
                                exitstat=exit_code)

      if (exit_code == 0) then
         gnuplot_available = .true.
         call global_logger%msg(LOG_INFO, "[PLOTTER] Gnuplot detected - plots will be rendered")
      else
         gnuplot_available = .false.
         call global_logger%msg(LOG_WARN, "[PLOTTER] WARNING: Gnuplot not found - plots will be skipped")
         call global_logger%msg(LOG_INFO, "[PLOTTER] Install gnuplot to enable plot rendering")
      end if
   end subroutine check_gnuplot_available

   ! ═══════════════════════════════════════════════════════════════════════
   !                     3D PLOTTING SUBROUTINES
   ! ═══════════════════════════════════════════════════════════════════════

   !> Initialize a 3D figure (sets up gnuplot for splot)
   !!
   !! Sets terminal to a large window with 3D-appropriate defaults:
   !! equal-axis scaling, default view angle, colorbar palette.
   subroutine plotter_figure_3d(this)
      class(plotter_t), intent(inout) :: this
      character(len=10) :: id_str

      if (.not. gnuplot_checked) then
         call check_gnuplot_available()
      end if

      global_plotter_id = global_plotter_id + 1
      this%instance_id = global_plotter_id

      call execute_command_line("mkdir -p output")

      write (id_str, '(I0)') this%instance_id
      this%script_file = "output/plot_script_"//trim(id_str)//".gp"

      open (newunit=this%script_unit, file=this%script_file, status='replace')

      ! 3D terminal and defaults
      write (this%script_unit, '(A)') "set term qt size 1200,800 font 'Arial,11'"
      write (this%script_unit, '(A)') "set key top right box opaque"
      write (this%script_unit, '(A)') "set ticslevel 0"
      write (this%script_unit, '(A)') "set view 60, 330, 1.0"
      write (this%script_unit, '(A)') &
         "set palette defined (-1 'dark-blue', -0.5 'blue', 0 'white', 0.5 'red', 1 'dark-red')"

      ! Reset all state
      this%plot_cmd = ""
      this%splot_cmd = ""
      this%settings = ""
      this%series_count = 0
      this%splot_count = 0
      this%is_3d = .true.
   end subroutine plotter_figure_3d

   !> Set the Z-axis label (3D plots only)
   subroutine plotter_zlabel(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      write (this%script_unit, '(A,A,A)') "set zlabel '", trim(text), "'"
      this%settings = trim(this%settings)//"set zlabel '"//trim(text)//"'"//new_line('a')
   end subroutine plotter_zlabel

   !> Set the 3D view angle
   !!
   !! @param rot_x  Rotation about X-axis (elevation) [degrees]
   !! @param rot_z  Rotation about Z-axis (azimuth) [degrees]
   subroutine plotter_view(this, rot_x, rot_z)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in) :: rot_x, rot_z
      character(len=256) :: view_str

      write (this%script_unit, '(A,F8.2,A,F8.2,A)') &
         "set view ", rot_x, ", ", rot_z, ", 1.0"
      write (view_str, '(A,F8.2,A,F8.2,A)') &
         "set view ", rot_x, ", ", rot_z, ", 1.0"
      this%settings = trim(this%settings)//trim(view_str)//new_line('a')
   end subroutine plotter_view

   !> Add a set of wireframe panels to the 3D plot
   !!
   !! Each panel is defined by its four corner coordinates. The plotter
   !! writes each panel as a closed quadrilateral to a data file and
   !! builds the corresponding gnuplot splot command.
   !!
   !! @param px      X-coordinates of panel corners (4, n_panels)
   !! @param py      Y-coordinates of panel corners (4, n_panels)
   !! @param pz      Z-coordinates of panel corners (4, n_panels)
   !! @param label   Legend label for this dataset
   !! @param color   Line color string (e.g. "blue", "#FF0000"). Ignored when scalar is present.
   !! @param scalar  Optional per-panel scalar value for palette coloring (n_panels)
   subroutine plotter_add_panels_3d(this, px, py, pz, label, color, scalar)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in)            :: px(:, :), py(:, :), pz(:, :) ! (4, n_panels)
      character(len=*), intent(in)    :: label
      character(len=*), intent(in), optional :: color
      real(wp), intent(in), optional  :: scalar(:)  ! (n_panels)

      integer :: d_unit, i, j, np
      character(len=256) :: filename
      character(len=10)  :: id_str, num_str
      character(len=32)  :: col_str
      logical :: has_scalar

      has_scalar = present(scalar)
      np = size(px, 2)
      if (np < 1) return

      this%splot_count = this%splot_count + 1
      write (id_str, '(I0)') this%instance_id
      write (num_str, '(I0)') this%splot_count

      filename = "output/data_3d_"//trim(id_str)//"_"//trim(num_str)//".dat"

      open (newunit=d_unit, file=filename, status='replace')
      write (d_unit, '(A,I0,A)') "# ", np, " panels"

      do j = 1, np
         ! Write 4 corners + first corner again (close the quad)
         do i = 1, 4
            if (has_scalar) then
               write (d_unit, '(4(ES16.8,1X))') px(i, j), py(i, j), pz(i, j), scalar(j)
            else
               write (d_unit, '(3(ES16.8,1X))') px(i, j), py(i, j), pz(i, j)
            end if
         end do
         ! Close the loop
         if (has_scalar) then
            write (d_unit, '(4(ES16.8,1X))') px(1, j), py(1, j), pz(1, j), scalar(j)
         else
            write (d_unit, '(3(ES16.8,1X))') px(1, j), py(1, j), pz(1, j)
         end if
         write (d_unit, *)  ! Blank line separates panels
      end do
      close (d_unit)

      ! Build splot command fragment
      col_str = "blue"
      if (present(color)) col_str = trim(color)

      if (this%splot_count > 1) then
         this%splot_cmd = trim(this%splot_cmd)//", "
      end if

      if (has_scalar) then
         this%splot_cmd = trim(this%splot_cmd)//" '"//trim(filename)//"' "// &
                          "using 1:2:3:4 with lines palette lw 1.0 title '"//trim(label)//"'"
      else
         this%splot_cmd = trim(this%splot_cmd)//" '"//trim(filename)//"' "// &
                          "with lines lc rgb '"//trim(col_str)//"' lw 1.5 title '"//trim(label)//"'"
      end if

   end subroutine plotter_add_panels_3d

   !> Render the 3D plot interactively (opens a gnuplot window)
   !!
   !! Launches gnuplot as a background process so the Fortran program
   !! continues immediately. The gnuplot window stays open and fully
   !! interactive (rotate, zoom, pan with mouse) until the user closes it.
   subroutine plotter_render_3d(this)
      class(plotter_t), intent(inout) :: this
      logical :: is_open

      ! Write the splot command and pause if still open
      inquire (unit=this%script_unit, opened=is_open)
      if (is_open) then
         if (this%splot_count > 0) then
            write (this%script_unit, '(A,A)') "splot ", trim(this%splot_cmd)
         end if
         ! Keep gnuplot window open and interactive until user closes it
         write (this%script_unit, '(A)') "pause mouse close"
         close (this%script_unit)
      end if

      if (.not. gnuplot_available) then
         call global_logger%msg(LOG_WARN, "[PLOTTER] Gnuplot not available - skipping 3D render")
         call global_logger%msg(LOG_INFO, "[PLOTTER] Script saved to: "//trim(this%script_file))
         return
      end if

      ! Launch gnuplot in background (&) so it doesn't block the program
      call execute_command_line("gnuplot "//trim(this%script_file)//" &")
      call global_logger%msg(LOG_INFO, "[PLOTTER] 3D interactive plot launched: "//trim(this%script_file))
   end subroutine plotter_render_3d

   !> Save the 3D plot to a PNG file (no interactive window)
   !!
   !! Creates a separate gnuplot script with the PNG terminal, writes all
   !! accumulated settings and the splot command, then executes gnuplot.
   !! This can be called after or instead of render_3d.
   subroutine plotter_save_3d(this, filename)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: filename

      integer :: png_unit
      character(len=256) :: png_script
      character(len=10) :: id_str

      if (.not. gnuplot_available) then
         call global_logger%msg(LOG_ERROR, "[PLOTTER] Cannot save 3D PNG - gnuplot not available")
         return
      end if

      if (this%splot_count == 0) then
         call global_logger%msg(LOG_WARN, "[PLOTTER] No 3D data to save")
         return
      end if

      write (id_str, '(I0)') this%instance_id
      png_script = "output/plot_png_"//trim(id_str)//".gp"

      open (newunit=png_unit, file=png_script, status='replace')

      ! PNG terminal
      write (png_unit, '(A)') "set term pngcairo size 1400,900 font 'Arial,11'"
      write (png_unit, '(A,A,A)') "set output '", trim(filename), "'"
      write (png_unit, '(A)') "set key top right box opaque"
      write (png_unit, '(A)') "set ticslevel 0"
      write (png_unit, '(A)') &
         "set palette defined (-1 'dark-blue', -0.5 'blue', 0 'white', 0.5 'red', 1 'dark-red')"

      ! Write all accumulated settings (title, labels, view, grid, etc.)
      if (len_trim(this%settings) > 0) then
         write (png_unit, '(A)') trim(this%settings)
      end if

      ! Write splot command
      write (png_unit, '(A,A)') "splot ", trim(this%splot_cmd)

      close (png_unit)

      call execute_command_line("gnuplot "//trim(png_script))
      call global_logger%msg(LOG_INFO, "[PLOTTER] 3D plot saved to: "//trim(filename))
   end subroutine plotter_save_3d

end module plotting_mod
