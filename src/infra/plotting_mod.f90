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
   contains
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

      close (this%script_unit)

      if (.not. gnuplot_available) then
         call global_logger%msg(LOG_WARN, "[PLOTTER] Gnuplot not available - skipping render")
         call global_logger%msg(LOG_INFO, "[PLOTTER] Data saved to: "//trim(this%script_file))
         return
      end if

      call execute_command_line("gnuplot -persist "//trim(this%script_file))
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

end module plotting_mod
