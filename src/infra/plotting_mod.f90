module plotting_mod
   use base_kinds_mod, only: wp
   use logger_mod, only: global_logger, LOG_INFO, LOG_WARN, LOG_ERROR
   use pyplot_module, only: pyplot

   implicit none
   private
   public :: plotter_t, check_matplotlib

   ! Module-level counter for unique plotter instances
   integer, save :: global_plotter_id = 0
   character(len=16), save :: python_executable = 'python'

   type :: plotter_t
      private
      type(pyplot) :: plt
      integer :: instance_id = -1
      integer :: series_count = 0
      logical :: is_initialized = .false.

      ! Figure properties
      character(len=256) :: title_str = ""
      character(len=256) :: xlabel_str = ""
      character(len=256) :: ylabel_str = ""
      logical            :: grid_enabled = .false.
      integer            :: fig_size(2) = [8, 6]
      logical            :: has_figsize = .false.
      logical            :: use_tight_layout = .true.

      ! Axis properties
      real(wp)           :: x_lim(2) = [0.0_wp, 0.0_wp]
      real(wp)           :: y_lim(2) = [0.0_wp, 0.0_wp]
      logical            :: has_xlim = .false.
      logical            :: has_ylim = .false.
      character(len=20)  :: x_scale_str = "linear"
      character(len=20)  :: y_scale_str = "linear"

   contains
      procedure :: figure => plotter_figure
      procedure :: title => plotter_title
      procedure :: xlabel => plotter_xlabel
      procedure :: ylabel => plotter_ylabel
      procedure :: grid => plotter_grid
      procedure :: xrange => plotter_xrange
      procedure :: yrange => plotter_yrange
      procedure :: xscale => plotter_xscale
      procedure :: yscale => plotter_yscale
      procedure :: add => plotter_add_series
      procedure :: save => plotter_save
   end type plotter_t

contains

   subroutine plotter_figure(this, figsize, tight_layout)
      class(plotter_t), intent(inout) :: this
      integer, intent(in), optional   :: figsize(2)
      logical, intent(in), optional   :: tight_layout

      global_plotter_id = global_plotter_id + 1
      this%instance_id = global_plotter_id
      call create_output_directory()

      this%series_count = 0
      this%is_initialized = .false.
      this%title_str = ""
      this%xlabel_str = ""
      this%ylabel_str = ""
      this%grid_enabled = .false.

      this%has_xlim = .false.
      this%has_ylim = .false.
      this%x_scale_str = "linear"
      this%y_scale_str = "linear"

      if (present(figsize)) then
         this%fig_size = figsize
         this%has_figsize = .true.
      else
         this%has_figsize = .false.
      end if

      if (present(tight_layout)) then
         this%use_tight_layout = tight_layout
      else
         this%use_tight_layout = .true. ! Defaulting to true is usually better
      end if
   end subroutine plotter_figure

   subroutine plotter_title(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      this%title_str = text
   end subroutine plotter_title

   subroutine plotter_xlabel(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      this%xlabel_str = text
   end subroutine plotter_xlabel

   subroutine plotter_ylabel(this, text)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: text
      this%ylabel_str = text
   end subroutine plotter_ylabel

   subroutine plotter_grid(this, enable)
      class(plotter_t), intent(inout) :: this
      logical, intent(in) :: enable
      this%grid_enabled = enable
   end subroutine plotter_grid

   subroutine plotter_xrange(this, xmin, xmax)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in) :: xmin, xmax
      this%x_lim = [xmin, xmax]
      this%has_xlim = .true.
   end subroutine plotter_xrange

   subroutine plotter_yrange(this, ymin, ymax)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in) :: ymin, ymax
      this%y_lim = [ymin, ymax]
      this%has_ylim = .true.
   end subroutine plotter_yrange

   subroutine plotter_xscale(this, scale)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in) :: scale
      this%x_scale_str = scale
   end subroutine plotter_xscale

   subroutine plotter_yscale(this, scale)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in) :: scale
      this%y_scale_str = scale
   end subroutine plotter_yscale

   subroutine plotter_add_series(this, x, y, label, style, linewidth, markersize, color)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in)            :: x(:), y(:)
      character(len=*), intent(in)    :: label
      character(len=*), intent(in)    :: style
      integer, intent(in), optional   :: linewidth
      integer, intent(in), optional   :: markersize
      real(wp), intent(in), optional  :: color(3) ! RGB array [0.0 to 1.0]

      ! Lazily initialize pyplot on the first added series
      if (.not. this%is_initialized) then
         if (this%has_figsize) then
            call this%plt%initialize(grid=this%grid_enabled, &
                                     xlabel=trim(this%xlabel_str), &
                                     ylabel=trim(this%ylabel_str), &
                                     title=trim(this%title_str), &
                                     tight_layout=this%use_tight_layout, &
                                     figsize=this%fig_size, &
                                     legend=.true.)
         else
            call this%plt%initialize(grid=this%grid_enabled, &
                                     xlabel=trim(this%xlabel_str), &
                                     ylabel=trim(this%ylabel_str), &
                                     title=trim(this%title_str), &
                                     tight_layout=this%use_tight_layout, &
                                     legend=.true.)
         end if
         this%is_initialized = .true.
      end if

      this%series_count = this%series_count + 1

      if (this%has_xlim .and. this%has_ylim) then
         call this%plt%add_plot(x, y, label=trim(label), linestyle=trim(style), &
                                linewidth=linewidth, markersize=markersize, color=color, &
                                xscale=trim(this%x_scale_str), yscale=trim(this%y_scale_str), &
                                xlim=this%x_lim, ylim=this%y_lim)
      elseif (this%has_xlim) then
         call this%plt%add_plot(x, y, label=trim(label), linestyle=trim(style), &
                                linewidth=linewidth, markersize=markersize, color=color, &
                                xscale=trim(this%x_scale_str), yscale=trim(this%y_scale_str), &
                                xlim=this%x_lim)
      elseif (this%has_ylim) then
         call this%plt%add_plot(x, y, label=trim(label), linestyle=trim(style), &
                                linewidth=linewidth, markersize=markersize, color=color, &
                                xscale=trim(this%x_scale_str), yscale=trim(this%y_scale_str), &
                                ylim=this%y_lim)
      else
         call this%plt%add_plot(x, y, label=trim(label), linestyle=trim(style), &
                                linewidth=linewidth, markersize=markersize, color=color, &
                                xscale=trim(this%x_scale_str), yscale=trim(this%y_scale_str))
      end if
   end subroutine plotter_add_series

   subroutine plotter_save(this, filename)
      class(plotter_t), intent(inout) :: this
      character(len=*), intent(in)    :: filename

      character(len=256) :: py_filename
      character(len=10)  :: id_str

      if (this%series_count == 0) then
         call global_logger%msg(LOG_WARN, "[PLOTTER] No data to save")
         return
      end if

      write (id_str, '(I0)') this%instance_id
      py_filename = "output/plot_"//trim(id_str)//".py"

      if (check_matplotlib()) then
         call this%plt%savefig(trim(filename), pyfile=trim(py_filename), python=trim(python_executable))
         call global_logger%msg(LOG_INFO, "[PLOTTER] Plot saved to: "//trim(filename))
      else
         call global_logger%msg(LOG_WARN, "[PLOTTER] Skipping plot rendering due to missing Matplotlib.")
      end if
   end subroutine plotter_save

   function check_matplotlib() result(is_available)
      logical :: is_available
      integer :: exit_code
      logical, save :: checked = .false.
      logical, save :: available = .false.
      logical :: is_windows

      if (.not. checked) then
         is_windows = is_windows_platform()

         if (is_windows) then
            call try_python_import('py -3', available, exit_code)
            if (available) then
               python_executable = 'py -3'
               call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (py -3) verified successfully.")
            else
               call try_python_import('py', available, exit_code)
               if (available) then
                  python_executable = 'py'
                  call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (py) verified successfully.")
               else
                  call try_python_import('python', available, exit_code)
                  if (available) then
                     python_executable = 'python'
                     call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (python) verified successfully.")
                  else
                     available = .false.
                     call global_logger%msg(LOG_WARN, "[PLOTTER] WARNING: Python or matplotlib not found.")
                  end if
               end if
            end if
         else
            call try_python_import('python3', available, exit_code)
            if (available) then
               python_executable = 'python3'
               call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (python3) verified successfully.")
            else
               call try_python_import('python', available, exit_code)
               if (available) then
                  python_executable = 'python'
                  call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (python) verified successfully.")
               else
                  available = .false.
                  call global_logger%msg(LOG_WARN, "[PLOTTER] WARNING: Python or matplotlib not found.")
               end if
            end if
         end if
         checked = .true.
      end if

      is_available = available
   end function check_matplotlib

   subroutine try_python_import(python_cmd, available, exit_code)
      character(len=*), intent(in) :: python_cmd
      logical, intent(out) :: available
      integer, intent(out) :: exit_code

      integer :: cmd_status
      call execute_command_line(trim(python_cmd)//' -c "import matplotlib.pyplot"', &
                                cmdstat=cmd_status, exitstat=exit_code)
      available = (cmd_status == 0 .and. exit_code == 0)
   end subroutine try_python_import

   subroutine create_output_directory()
      logical :: dir_exists
      integer :: cmdstat, exitstat

      inquire (file='output', exist=dir_exists)
      if (dir_exists) return

      if (is_windows_platform()) then
         call execute_command_line('cmd /c if not exist output mkdir output', &
                                   cmdstat=cmdstat, exitstat=exitstat)
      else
         call execute_command_line('mkdir -p output', cmdstat=cmdstat, exitstat=exitstat)
      end if
   end subroutine create_output_directory

   logical function is_windows_platform() result(is_windows)
      character(len=64) :: os_name
      integer :: env_len, env_stat

      os_name = ''
      call get_environment_variable('OS', value=os_name, length=env_len, status=env_stat)

      if (env_stat == 0 .and. env_len > 0) then
         is_windows = index(adjustl(os_name(1:env_len)), 'Windows_NT') > 0
      else
         is_windows = .false.
      end if
   end function is_windows_platform

end module plotting_mod
