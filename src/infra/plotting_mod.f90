module plotting_mod
   use base_kinds_mod, only: wp
   use logger_mod, only: global_logger, LOG_INFO, LOG_WARN, LOG_ERROR
   use pyplot_module, only: pyplot

   implicit none
   private
   public :: plotter_t, check_matplotlib

   ! Module-level counter for unique plotter instances
   integer, save :: global_plotter_id = 0

   type :: plotter_t
      private
      type(pyplot) :: plt
      integer :: instance_id = -1
      integer :: series_count = 0
      logical :: is_initialized = .false.

      ! Cached settings for lazy initialization
      character(len=256) :: title_str = ""
      character(len=256) :: xlabel_str = ""
      character(len=256) :: ylabel_str = ""
      logical            :: grid_enabled = .false.
   contains
      procedure :: figure => plotter_figure
      procedure :: title => plotter_title
      procedure :: xlabel => plotter_xlabel
      procedure :: ylabel => plotter_ylabel
      procedure :: grid => plotter_grid
      procedure :: add => plotter_add_series
      procedure :: save => plotter_save
   end type plotter_t

contains

   subroutine plotter_figure(this)
      class(plotter_t), intent(inout) :: this

      global_plotter_id = global_plotter_id + 1
      this%instance_id = global_plotter_id

      ! Ensure directory exists for python script outputs
      call execute_command_line("mkdir -p output")

      this%series_count = 0
      this%is_initialized = .false.
      this%title_str = ""
      this%xlabel_str = ""
      this%ylabel_str = ""
      this%grid_enabled = .false.
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

   subroutine plotter_add_series(this, x, y, label, style)
      class(plotter_t), intent(inout) :: this
      real(wp), intent(in)            :: x(:), y(:)
      character(len=*), intent(in)    :: label
      character(len=*), intent(in)    :: style

      ! Lazily initialize pyplot on the first added series
      if (.not. this%is_initialized) then
         call this%plt%initialize(grid=this%grid_enabled, &
                                  xlabel=trim(this%xlabel_str), &
                                  ylabel=trim(this%ylabel_str), &
                                  title=trim(this%title_str), &
                                  legend=.true.)
         this%is_initialized = .true.
      end if

      this%series_count = this%series_count + 1

      call this%plt%add_plot(x, y, label=trim(label), linestyle=trim(style))
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
         ! Matplotlib is available, proceed with saving and auto-execution
         call this%plt%savefig(trim(filename), pyfile=trim(py_filename))
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

      if (.not. checked) then
         ! First attempt: try 'python3'
         call execute_command_line( &
            'python3 -c "import matplotlib.pyplot" > /dev/null 2>&1', &
            exitstat=exit_code)

         if (exit_code == 0) then
            available = .true.
            call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (python3) verified successfully.")
         else
            ! Second attempt: try 'python'
            call execute_command_line( &
               'python -c "import matplotlib.pyplot" > /dev/null 2>&1', &
               exitstat=exit_code)

            if (exit_code == 0) then
               available = .true.
               call global_logger%msg(LOG_INFO, "[PLOTTER] Matplotlib (python) verified successfully.")
            else
               available = .false.
               call global_logger%msg(LOG_WARN, "[PLOTTER] WARNING: Python or matplotlib not found.")
            end if
         end if
         checked = .true.
      end if

      is_available = available
   end function check_matplotlib

end module plotting_mod
