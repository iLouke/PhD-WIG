module io_mod
   !! Configuration and I/O Management Module
   !!
   !! Handles configuration file reading and parameter management.
   !! Supports INI-style configuration files with sections and key-value pairs.
   !!
   !! Example config file:
   !!   [logger]
   !!   enabled = .true.
   !!   level = 0
   !!   file = uvlm.log
   !!   console = .true.
   !!
   !!   [plotter]
   !!   enabled = .true.
   !!   format = png

   use base_kinds_mod, only: wp, ip
   implicit none
   private

   ! Public Types and Subroutines
   public :: config_t
   public :: config_read
   public :: mesh_point_t, mesh_panel_t
   public :: read_points_file, read_panels_file

   ! Configuration parameter limits
   integer, parameter :: CONFIG_MAX_PARAMS = 100
   integer, parameter :: CONFIG_MAX_SECTIONS = 20
   integer, parameter :: CONFIG_MAX_LINE = 512
   integer, parameter :: CONFIG_MAX_KEY = 64
   integer, parameter :: CONFIG_MAX_VALUE = 256
   integer, parameter :: MAX_MESH_POINTS = 10000
   integer, parameter :: MAX_MESH_PANELS = 10000

   ! Mesh Point Type
   type :: mesh_point_t
      integer(ip) :: id
      real(wp) :: x, y, z
      integer(ip) :: marked
   end type mesh_point_t

   ! Mesh Panel Type
   type :: mesh_panel_t
      integer(ip) :: id
      integer(ip) :: nodes(4)
      integer(ip) :: section
      real(wp) :: normal(3)
      real(wp) :: collocation(3)
      real(wp) :: area
   end type mesh_panel_t

   ! Configuration Parameter Type
   type :: config_param_t
      character(len=CONFIG_MAX_KEY) :: key = ""
      character(len=CONFIG_MAX_VALUE) :: value = ""
      character(len=CONFIG_MAX_KEY) :: section = ""
   end type config_param_t

   ! Main Configuration Type
   type :: config_t
      private
      type(config_param_t), allocatable :: params(:)
      integer(ip) :: num_params = 0
      character(len=256) :: filename = ""
      logical :: loaded = .false.
   contains
      procedure :: read => config_read_file
      procedure :: get_logical => config_get_logical_value
      procedure :: get_integer => config_get_integer_value
      procedure :: get_real => config_get_real_value
      procedure :: get_string => config_get_string_value
      procedure :: is_loaded => config_is_loaded
      procedure :: print_summary => config_print_summary
   end type config_t

contains

   !> Initialize and read configuration file
   !! @param filename Path to configuration file
   !! @return config Configuration object
   subroutine config_read(config, filename)
      type(config_t), intent(inout) :: config
      character(len=*), intent(in) :: filename

      allocate (config%params(CONFIG_MAX_PARAMS))
      config%filename = filename
      call config%read()
   end subroutine config_read

   !> Read configuration file (internal subroutine)
   !! Parses INI-style configuration files with [section] headers
   subroutine config_read_file(this)
      class(config_t), intent(inout) :: this
      integer :: unit, stat, i
      character(len=CONFIG_MAX_LINE) :: line, current_section
      character(len=CONFIG_MAX_KEY) :: key
      character(len=CONFIG_MAX_VALUE) :: value

      current_section = ""
      this%num_params = 0

      ! Try to open config file
      open (newunit=unit, file=trim(this%filename), status='old', &
            action='read', iostat=stat)

      if (stat /= 0) then
         print '(A,A,A)', "[IO] WARNING: Config file not found: ", trim(this%filename), &
            ". Using defaults."
         this%loaded = .false.
         return
      end if

      ! Read file line by line
      do
         read (unit, '(A)', iostat=stat) line
         if (stat /= 0) exit

         ! Skip empty lines and comments
         line = adjustl(line)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '!' .or. line(1:1) == '#') cycle

         ! Check for section header [section]
         if (line(1:1) == '[' .and. line(len_trim(line):len_trim(line)) == ']') then
            current_section = line(2:len_trim(line) - 1)
            cycle
         end if

         ! Parse key=value pairs
         i = index(line, '=')
         if (i > 0) then
            key = adjustl(line(1:i - 1))
            value = adjustl(line(i + 1:len_trim(line)))

            ! Store parameter
            if (this%num_params < CONFIG_MAX_PARAMS) then
               this%num_params = this%num_params + 1
               this%params(this%num_params)%key = trim(key)
               this%params(this%num_params)%value = trim(value)
               this%params(this%num_params)%section = trim(current_section)
            end if
         end if
      end do

      close (unit)
      this%loaded = .true.

      print '(A,I0,A,A)', "[IO] Loaded ", this%num_params, " parameters from: ", &
         trim(this%filename)

   end subroutine config_read_file

   !> Get boolean configuration value
   !! @param section Configuration section name
   !! @param key Parameter key
   !! @param default Default value if not found
   !! @return bool_result Boolean value
   function config_get_bool(this, section, key, default) result(bool_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      logical, intent(in) :: default
      logical :: bool_result
      character(len=CONFIG_MAX_VALUE) :: str_value

      str_value = config_get_string_internal(this, section, key, "")

      if (len_trim(str_value) == 0) then
         bool_result = default
      else
         select case (trim(str_value))
         case ('.true.', 'true', 'True', 'TRUE', 'T', '1', 'yes', 'YES')
            bool_result = .true.
         case ('.false.', 'false', 'False', 'FALSE', 'F', '0', 'no', 'NO')
            bool_result = .false.
         case default
            bool_result = default
         end select
      end if
   end function config_get_bool

   !> Get logical (boolean) configuration value
   function config_get_logical_value(this, section, key, default) result(log_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      logical, intent(in) :: default
      logical :: log_result
      log_result = config_get_bool(this, section, key, default)
   end function config_get_logical_value

   !> Get integer configuration value
   function config_get_integer_value(this, section, key, default) result(int_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      integer(ip), intent(in) :: default
      integer(ip) :: int_result
      character(len=CONFIG_MAX_VALUE) :: str_value
      integer :: ios

      str_value = config_get_string_internal(this, section, key, "")

      if (len_trim(str_value) == 0) then
         int_result = default
      else
         read (str_value, *, iostat=ios) int_result
         if (ios /= 0) int_result = default
      end if
   end function config_get_integer_value

   !> Get real configuration value
   function config_get_real_value(this, section, key, default) result(real_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      real(wp), intent(in) :: default
      real(wp) :: real_result
      character(len=CONFIG_MAX_VALUE) :: str_value
      integer :: ios

      str_value = config_get_string_internal(this, section, key, "")

      if (len_trim(str_value) == 0) then
         real_result = default
      else
         read (str_value, *, iostat=ios) real_result
         if (ios /= 0) real_result = default
      end if
   end function config_get_real_value

   !> Get string configuration value
   function config_get_string_value(this, section, key, default) result(str_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      character(len=*), intent(in) :: default
      character(len=:), allocatable :: str_result
      character(len=CONFIG_MAX_VALUE) :: str_value

      str_value = config_get_string_internal(this, section, key, default)
      allocate (character(len=len_trim(str_value)) :: str_result)
      str_result = trim(str_value)
   end function config_get_string_value

   !> Internal helper: get string value
   function config_get_string_internal(config, section, key, default) &
      result(str_res)
      type(config_t), intent(in) :: config
      character(len=*), intent(in) :: section, key, default
      character(len=CONFIG_MAX_VALUE) :: str_res
      integer :: i

      str_res = trim(default)

      if (.not. config%loaded) return

      do i = 1, config%num_params
         if (trim(config%params(i)%section) == trim(section) .and. &
             trim(config%params(i)%key) == trim(key)) then
            str_res = trim(config%params(i)%value)
            return
         end if
      end do
   end function config_get_string_internal

   !> Check if configuration was successfully loaded
   function config_is_loaded(this) result(loaded)
      class(config_t), intent(in) :: this
      logical :: loaded
      loaded = this%loaded
   end function config_is_loaded

   !> Print configuration summary
   subroutine config_print_summary(this)
      class(config_t), intent(in) :: this
      integer :: i
      character(len=CONFIG_MAX_KEY) :: current_section

      if (.not. this%loaded) then
         print *, "[IO] Configuration not loaded"
         return
      end if

      print *, "[IO] =========================================="
      print *, "[IO] Configuration Summary"
      print *, "[IO] =========================================="

      current_section = ""

      do i = 1, this%num_params
         if (trim(this%params(i)%section) /= trim(current_section)) then
            current_section = trim(this%params(i)%section)
            print '(A,A,A)', "[IO] [", trim(current_section), "]"
         end if
         print '(A,A,A,A)', "[IO]   ", trim(this%params(i)%key), &
            " = ", trim(this%params(i)%value)
      end do

      print *, "[IO] =========================================="
   end subroutine config_print_summary

   !> Read points from file
   !! Format: ID X Y Z MARKED
   subroutine read_points_file(filename, points, n_points)
      character(len=*), intent(in) :: filename
      type(mesh_point_t), allocatable, intent(out) :: points(:)
      integer(ip), intent(out) :: n_points
      integer :: unit, ios
      integer(ip) :: i, id, marked
      real(wp) :: x, y, z

      ! Count lines first
      open (newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print '(A,A)', "[IO] ERROR: Cannot open points file: ", trim(filename)
         n_points = 0
         allocate (points(0))
         return
      end if

      n_points = 0
      do
         read (unit, *, iostat=ios) id, x, y, z, marked
         if (ios /= 0) exit
         n_points = n_points + 1
      end do

      allocate (points(n_points))

      ! Read points
      rewind (unit)
      do i = 1, n_points
         read (unit, *, iostat=ios) id, x, y, z, marked
         if (ios /= 0) exit
         points(i)%id = id
         points(i)%x = x
         points(i)%y = y
         points(i)%z = z
         points(i)%marked = marked
      end do

      close (unit)
      print '(A,I0,A,A)', "[IO] Loaded ", n_points, " points from: ", trim(filename)
   end subroutine read_points_file

   !> Read panels from file
   !! Format: ID P1 P2 P3 P4 SECTION NX NY NZ CX CY CZ AREA
   subroutine read_panels_file(filename, panels, n_panels)
      character(len=*), intent(in) :: filename
      type(mesh_panel_t), allocatable, intent(out) :: panels(:)
      integer(ip), intent(out) :: n_panels
      integer :: unit, ios
      integer(ip) :: i, id, p1, p2, p3, p4, section
      real(wp) :: nx, ny, nz, cx, cy, cz, area

      ! Count lines first
      open (newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print '(A,A)', "[IO] ERROR: Cannot open panels file: ", trim(filename)
         n_panels = 0
         allocate (panels(0))
         return
      end if

      n_panels = 0
      do
         read (unit, *, iostat=ios) id, p1, p2, p3, p4, section, nx, ny, nz, cx, cy, cz, area
         if (ios /= 0) exit
         n_panels = n_panels + 1
      end do

      allocate (panels(n_panels))

      ! Read panels
      rewind (unit)
      do i = 1, n_panels
         read (unit, *, iostat=ios) id, p1, p2, p3, p4, section, nx, ny, nz, cx, cy, cz, area
         if (ios /= 0) exit
         panels(i)%id = id
         panels(i)%nodes(1) = p1
         panels(i)%nodes(2) = p2
         panels(i)%nodes(3) = p3
         panels(i)%nodes(4) = p4
         panels(i)%section = section
         panels(i)%normal(1) = nx
         panels(i)%normal(2) = ny
         panels(i)%normal(3) = nz
         panels(i)%collocation(1) = cx
         panels(i)%collocation(2) = cy
         panels(i)%collocation(3) = cz
         panels(i)%area = area
      end do

      close (unit)
      print '(A,I0,A,A)', "[IO] Loaded ", n_panels, " panels from: ", trim(filename)
   end subroutine read_panels_file

end module io_mod
