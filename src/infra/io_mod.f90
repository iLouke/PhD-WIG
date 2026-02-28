module io_mod
   !! Configuration and I/O Management Module
   !!
   !! Handles configuration file reading and parameter management.
   !! Supports INI-style configuration files with sections and key-value pairs.
   !! Section names support dot-notation (e.g. [section.aileron_right]) and are
   !! preserved exactly as written.

   use base_kinds_mod, only: wp, ip
   use logger_mod, only: global_logger, LOG_INFO, LOG_WARN, LOG_ERROR
   implicit none
   private

   ! Public Types and Subroutines
   public :: config_t

   ! Configuration parameter limits
   integer, parameter :: CONFIG_MAX_PARAMS = 100
   integer, parameter :: CONFIG_MAX_SECTIONS = 20
   integer, parameter :: CONFIG_MAX_LINE = 512
   integer, parameter :: CONFIG_MAX_KEY = 64
   integer, parameter :: CONFIG_MAX_VALUE = 256
   integer, parameter :: MAX_MESH_POINTS = 10000
   integer, parameter :: MAX_MESH_PANELS = 10000

   ! Maximum number of elements in a real array value (comma-separated)
   integer, parameter :: CONFIG_MAX_ARRAY = 64

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
      ! --- I/O ---
      procedure :: read => config_read
      procedure :: is_loaded => config_is_loaded
      procedure :: print_summary => config_print_summary
      ! --- Generic getters (section + key based) ---
      procedure :: get_bool => config_get_bool
      procedure :: get_logical => config_get_logical_value
      procedure :: get_int => config_get_integer_value
      procedure :: get_real => config_get_real_value
      procedure :: get_string => config_get_string_value
      procedure :: get_real_array => config_get_real_array
      ! --- Validation ---
      procedure :: validate => config_validate
   end type config_t

contains

   ! ===================================================================
   !  Internal helpers (module procedures, not type-bound)
   ! ===================================================================

   !> Internal helper: look up a raw string value from the parameter store
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

   !> Helper: convert a string to lower-case (ASCII)
   pure function to_lower(str) result(lstr)
      character(len=*), intent(in) :: str
      character(len=len(str)) :: lstr
      integer :: i, ic
      lstr = str
      do i = 1, len(str)
         ic = iachar(str(i:i))
         if (ic >= iachar('A') .and. ic <= iachar('Z')) then
            lstr(i:i) = achar(ic + 32)
         end if
      end do
   end function to_lower

   !> Check whether a line contains at least one alphabetic character.
   pure function line_has_alpha(line) result(has_alpha)
      character(len=*), intent(in) :: line
      logical :: has_alpha
      integer :: i
      character(len=1) :: c

      has_alpha = .false.
      do i = 1, len_trim(line)
         c = line(i:i)
         if ((c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')) then
            has_alpha = .true.
            return
         end if
      end do
   end function line_has_alpha

   !> Helper: count data rows in a CSV/text file.
   function count_csv_data_rows(filename) result(n_rows)
      character(len=*), intent(in) :: filename
      integer :: n_rows
      integer :: unit, ios
      character(len=CONFIG_MAX_LINE) :: line
      logical :: header_skipped

      n_rows = 0
      header_skipped = .false.

      open (newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
      if (ios /= 0) then
         n_rows = -1   ! signal: file not found
         return
      end if

      do
         read (unit, '(A)', iostat=ios) line
         if (ios /= 0) exit
         line = adjustl(line)

         if (len_trim(line) == 0) cycle
         if (line(1:1) == '#' .or. line(1:1) == '!') cycle

         if (.not. header_skipped) then
            if (line_has_alpha(line)) then
               header_skipped = .true.
               cycle
            else
               header_skipped = .true. ! It's numeric data, count it
            end if
         end if

         n_rows = n_rows + 1
      end do
      close (unit)
   end function count_csv_data_rows

   ! ===================================================================
   !  Type-bound procedures
   ! ===================================================================

   subroutine config_read(this, filename, log)
      class(config_t), intent(inout) :: this
      character(len=*), intent(in) :: filename
      logical, intent(in), optional :: log

      integer :: unit, stat, i
      character(len=CONFIG_MAX_LINE) :: line, current_section
      character(len=CONFIG_MAX_KEY) :: key
      character(len=CONFIG_MAX_VALUE) :: value

      if (.not. allocated(this%params)) allocate (this%params(CONFIG_MAX_PARAMS))

      this%filename = filename
      current_section = ""
      this%num_params = 0

      open (newunit=unit, file=trim(this%filename), status='old', action='read', iostat=stat)

      if (stat /= 0) then
         call global_logger%msg(LOG_WARN, "[IO] Config file not found: "//trim(this%filename)//". Using defaults.")
         this%loaded = .false.
         return
      end if

      do
         read (unit, '(A)', iostat=stat) line
         if (stat /= 0) exit

         line = adjustl(line)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == '!' .or. line(1:1) == '#') cycle

         if (line(1:1) == '[' .and. line(len_trim(line):len_trim(line)) == ']') then
            current_section = line(2:len_trim(line) - 1)
            cycle
         end if

         i = index(line, '=')
         if (i > 0) then
            key = adjustl(line(1:i - 1))
            value = adjustl(line(i + 1:len_trim(line)))

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
      call global_logger%msg(LOG_INFO, "[IO] Configuration file loaded: "//trim(this%filename))

      if (present(log)) then
         if (log) call this%print_summary()
      end if
   end subroutine config_read

   function config_is_loaded(this) result(loaded)
      class(config_t), intent(in) :: this
      logical :: loaded
      loaded = this%loaded
   end function config_is_loaded

   subroutine config_print_summary(this)
      class(config_t), intent(in) :: this
      integer :: i
      character(len=CONFIG_MAX_KEY) :: current_section

      if (.not. this%loaded) return

      call global_logger%msg(LOG_INFO, "[IO] ==========================================")
      call global_logger%msg(LOG_INFO, "[IO] Configuration Summary")
      call global_logger%msg(LOG_INFO, "[IO] ==========================================")

      current_section = ""
      do i = 1, this%num_params
         if (trim(this%params(i)%section) /= trim(current_section)) then
            current_section = trim(this%params(i)%section)
            call global_logger%msg(LOG_INFO, "[IO] ["//trim(current_section)//"]")
         end if
         call global_logger%msg(LOG_INFO, "[IO]   "//trim(this%params(i)%key)//" = "//trim(this%params(i)%value))
      end do
      call global_logger%msg(LOG_INFO, "[IO] ==========================================")
   end subroutine config_print_summary

   function config_get_bool(this, section, key, default) result(bool_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      logical, intent(in) :: default
      logical :: bool_result
      character(len=CONFIG_MAX_VALUE) :: str_value

      str_value = config_get_string_internal(this, section, key, "")

      if (len_trim(str_value) == 0) then
         bool_result = default
         return
      end if

      select case (trim(to_lower(str_value)))
      case ('.true.', 'true', 't', '1', 'yes', 'on')
         bool_result = .true.
      case ('.false.', 'false', 'f', '0', 'no', 'off')
         bool_result = .false.
      case default
         bool_result = default
      end select
   end function config_get_bool

   function config_get_logical_value(this, section, key, default) result(log_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      logical, intent(in) :: default
      logical :: log_result
      log_result = this%get_bool(section, key, default)
   end function config_get_logical_value

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

   ! FIX 2: Returns an allocatable string to prevent whitespace padding errors
   function config_get_string_value(this, section, key, default) result(str_result)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key, default
      character(len=:), allocatable :: str_result
      character(len=CONFIG_MAX_VALUE) :: str_value

      str_value = config_get_string_internal(this, section, key, default)
      allocate (character(len=len_trim(str_value)) :: str_result)
      str_result = trim(str_value)
   end function config_get_string_value

   ! FIX 1: out_array is restored to allocatable, and we use a temp_array
   subroutine config_get_real_array(this, section, key, default_array, out_array, n_elements)
      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: section, key
      real(wp), intent(in) :: default_array(:)
      real(wp), allocatable, intent(out) :: out_array(:)
      integer, intent(out) :: n_elements

      character(len=CONFIG_MAX_VALUE) :: str_value, token
      real(wp) :: temp_array(CONFIG_MAX_ARRAY)
      integer :: pos, ios, count

      str_value = config_get_string_internal(this, section, key, "")

      if (len_trim(str_value) == 0) then
         n_elements = size(default_array)
         allocate (out_array(n_elements))
         out_array = default_array
         return
      end if

      count = 0
      do while (len_trim(str_value) > 0 .and. count < CONFIG_MAX_ARRAY)
         pos = index(str_value, ',')
         if (pos > 0) then
            token = adjustl(str_value(1:pos - 1))
            str_value = adjustl(str_value(pos + 1:))
         else
            token = adjustl(str_value)
            str_value = ""
         end if

         if (len_trim(token) == 0) cycle

         read (token, *, iostat=ios) temp_array(count + 1)
         if (ios == 0) then
            count = count + 1
         else
            call global_logger%msg(LOG_WARN, "[IO] Bad real token: '"//trim(token)//"' skipped")
         end if
      end do

      if (count == 0) then
         n_elements = size(default_array)
         allocate (out_array(n_elements))
         out_array = default_array
      else
         n_elements = count
         allocate (out_array(n_elements))
         out_array(1:n_elements) = temp_array(1:count)
      end if
   end subroutine config_get_real_array

   subroutine config_validate(this)
      class(config_t), intent(in) :: this
      logical :: dynamics
      character(len=:), allocatable :: flight_profile, def_file
      integer :: n_rows, i
      logical :: file_exists
      character(len=16) :: n_rows_str

      if (.not. this%loaded) return

      ! ------------------------------------------------------------------
      ! 1. Validate Kinematics (These remain FATAL because we can't guess physics)
      ! ------------------------------------------------------------------
      dynamics = this%get_bool("kinematics", "dynamics", .false.)
      flight_profile = this%get_string("kinematics", "flight_profile", "")

      if (len_trim(flight_profile) > 0) then
         inquire (file=trim(flight_profile), exist=file_exists)
         if (.not. file_exists) then
            call global_logger%msg(LOG_ERROR, "[IO] FATAL: flight_profile missing: "//flight_profile)
            error stop "[IO] FATAL: flight_profile file missing."
         end if

         n_rows = count_csv_data_rows(flight_profile)
         write (n_rows_str, '(I0)') n_rows

         if (dynamics) then
            if (n_rows > 1) then
               call global_logger%msg(LOG_ERROR, "[IO] FATAL: dynamics=T but "//flight_profile//" has "//trim(n_rows_str)//" rows.")
               error stop "[IO] FATAL: flight_profile has too many rows for dynamic mode."
            end if
            call global_logger%msg(LOG_INFO, "[IO] validate: dynamics=T, IC rows="//trim(n_rows_str)//" OK")
         else
            if (n_rows <= 1) then
               call global_logger%msg(LOG_ERROR, "[IO] FATAL: dynamics=F but "//flight_profile//" has "//trim(n_rows_str)//" rows.")
               error stop "[IO] FATAL: flight_profile needs multiple rows for prescribed mode."
            end if
            call global_logger%msg(LOG_INFO, "[IO] validate: dynamics=F, trajectory rows="//trim(n_rows_str)//" OK")
         end if
      end if

      ! ------------------------------------------------------------------
      ! 2. Validate Control Surface Schedules (Fallback to Constant)
      ! ------------------------------------------------------------------
      do i = 1, this%num_params
         if (trim(this%params(i)%key) == "deflection_type") then
            if (trim(this%params(i)%value) == "file") then

               def_file = this%get_string(trim(this%params(i)%section), "deflection_data", "")

               if (len_trim(def_file) == 0) then
                  call global_logger%msg(LOG_WARN, "[IO] WARNING: deflection_data missing in ["//trim(this%params(i)%section)//"]")
                  call global_logger%msg(LOG_WARN, "[IO] WARNING: Surface will be held CONSTANT at 0.0 degrees.")
                  cycle
               end if

               inquire (file=trim(def_file), exist=file_exists)
               if (.not. file_exists) then
                  call global_logger%msg(LOG_WARN, "[IO] WARNING: Control surface file missing: "//def_file)
        call global_logger%msg(LOG_WARN, "[IO] WARNING: ["//trim(this%params(i)%section)//"] will be held CONSTANT at 0.0 degrees.")
               else
                  n_rows = count_csv_data_rows(def_file)
                  write (n_rows_str, '(I0)') n_rows
     call global_logger%msg(LOG_INFO, "[IO] validate: ["//trim(this%params(i)%section)//"] schedule rows="//trim(n_rows_str)//" OK")
               end if

            end if
         end if
      end do

   end subroutine config_validate

end module io_mod
