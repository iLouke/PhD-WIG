module logger_mod
    use base_kinds_mod, only: wp
    implicit none
    private

    public :: logger_t
    public :: LOG_DEBUG, LOG_INFO, LOG_WARN, LOG_ERROR

    integer, parameter :: LOG_DEBUG = 0
    integer, parameter :: LOG_INFO  = 1
    integer, parameter :: LOG_WARN  = 2
    integer, parameter :: LOG_ERROR = 3

    type :: logger_t
        private
        integer :: file_unit = -1
        integer :: level     = LOG_INFO
        character(len=256) :: filepath = ""
    contains
        procedure :: init    => logger_init
        procedure :: log_msg => logger_write
        procedure :: finalize => logger_close
    end type logger_t

contains

    subroutine logger_init(this, filename, level)
        class(logger_t), intent(inout) :: this
        character(len=*), intent(in)   :: filename
        integer, intent(in), optional  :: level
        
        ! 1. Create output directory (Unix command)
        call execute_command_line("mkdir -p output")

        ! 2. Set path to output/filename
        this%filepath = "output/" // trim(filename)

        if (present(level)) this%level = level

        open(newunit=this%file_unit, file=trim(this%filepath), &
             status='unknown', position='append', action='write')

        call this%log_msg(LOG_INFO, "--- Logger Initialized ---")
    end subroutine logger_init

    subroutine logger_write(this, level, message)
        class(logger_t), intent(in)  :: this
        integer, intent(in)          :: level
        character(len=*), intent(in) :: message
        
        character(len=10) :: lvl_str
        character(len=8)  :: date_str, time_str
        character(len=30) :: timestamp

        if (level < this%level) return

        select case (level)
            case (LOG_DEBUG); lvl_str = "[DEBUG]"
            case (LOG_INFO);  lvl_str = "[INFO] "
            case (LOG_WARN);  lvl_str = "[WARN] "
            case (LOG_ERROR); lvl_str = "[ERROR]"
        end select

        call date_and_time(date=date_str, time=time_str)
        timestamp = date_str(1:4)//"-"//date_str(5:6)//"-"//date_str(7:8)//" "&
                  //time_str(1:2)//":"//time_str(3:4)//":"//time_str(5:6)

        ! Terminal Output
        write(*, '(A, " ", A, " : ", A)') trim(timestamp), lvl_str, trim(message)

        ! File Output
        if (this%file_unit /= -1) then
            write(this%file_unit, '(A, " ", A, " : ", A)') trim(timestamp), lvl_str, trim(message)
            flush(this%file_unit)
        end if
    end subroutine logger_write

    subroutine logger_close(this)
        class(logger_t), intent(inout) :: this
        if (this%file_unit /= -1) then
            call this%log_msg(LOG_INFO, "--- Logger Finalized ---")
            close(this%file_unit)
            this%file_unit = -1
        end if
    end subroutine logger_close

end module logger_mod