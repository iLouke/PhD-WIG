module timer_mod
    use base_kinds_mod, only: wp
    implicit none
    private
    
    public :: timer_t

    type :: timer_t
        private
        integer(8) :: start_count = 0
        integer(8) :: end_count   = 0
        integer(8) :: rate        = 0
        real(wp)   :: elapsed     = 0.0_wp
        logical    :: running     = .false.
    contains
        procedure :: start  => timer_start
        procedure :: stop   => timer_stop
        procedure :: reset  => timer_reset
        procedure :: report => timer_report
    end type timer_t

contains

    subroutine timer_start(this)
        class(timer_t), intent(inout) :: this
        if (this%running) return
        
        call system_clock(this%start_count, this%rate)
        this%running = .true.
    end subroutine timer_start

    subroutine timer_stop(this)
        class(timer_t), intent(inout) :: this
        if (.not. this%running) return
        
        call system_clock(this%end_count)
        this%elapsed = this%elapsed + &
            real(this%end_count - this%start_count, wp) / real(this%rate, wp)
        this%running = .false.
    end subroutine timer_stop

    subroutine timer_reset(this)
        class(timer_t), intent(inout) :: this
        this%elapsed = 0.0_wp
        this%running = .false.
    end subroutine timer_reset

    function timer_report(this) result(seconds)
        class(timer_t), intent(in) :: this
        real(wp) :: seconds
        seconds = this%elapsed
    end function timer_report

end module timer_mod
