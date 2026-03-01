module timer_mod
   use base_kinds_mod
   implicit none
   private

   public :: timer_t

   type :: timer_t
      private
      integer(i8) :: start_tick = 0
      integer(i8) :: total_ticks = 0
      integer(i8) :: rate = 1
      logical     :: running = .false.
   contains
      procedure :: start => timer_start
      procedure :: stop => timer_stop
      procedure :: reset => timer_reset
      procedure :: report => timer_report
   end type timer_t

contains

   subroutine timer_start(this)
      class(timer_t), intent(inout) :: this
      integer(i8) :: now

      if (this%running) return

      call system_clock(count=now, count_rate=this%rate)
      this%start_tick = now
      this%running = .true.
   end subroutine timer_start

   subroutine timer_stop(this)
      class(timer_t), intent(inout) :: this
      integer(i8) :: now

      if (.not. this%running) return

      call system_clock(count=now)
      this%total_ticks = this%total_ticks + (now - this%start_tick)
      this%running = .false.
   end subroutine timer_stop

   subroutine timer_reset(this)
      class(timer_t), intent(inout) :: this
      this%total_ticks = 0
      this%running = .false.
   end subroutine timer_reset

   !> Returns elapsed time in seconds.
   !> Works even if the timer is still running.
   function timer_report(this) result(seconds)
      class(timer_t), intent(in) :: this
      real(wp) :: seconds
      integer(i8) :: now, current_total

      if (this%running) then
         call system_clock(count=now)
         current_total = this%total_ticks + (now - this%start_tick)
      else
         current_total = this%total_ticks
      end if

      if (this%rate > 0) then
         seconds = real(current_total, wp)/real(this%rate, wp)
      else
         seconds = 0.0_wp
      end if
   end function timer_report

end module timer_mod
