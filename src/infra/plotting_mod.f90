module plotting_mod
    use base_kinds_mod, only: wp
    implicit none
    private
    public :: plotter_t

    type :: plotter_t
        private
        integer :: script_unit = -1
        character(len=256) :: script_file = "output/plot_script.gp"
        character(len=4096) :: plot_cmd = "" 
        integer :: series_count = 0
    contains
        procedure :: figure     => plotter_figure
        procedure :: title      => plotter_title
        procedure :: xlabel     => plotter_xlabel
        procedure :: ylabel     => plotter_ylabel
        procedure :: grid       => plotter_grid
        procedure :: xrange     => plotter_xrange
        procedure :: yrange     => plotter_yrange
        procedure :: add        => plotter_add_series
        procedure :: render     => plotter_render
    end type plotter_t

contains

    subroutine plotter_figure(this)
        class(plotter_t), intent(inout) :: this
        
        ! Ensure directory exists (Linux/Unix command)
        call execute_command_line("mkdir -p output")
        
        open(newunit=this%script_unit, file=this%script_file, status='replace')
        
        write(this%script_unit, '(A)') "set term qt size 900,600 font 'Arial,11'"
        write(this%script_unit, '(A)') "set key top right box opaque"
        
        this%plot_cmd = ""
        this%series_count = 0
    end subroutine plotter_figure

    subroutine plotter_title(this, text)
        class(plotter_t), intent(inout) :: this
        character(len=*), intent(in)    :: text
        write(this%script_unit, '(A,A,A)') "set title '", trim(text), "'"
    end subroutine plotter_title

    subroutine plotter_xlabel(this, text)
        class(plotter_t), intent(inout) :: this
        character(len=*), intent(in)    :: text
        write(this%script_unit, '(A,A,A)') "set xlabel '", trim(text), "'"
    end subroutine plotter_xlabel

    subroutine plotter_ylabel(this, text)
        class(plotter_t), intent(inout) :: this
        character(len=*), intent(in)    :: text
        write(this%script_unit, '(A,A,A)') "set ylabel '", trim(text), "'"
    end subroutine plotter_ylabel

    subroutine plotter_grid(this, enable)
        class(plotter_t), intent(inout) :: this
        logical, intent(in) :: enable
        if (enable) then
            write(this%script_unit, '(A)') "set grid"
        else
            write(this%script_unit, '(A)') "unset grid"
        end if
    end subroutine plotter_grid
    
    subroutine plotter_xrange(this, min_val, max_val)
        class(plotter_t), intent(inout) :: this
        real(wp), intent(in) :: min_val, max_val
        write(this%script_unit, '(A,F12.4,A,F12.4,A)') "set xrange [", min_val, ":", max_val, "]"
    end subroutine plotter_xrange  ! <--- Fixed Label

    subroutine plotter_yrange(this, min_val, max_val)
        class(plotter_t), intent(inout) :: this
        real(wp), intent(in) :: min_val, max_val
        write(this%script_unit, '(A,F12.4,A,F12.4,A)') "set yrange [", min_val, ":", max_val, "]"
    end subroutine plotter_yrange  ! <--- Fixed Label

    subroutine plotter_add_series(this, x, y, label, style)
        class(plotter_t), intent(inout) :: this
        real(wp), intent(in)            :: x(:), y(:)
        character(len=*), intent(in)    :: label
        character(len=*), intent(in)    :: style
        
        integer :: d_unit, i
        character(len=256) :: filename
        character(len=10) :: num_str

        this%series_count = this%series_count + 1
        write(num_str, '(I0)') this%series_count
        
        ! Save data to output/data_N.dat
        filename = "output/data_" // trim(num_str) // ".dat"
        
        open(newunit=d_unit, file=filename, status='replace')
        do i = 1, size(x)
            write(d_unit, *) x(i), y(i)
        end do
        close(d_unit)

        if (this%series_count > 1) then
            this%plot_cmd = trim(this%plot_cmd) // ", "
        end if
        
        ! The script refers to the file by its relative path 'output/data_N.dat'
        this%plot_cmd = trim(this%plot_cmd) // " '" // trim(filename) // "' " // &
                        "title '" // trim(label) // "' with " // trim(style)
        
    end subroutine plotter_add_series

    subroutine plotter_render(this)
        class(plotter_t), intent(inout) :: this
        
        if (this%series_count > 0) then
            write(this%script_unit, '(A, A)') "plot ", trim(this%plot_cmd)
        end if
        
        close(this%script_unit)
        call execute_command_line("gnuplot -persist " // trim(this%script_file))
    end subroutine plotter_render

end module plotting_mod