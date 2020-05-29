module plot
    contains
    subroutine gnuplot(time_steps)
    implicit none
    integer,intent(in) :: time_steps
    
    open(42,file='plot.gnu',status='unknown',action='write')
    
    write(42,*) 'set terminal wxt size 1000,800'
    write(42,*) 'set bmargin 5'
    write(42,*) 'set xlabel "x"'
    write(42,*) 'set ylabel "Temperatura"'
    write(42,*) 'set title " "'
    write(42,*) 'set grid'
    write(42,*) 'unset key'
    write(42,*) 'set multiplot layout 2, 1'
    write(42,*) 'plot for [col=3:', time_steps+2, '] "data/res.dat" using 2:col  with lines lw 2'
    write(42,*) ' plot "data/res.dat" using 2:', time_steps+2, 'with lines lw 2'
    write(42,*) 'unset multiplot'
    write(42,*) 'pause mouse'

    close(42)

    call system('gnuplot plot.gnu')
    
    end subroutine gnuplot

end module