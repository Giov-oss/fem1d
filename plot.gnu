 set terminal wxt size 1000,800
 set bmargin 5
 set xlabel "x"
 set ylabel "Temperatura"
 set title " "
 set grid
 unset key
 set multiplot layout 2, 1
 plot for [col=3:         400 ] "data/res.dat" using 2:col  with lines lw 2
  plot "data/res.dat" using 2:         400 with lines lw 2
 unset multiplot
 pause mouse
