set terminal postscript eps enhanced color solid rounded "Helvetica" 17
set output 'speedup--vs--processors--for--quicksort--size=2000000--param=0--optim=0--machine=srv-53-07.eps'
set size 0.55,0.55
set pointsize 2
set title 'quicksort (Intel)'
set xrange [0:]
set yrange [0:]
set key top left
set xlabel 'processors'
set ylabel 'speedup'

plot 'speedup--vs--processors--for--quicksort--size=2000000--param=0--optim=0--machine=srv-53-07--ws.dat'  with linespoints linewidth 3  title 'ws', 'speedup--vs--processors--for--quicksort--size=2000000--param=0--optim=0--machine=srv-53-07--oracle.dat'  with linespoints linewidth 3  title 'oracle', x with lines title 'linear'
