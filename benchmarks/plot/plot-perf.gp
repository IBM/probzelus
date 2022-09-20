if (!exists("ex")) ex='coin'
dir='../'.ex.'/'

set datafile separator comma
set key autotitle columnhead samplen 2

set terminal png
set output ex.'-perf.png'

set xlabel 'Number of Particles'
set logscale x
set logscale y
set ylabel 'Execution time (ms)'
set title ex.': Performance'

plot dir.'particles/perf.csv' using 1:3:2:4 with yerrorbars lt 3 pointtype 5 title 'PF', \
     dir.'ds/perf.csv' using 1:3:2:4 with yerrorbars lt 1 pointtype 11 title 'SDS', \
     dir.'semi_symb/perf.csv' using 1:3:2:4 with yerrorbars lt 5 pointtype 13 title 'SSDS'

#     dir.'ds_bounded/perf.csv' using 1:3:2:4 with yerrorbars lt 4 pointtype 7 title 'BDS', \
