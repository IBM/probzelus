if (!exists("ex")) ex='coin'
dir='../'.ex.'/'


set datafile separator comma
set key autotitle columnhead samplen 2

set terminal png
set output ex.'-mem-ideal.png'

set xlabel 'Step'
set logscale y
set ylabel 'Words in heap (in thousands, log scale)'
set title ex.': Ideal Memory'

plot dir.'particles/mem-ideal.csv' using 1:3 every 10 lt 3 pt 7 title 'PF', \
     dir.'ds/mem-ideal.csv' using 1:3 every 10 lt 1 pt 11 title 'SDS', \
     dir.'semi_symb/mem-ideal.csv' using 1:3 every 10 lt 5 pt 13 title 'SSDS'

#     dir.'ds_bounded/mem-ideal.csv' using 1:3 every 10 lt 4 pt 5 title 'BDS', \
#     dir.'ds_nogc/mem-ideal.csv' using 1:3 every 10 lt 2 pt 9 title 'DS', \
