set terminal eps
set output 'bench_micro.eps'

set key left top

set xlabel "Arity"

set autoscale
set yrange [0:]

set ylabel "Time (ns)"

#set title "Mean FFI Call Latency by Arity"

set style data linespoints

plot "staged_functor.txt"     using 1:2 title "Cmeleon Staged", \
     "traditional.txt"        using 1:2 title "OCaml Manual", \
     "cowboy.txt"             using 1:2 title "OCaml Expert"
