set term postscript enhanced color 24
set size 1,1

set output "BandStructure.eps"
set title "Band Structure (VASP)"
set ylabel "Energy (eV)"
set noxtics
set xrange [0:0.7169]
set yrange [-10:10]
#set label "{/Symbol G}" at 0, graph -0.05 
#set label "H" at 0.5747, graph -0.05 
#set label "N" at 1.011386, graph -0.05 
#set label "{/Symbol G}" at 1.4391, graph -0.05 
#set label "P" at 1.958, graph -0.05 
#set label "H" at 2.482, graph -0.05 
#set label "P" at 2.7958, graph -0.05 
#set label "N" at 1.4391, graph -0.05 
set arrow from 0.574, graph 0 to 0.574, graph 1 nohead 
set arrow from 1.011, graph 0 to 1.011, graph 1 nohead 
set arrow from 1.439, graph 0 to 1.439, graph 1 nohead 
set arrow from 1.958, graph 0 to 1.958, graph 1 nohead 
set arrow from 2.482, graph 0 to 2.482, graph 1 nohead 
set arrow from 2.795, graph 0 to 2.795, graph 1 nohead 
plot "bnd.dat" using ($1):($2) notitle w l lt 1 lw 4
