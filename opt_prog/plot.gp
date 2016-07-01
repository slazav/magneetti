#!/usr/bin/gnuplot

#set terminal fig metric color
#set output "pl_a.fig"
plot\
 "sol1.fld" using 1:3 with lines title "calculated data - no grad, no quad",\
 "sol1.fit" using 1:2 with lines lc 0 title "constant fit"
pause -1

#set output "pl_b.fig"
plot\
 "sol2.fld" using 1:3 with lines title "calculated data - grad",\
 "sol2.fit" using 1:2 with lines lc 0 title "gradient fit"
pause -1

#set output "pl_c.fig"
plot\
 "sol3.fld" using 1:3 with lines title "calculated data - quad",\
 "sol3.fit" using 1:2 with lines lc 0 title "quadratic fit, r=0",\
 "sol3.fit" using 1:3 with lines lc 3 title "quadratic fit, r=r0"
pause -1

#set output "pl_d.fig"
plot\
 "sol4.fld" using 1:3 with lines title "calculated data - quad",\
 "sol4.fit" using 1:2 with lines lc 0 title "quadratic fit, r=0",\
 "sol4.fit" using 1:3 with lines lc 3 title "quadratic fit, r=r0"
pause -1