set term wxt enhanced 0 font "Calibri,14" size 640,420



set grid
set xrange [*:*]
set xlabel ""
unset log x
set yrange [*:*]
set ylabel ""
unset log y
set border back
set border 31
set key off
plot  "-"  using 2:xtic(1)  with boxes  fill solid   0.200000003 notitle lc rgb "#0000FF" axes x1y1
"Label 1"	   8.6990874278022878E-002	
"Label 2"	  0.35338872273558908	
"Label 3"	  0.73660080565792341	
"Label 4"	  0.48806058091840576	
"Label 5"	  0.99207258702014245	
e
