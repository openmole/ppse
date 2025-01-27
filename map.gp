set datafile separator ","
set view 26,160,1,1
unset key
#set terminal png rounded size 1000,1000
#set output '/tmp/map.png'
set style line 1 pt 7 lc -1 ps 1;
set ylabel 'x'
set xlabel 'y'
#set format x '%.2t*10^%+03T';
#set format y '%.2t*10^%+03T';
set pm3d map
set size square
#set cbrange [0:0.2]   # Fix color map interval to 0 to 0.2
splot "/tmp/seir-ppse/0.csv" with points ls 1 palette

