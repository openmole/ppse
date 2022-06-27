
sbt "project ppse" "runMain ppse.test.benchmarkPPSE --map /tmp/map.csv"
gnuplot -s map.gp -p
