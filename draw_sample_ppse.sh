
sbt "project ppse" "runMain ppse.test.SamplePPSE --map /tmp/map.csv"
gnuplot -s map.gp -p
