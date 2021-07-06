
sbt "runMain ppse.test.SampleUniformApp --map /tmp/map.csv"
gnuplot -s map.gp -p
