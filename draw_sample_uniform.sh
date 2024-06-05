
sbt "project ppse" "runMain ppse.test.sampleUniform --map /tmp/map.csv"
gnuplot -s map.gp -p
