
sbt "runMain ppse.test.SampleMultiGaussianApp /tmp/map.csv"
gnuplot -s map.gp -p
