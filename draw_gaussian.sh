sbt "runMain ppse.test.DrawGaussianMixture /tmp/map.csv"
gnuplot -s map.gp -p
