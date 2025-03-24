(cd ./container/traffic && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.trafficRandom /tmp/traffic-random.csv 1000000")
