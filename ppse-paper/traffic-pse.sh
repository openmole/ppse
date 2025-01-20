(cd ./container && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.trafficBenchmarkPSE /tmp/traffic-pse 500 20")
