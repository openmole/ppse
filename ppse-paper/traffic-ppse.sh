(cd ./container && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.trafficBenchmarkPPSE /tmp/traffic-ppse 500 20")
