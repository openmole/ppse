(cd ./container/traffic && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.trafficBenchmarkPPSE /tmp/traffic-ppse 1000 20")
