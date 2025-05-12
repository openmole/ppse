(cd ./container/wolf-sheep && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepBenchmarkPPSE /tmp/wolf-sheep-ppse 1000 20") 
