(cd ./container/wolf-sheep && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepBenchmarkPPSE /tmp/wolf-sheep-ppse 1000 1 s10w10") && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepBenchmarkPPSE /tmp/wolf-sheep-ppse 1000 1 s90w90") && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepBenchmarkPPSE /tmp/wolf-sheep-ppse 1000 1 s10s90") && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepBenchmarkPPSE /tmp/wolf-sheep-ppse 1000 1 w10w90") && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepBenchmarkPPSE /tmp/wolf-sheep-ppse 1000 20 s50w50") 
