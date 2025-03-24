(cd ./container/wolf-sheep && ./launch.sh) && \
(cd .. && sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.wolfSheepRandom /tmp/wolf-sheep-random.csv 1000000")
