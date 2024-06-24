docker build . -t traffic && \
docker rm -f traffic && \
docker run -d --name traffic traffic sleep inf
#docker save traffic -o /tmp/traffic.tar
#singularity build --sandbox traffic_box docker-archive:///tmp/traffic.tar
