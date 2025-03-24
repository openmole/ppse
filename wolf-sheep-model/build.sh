docker build . -t wolf-sheep
docker rm -f wolf-sheep
docker run -d --name wolf-sheep wolf-sheep sleep inf
#docker save traffic -o /tmp/traffic.tar
#singularity build --sandbox traffic_box docker-archive:///tmp/traffic.tar
