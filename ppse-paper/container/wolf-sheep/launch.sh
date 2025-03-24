docker build . -t wolf-sheep && \
(docker rm -f wolf-sheep || true) && \
docker run -d --name wolf-sheep wolf-sheep sleep inf
