docker build . -t traffic && \
(docker rm -f traffic || true) && \
docker run -d --name traffic traffic sleep inf
