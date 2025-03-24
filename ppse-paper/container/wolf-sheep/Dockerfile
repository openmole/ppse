FROM debian:testing-slim as build

MAINTAINER "Romain Reuillon <romain.reuillon@openmole.org"
    
RUN apt-get update \ 
	&& apt-get install -y --no-install-recommends locales ca-certificates fakeroot sudo aria2 curl \
	&& rm -rf /var/lib/apt/lists/*

RUN curl -sSLf https://virtuslab.github.io/scala-cli-packages/scala-setup.sh | sh
RUN mv /root/.cache/scalacli/local-repo/bin/scala-cli/scala-cli /bin/

ARG NETLOGO_HOME=/netlogo
ARG NETLOGO_VERSION=6.4.0

ENV LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    NETLOGO_TARBALL=NetLogo-$NETLOGO_VERSION-64.tgz

ENV NETLOGO_URL=https://ccl.northwestern.edu/netlogo/$NETLOGO_VERSION/$NETLOGO_TARBALL

RUN aria2c -x 16 -s 16 -o $NETLOGO_TARBALL $NETLOGO_URL && \ 
    tar xzf $NETLOGO_TARBALL

RUN mv "NetLogo-$NETLOGO_VERSION-64" netlogo && \
    rm -rf "$NETLOGO_HOME/lib/runtime" && \
    rm -rf "$NETLOGO_HOME/models"


RUN echo 'println("Hello")' | scala-cli -

RUN mkdir wolf-sheep
COPY WolfSheep.java /wolf-sheep

RUN cd /wolf-sheep && \
    scala-cli --power package -f --jar "$NETLOGO_HOME/lib/app" -o wolf-sheep.jar --java WolfSheep.java --library && ls wolf-sheep.jar


FROM debian:testing-slim

COPY OpenJDK21U-jre_x64_linux_hotspot_21.0.3_9.tar.gz jre.tar.gz

RUN tar -xvzf jre.tar.gz && \
    rm jre.tar.gz && \
    mv jdk* jre

ENV PATH=/jre/bin:$PATH

ARG NETLOGO_HOME=/netlogo

COPY --from=build $NETLOGO_HOME $NETLOGO_HOME
COPY --from=build /wolf-sheep /wolf-sheep
COPY ./WolfSheep.nlogo /wolf-sheep
COPY ./wolf-sheep /bin/wolf-sheep
WORKDIR /wolf-sheep
