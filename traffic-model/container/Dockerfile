FROM debian:testing-slim as build

MAINTAINER "Romain Reuillon <romain.reuillon@openmole.org"
    
RUN apt-get update \ 
	&& apt-get install -y --no-install-recommends locales ca-certificates fakeroot sudo curl \
	&& rm -rf /var/lib/apt/lists/*

RUN curl -sSLf https://virtuslab.github.io/scala-cli-packages/scala-setup.sh | sh
RUN mv /root/.cache/scalacli/local-repo/bin/scala-cli/scala-cli /bin/

ARG NETLOGO_HOME=/netlogo
ARG NETLOGO_VERSION=6.4.0

ENV LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    NETLOGO_TARBALL=NetLogo-$NETLOGO_VERSION-64.tgz

ENV NETLOGO_URL=https://ccl.northwestern.edu/netlogo/$NETLOGO_VERSION/$NETLOGO_TARBALL

RUN curl -o $NETLOGO_TARBALL $NETLOGO_URL && \ 
    tar xzf $NETLOGO_TARBALL

RUN mv "NetLogo-$NETLOGO_VERSION-64" netlogo && \
    rm -rf "$NETLOGO_HOME/lib/runtime" && \
    rm -rf "$NETLOGO_HOME/models"

RUN mkdir traffic
COPY Traffic.java /traffic

RUN cd /traffic && \
    scala-cli --power package -f --jar "$NETLOGO_HOME/lib/app" -o traffic.jar --java Traffic.java --library

FROM debian:testing-slim

RUN apt-get update \
        && apt-get install -y --no-install-recommends locales ca-certificates fakeroot sudo wget \
        && rm -rf /var/lib/apt/lists/*

RUN wget -O jre.tar.gz https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.7%2B7/OpenJDK17U-jre_x64_linux_hotspot_17.0.7_7.tar.gz && \
    tar -xvzf jre.tar.gz && \
    rm jre.tar.gz && \
    mv jdk* jre

ENV PATH=/jre/bin:$PATH

ARG NETLOGO_HOME=/netlogo

COPY --from=build $NETLOGO_HOME $NETLOGO_HOME
COPY --from=build /traffic /traffic
COPY ./Traffic_2_Lanes.nlogo /traffic
COPY ./traffic /bin/traffic
WORKDIR /traffic


#RUN touch run.sc && scala-cli run --server=false -j 17 --dep "com.thoughtworks.xstream:xstream:1.4.20" run.sc && rm run.sc

#WORKDIR $NETLOGO_HOME
    
#(cd $NETLOGO_HOME && cp netlogo-headless.sh netlogo-headless-limit.sh && sed 's/^\"\$\JAVA\"/cpulimit -l 100 -f -- \"\$JAVA\"/1' netlogo-headless-limit.sh)

#ENV PATH=${NETLOGO_HOME}:${PATH}


