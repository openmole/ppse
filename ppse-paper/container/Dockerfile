FROM debian:testing-slim AS build

LABEL org.opencontainers.image.authors="Romain Reuillon <romain.reuillon@openmole.org"
    
RUN apt-get update \ 
	&& apt-get install -y --no-install-recommends ca-certificates curl \
	&& rm -rf /var/lib/apt/lists/*

ARG NETLOGO_HOME=/netlogo

ENV LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 

COPY NetLogo-6.4.0-64.tgz /

RUN tar xzf NetLogo-6.4.0-64.tgz && \
    mv NetLogo-6.4.0-64 netlogo && \
    rm -rf "$NETLOGO_HOME/lib/runtime" && \
    rm -rf "$NETLOGO_HOME/models"

RUN mkdir traffic
COPY Traffic.java /traffic

RUN curl -sSLf https://virtuslab.github.io/scala-cli-packages/scala-setup.sh | sh
RUN mv /root/.cache/scalacli/local-repo/bin/scala-cli/scala-cli /bin/

RUN cd /traffic && \
    scala-cli --power package -f --jar "$NETLOGO_HOME/lib/app" -o traffic.jar --java Traffic.java --library

FROM debian:testing-slim

COPY OpenJDK21U-jre_x64_linux_hotspot_21.0.3_9.tar.gz jre.tar.gz

RUN tar -xvzf jre.tar.gz && \
    rm jre.tar.gz && \
    mv jdk* jre

ENV PATH=/jre/bin:$PATH

ARG NETLOGO_HOME=/netlogo

COPY --from=build $NETLOGO_HOME $NETLOGO_HOME
COPY --from=build /traffic /traffic
COPY ./Traffic_2_Lanes.nlogo /traffic
COPY ./traffic /usr/bin/traffic
RUN chmod +x /usr/bin/traffic

WORKDIR /traffic


