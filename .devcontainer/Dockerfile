# syntax=docker/dockerfile:1
FROM centos:7.9.2009

# for apt-get
# https://serverfault.com/questions/949991/how-to-install-tzdata-on-a-ubuntu-docker-image
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Bucharest
ENV DISPLAY=host.docker.internal:0.0
ENV SCALA_VERSION 2.13.8

RUN yum update -y && \
    yum install -y git curl wget unzip java-11-openjdk.x86_64 && \
    yum clean all

# Install custom sbt if needed
# See metals default sbt limitations: https://scalameta.org/metals/docs/editors/vscode.html#custom-sbt-launcher
RUN curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes
RUN ./cs install scala:$SCALA_VERSION && ./cs install scalac:$SCALA_VERSION
RUN echo "export PATH=\"$PATH:/root/.local/share/coursier/bin\"" >> /root/.bashrc