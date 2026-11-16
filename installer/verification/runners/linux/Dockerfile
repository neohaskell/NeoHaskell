FROM ubuntu:24.04

ENV container=docker
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install --yes --no-install-recommends dbus systemd systemd-sysv passwd \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --create-home --shell /bin/bash jess

STOPSIGNAL SIGRTMIN+3
CMD ["/sbin/init"]
