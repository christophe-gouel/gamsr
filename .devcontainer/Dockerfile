FROM rocker/r-ver:latest

ARG GAMS_MAJOR=42
ARG GAMS_MINOR=3
ARG GAMS_MAINT=0

RUN apt-get update && apt-get install wget git -y
RUN mkdir /opt/gams &&\
    cd /opt/gams && \
    wget https://d37drm4t2jghv5.cloudfront.net/distributions/${GAMS_MAJOR}.${GAMS_MINOR}.${GAMS_MAINT}/linux/linux_x64_64_sfx.exe && \
    chmod a+x linux_x64_64_sfx.exe && \
    ./linux_x64_64_sfx.exe && \
    rm linux_x64_64_sfx.exe

ENV PATH="/opt/gams/gams${GAMS_MAJOR}.${GAMS_MINOR}_linux_x64_64_sfx:${PATH}"
