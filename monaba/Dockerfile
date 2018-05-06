FROM fpco/stack-build
 
RUN apt-get update
RUN apt-get -y install libgeoip-dev libcrypto++-dev libssl-dev php ffmpeg exiftool

RUN mkdir -p /opt/monaba

ADD http://sourceforge.net/projects/geshi/files/geshi/GeSHi%201.0.8.11/GeSHi-1.0.8.11.tar.gz /opt/monaba/geshi
ADD http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz /usr/share/GeoIP/GeoIPCity.dat

COPY Monaba.cabal /opt/monaba/Monaba.cabal
COPY stack.yaml /opt/monaba/stack.yaml
WORKDIR /opt/monaba
RUN stack setup
COPY . /opt/monaba
RUN stack build

RUN stack install --local-bin-path=/opt/monaba
