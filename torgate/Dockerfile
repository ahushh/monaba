FROM ubuntu:18.04

RUN apt-get update && apt-get install -y tor

ADD ./torrc.conf /etc/tor/torrc
ADD ./init.sh /init.sh

CMD bash /init.sh
