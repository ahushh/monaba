FROM ubuntu:16.04
EXPOSE 9312

RUN apt-get update && apt-get -y install sphinxsearch cron curl supervisor

RUN mkdir -p /var/log/supervisord

WORKDIR /root

COPY ./sphinx.conf ./sphinx.conf

RUN echo '*/15 * * * * indexer --rotate monaba' | crontab -

COPY ./supervisord.conf ./supervisord.conf

COPY ./run.sh ./run.sh
RUN chmod +x ./run.sh
CMD /root/run.sh