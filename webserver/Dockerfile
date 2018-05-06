FROM ubuntu:14.04

RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "nginx"]

RUN ln -sf /dev/stdout /var/log/nginx/access.log
RUN ln -sf /dev/stderr /var/log/nginx/error.log
VOLUME ["/var/cache/nginx"]
EXPOSE 80 443

ADD ./monaba.conf /etc/nginx/sites-available/monaba.conf
RUN ["ln", "-s", "/etc/nginx/sites-available/monaba.conf", "/etc/nginx/sites-enabled/monaba.conf"]
RUN ["rm", "-fr", "/etc/nginx/sites-available/default"]

RUN ["groupadd", "webserver"]
RUN ["useradd", "webserver", "-s", "/bin/bash", "-m", "-g", "webserver", "-G", "webserver"]
ENV HOME /home/webserver
WORKDIR /home/webserver
RUN ["chown", "-R", "webserver:webserver", "/home/webserver"]

CMD ["nginx", "-g", "daemon off;"]
