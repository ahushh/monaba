FROM nginx

RUN ln -sf /dev/stdout /var/log/nginx/access.log
RUN ln -sf /dev/stderr /var/log/nginx/error.log

COPY ./monaba.conf /etc/nginx/conf.d/default.conf

CMD ["nginx", "-g", "daemon off;"]
