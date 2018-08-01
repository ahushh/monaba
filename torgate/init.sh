#!/usr/bin/env bash

mkdir -p /var/lib/tor/hidden_service

if [ -z "${TORGATE_HOSTNAME}" ]; then
    echo "TORGATE_HOSTNAME environment var is undefined, torgate will be disabled";
    exit 0;
else
    echo "${TORGATE_HOSTNAME}" > /var/lib/tor/hidden_service/hostname
fi

if [ -z "${TORGATE_PRIVATE_KEY}" ]; then
    echo "TORGATE_PRIVATE_KEY environment var is undefined, torgate will be disabled";
    exit 0;
else
    echo "-----BEGIN RSA PRIVATE KEY-----" > /var/lib/tor/hidden_service/private_key
    echo "${TORGATE_PRIVATE_KEY}" | fold -w 64 >> /var/lib/tor/hidden_service/private_key
    echo "-----END RSA PRIVATE KEY-----" >> /var/lib/tor/hidden_service/private_key
fi

if [ -z "${TORGATE_ENDPOINT}" ]; then
    echo "TORGATE_ENDPOINT environment var is undefined, torgate will be disabled";
    exit 0;
else
    sed -i 's/\__TORGATE_ENDPOINT__/'"${TORGATE_ENDPOINT}"'/' /etc/tor/torrc
fi

chmod -R 600 /var/lib/tor/hidden_service
echo "Torgate started: ${TORGATE_HOSTNAME} -> ${TORGATE_ENDPOINT}"

tor
