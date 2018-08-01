#!/bin/bash

which docker > /dev/null
if [ $? -eq 1 ]; then
  echo 'You must install Docker first';
  exit 1;
fi  

which docker-compose > /dev/null
if [ $? -eq 1 ]; then
  echo 'You must install docker-compose first';
  exit 1;
fi  

echo 'Installing nano-md5...'
wget -q https://hackage.haskell.org/package/nano-md5-0.1.2/nano-md5-0.1.2.tar.gz
tar -zxf nano-md5-0.1.2.tar.gz
rm nano-md5-0.1.2.tar.gz
patch nano-md5-0.1.2/Data/Digest/OpenSSL/MD5.hs < monaba/extra/MD5.hs.patch
cp -r nano-md5-0.1.2 monaba/nano-md5-0.1.2
rm -fr nano-md5-0.1.2

echo 'Installing GeSHI...'
tar -zxf monaba/extra/GeSHi-1.0.8.11.tar.gz
cp -r geshi monaba/geshi
rm -fr geshi

echo 'Creating settings file...'
cp monaba/config/settings.yml .

echo 'Creating storage dirs...'
mkdir -p storage/upload storage/pgdata storage/searchdata

echo 'Done'
