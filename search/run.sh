#!/bin/bash

indexer -c /root/sphinx.conf --all
supervisord -c /root/supervisord.conf
