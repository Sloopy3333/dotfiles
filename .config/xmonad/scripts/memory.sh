#!/bin/sh
mem=$(free -m | sed -n '2{p;q}' | awk '{print $3}')
echo "MEM $mem MB"
