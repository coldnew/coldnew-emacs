#!/bin/sh

sudo apt-get update
sudo apt-get upgrade
sudo apt-get install \
    gcc-multilib g++-multilib build-essential autoconf \
    automake libtool texinfo xorg-dev libgtk2.0-dev \
    libjpeg-dev libncurses-dev libdbus-1-dev libgif-dev \
    libtiff-dev libm17n-dev libpng12-dev librsvg2-dev \
    libotf-dev libxml2-dev libcairo-dev