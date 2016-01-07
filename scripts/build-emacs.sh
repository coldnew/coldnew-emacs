#!/usr/bin/env bash

# Get where is this script
SDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Configration
INSDIR="${SDIR}/../"
SRCDIR="${SDIR}/src/emacs"

do_autogen () {
    ./autogen.h
}

do_configure_osx () {
    ./configure --prefix=${INSDIR} --without-x --without-ns --with-modules
}

do_configure_linux () {
    :
}

do_make () {
    make -j9
}

do_install () {
    make install
}

################################################################################

# building emacs
cd $SRCDIR
do_autogen

# configure according to platform
case $(uname) in
    "Darwin")
	do_configure_osx
	;;
    "Linux")
	do_configure_linux
	;;
    *)
	echo "This building script only support Linux and Darwin"
	exit -1
	;;
esac

# You need to set --with-modules to enable dynamic modules feature
do_make
do_install