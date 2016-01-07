#!/usr/bin/env bash

# Get where is this script
SDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Configration
INSDIR="${SDIR}/../.build"
SRCDIR="${SDIR}/../src/emacs"

################################################################################
# Any subsequent(*) commands which fail will cause the shell script to exit immediately
set -e

################################################################################

do_autogen () {
    ./autogen.sh
}

do_configure_cli () {
    ./configure --prefix=${INSDIR} --with-modules \
		--without-x \
		--without-ns \

}

do_configure_osx () {
    ./configure --prefix=${INSDIR} --with-modules \
		--with-ns --disable-ns-self-contained \
		--with-gnutls \
		--with-rsvg \
		--with-imagemagick \
		--without-dbus --without-x
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