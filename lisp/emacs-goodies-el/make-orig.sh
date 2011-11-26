#! /bin/sh

set -e

dh_testdir
dh_clean
rm -f `find . -name "*~"`
rm -f `find . -name "#*"`
rm -f `find . -name ".#*"`
rm -fR elisp/emacs-goodies-el/info
rm -fR elisp/debian-el/info

SOURCE=$(dpkg-parsechangelog | awk '/^Source:/ { print $2 }')
VERSION=$(dpkg-parsechangelog | awk '/^Version:/ { print $2 }')
#UPSTREAM_VERSION=${VERSION%-*}
#DEBIAN_VERSION=${VERSION##*-}
THIS=$(basename $0)
THISDIR=$(basename $PWD)

(cd .. ; install -d build_${VERSION})
rm -fR ../build_${VERSION}/*
(cd ../build_${VERSION} ; install -d ${SOURCE}-${VERSION})
tar cf - --exclude=CVS elisp debian 00AddingFiles COPYING-GPL-v2 COPYING-GPL-v3 | ( cd ../build_${VERSION}/${SOURCE}-${VERSION} ; tar xf -)
#(cd ../build_${VERSION} ; tar cf ${SOURCE}_${VERSION}.orig.tar ${SOURCE}-${VERSION})
#(cd ../build_${VERSION} ; gzip --best ${SOURCE}_${VERSION}.orig.tar)
