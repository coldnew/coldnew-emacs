#!/usr/bin/env bash

# Get where is this script
SDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Configration
INSDIR="${SDIR}/../.build"
SRCDIR="${SDIR}/../modules/emacs"

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
    ./configure --prefix=${INSDIR} --with-modules \
		--without-ns --disable-ns-self-contained \
		--with-gnutls \
		--with-rsvg \
		--with-imagemagick \
		--with-dbus --with-x --with-x-toolkit=gtk3 --with-xwidgets
}

do_make () {
    make bootstrap
    make -j9
}

do_install_osx() {
    # Replace the symlink with one that avoids starting Cocoa.
    rm $INSDIR/bin/emacs -f
    cp -rf $SRCDIR/nextstep/Emacs.app $INSDIR
    cat <<-EOF > $INSDIR/bin/emacs
#!/usr/bin/env bash
exec $INSDIR/Emacs.app/Contents/MacOS/Emacs "\$@"
EOF
    chmod +x $INSDIR/bin/emacs
}

do_install () {
    make install
    # special hack for different os
    case $(uname) in
	"Darwin") do_install_osx ;;
	*) ;;
    esac

}

do_clean () {
    make distclean
    make mantainer-clean
}

################################################################################

# building emacs
cd $SRCDIR

# check commit id, if already build, it's no need to rebuild
if [ -f "$INSDIR/commit-id" ]; then
    OLD_ID=$(cat "$INSDIR/commit-id")
    NEW_ID=$(git rev-parse --short HEAD)
    if [ $OLD_ID == $NEW_ID ]; then
	echo "Your emacs is match to commit-id, no need to rebuild."
	exit 0 # quit
    else
	echo -ne "$NEW_ID" > "$INSDIR/commit-id"
	echo "Start rebuilding emacs..."
    fi
fi

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
