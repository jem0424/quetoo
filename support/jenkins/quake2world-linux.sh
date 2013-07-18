#!/bin/bash -e

#
# Build entry point for GNU / Linux via chroot.
#
function build() {
	/usr/bin/mock -r ${CHROOT} --cwd /tmp/quake2world --chroot "
		set -e
		autoreconf -i
		./configure ${CONFIGURE_FLAGS}
		make -C linux ${MAKE_FLAGS} ${MAKE_TARGETS}
	"
}

source ./common.sh