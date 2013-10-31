#!/bin/sh

echo "usage: mk-release [VERSION]"

VERSION="$1"
echo "archiving for version $VERSION"

EXCLUDES="--exclude=.git --exclude=_build --exclude=man --exclude=doc --exclude=.*.sw*"
OUTPUT="qcheck-$VERSION".tar.gz
PREFIX="--transform s+^\./+qcheck-$VERSION/+"

tar cavf $OUTPUT $PREFIX $EXCLUDES .
