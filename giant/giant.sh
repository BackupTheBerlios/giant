#!/bin/sh

cd `dirname $0`
system=`uname -s`
if [ "$system" = "Linux" ]; then
	./giant-Linux $*
elif [ "$system" = "SunOS" ]; then
	./giant-SunOS $*
else
	echo "$system is unsupported."
	exit 1
fi
