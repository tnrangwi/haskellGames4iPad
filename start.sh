#!/bin/sh

if [ "$1" = '-d' ] ; then
    debug=1
    shift
else
    debug=""
fi

reset() { stty $clropt ; }

case `uname` in
    Darwin) if [ -n "$debug" ] ; then echo "Tested OS" >&2 ; fi ;;
    Linux) echo "Untested OS - let's see if stty can do it anyway..." >&2 ;;
    SunOS) if [ -n "$debug" ] ; then echo "Tested OS" >&2 ; fi ;;
    *) echo "You are on your own. Trying stty anyway" >&2 ;;
esac

setopt=`stty -a | grep -w icanon | while read line ; do
    for o in $line ; do
        if [ "$o" = "icanon" ] ; then
            echo "-icanon"
        fi
    done
done`

setopt="$setopt "`stty -a | grep -w echo | while read line ; do
    for o in $line ; do
	if [ "$o" = "echo" ] ; then
	    echo "-echo"
	fi
    done
done`
clropt=`echo $setopt | sed 's/-//g'`

if [ -n "$debug" ] ; then
    echo "setopt:$setopt / clropt:$clropt" >&2
fi

if [ -z "$1" ] ; then
    echo "Usage: $0 [-d] <haskellGame>" >&2
    echo "You can omit the .hs extension" >&2
    echo "-d Debug output" >&2
    exit 1
elif [ -f "$1" ] ; then
    game="$1"
elif [ -f "$1"'.hs' ] ; then
    game="$1"'.hs'
else
    echo "Not found:$1" >&2
    exit 1
fi

stty $setopt
trap reset 0
runghc "$game"
