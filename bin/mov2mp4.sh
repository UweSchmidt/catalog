#!/bin/bash

# set -x

# convert all .mov files into .mp4 files
# and extract a .jpg which can be used as thumbnail
#

dry="echo DRY-RUN> "
if [[ "$1" = "-x" ]]
then
    run=1
    shift
    tomp4=ffmpeg
else
    run=0
    tomp4="$dry ffmpeg"
fi

# default dir to be searched
# defdir=$HOME/Bilder/Diakaesten
defdir="."

dir=${1:-$defdir}


function die() {
    echo $1 1>&2
    exit 1
}

cd $dir || die "directory not found: $dir"

for i in $(find . -name "*.mov")
do
    dn=$(dirname "$i")
    bn=$(basename -s .mov "$i")
    fn="$dn/${bn}.mp4"
    if [[ ! -f "$fn" ]]
    then
        [[ $run -eq 1 ]] && echo $tomp4 -i "$i" "$fn"
        $tomp4 -loglevel warning -i "$i" "$fn"
        ls -l "$dn/${bn}".*
    fi
done

for i in $(find . -name "*.mp4")
do
    dn=$(dirname "$i")
    bn=$(basename -s .mp4 "$i")
    ic="$dn/${bn}.jpg"
    if [[ ! -f "$ic" ]]
    then
        set -x # [[ $run -eq 1 ]] && echo $tomp4 -i "$fn" "$ic"
        $tomp4 -loglevel warning -ss 1 -i "$i" -qscale:v 4 -frames:v 1 "$ic"
        set +x
        ls -l "$dn/${bn}".*
    fi
done

if [[ $run -ne 1 ]]
then
    echo "DRY-RUN> run $0 -x $1"
fi
