#!/bin/bash

# set -x

# convert all .mov files into .mp4 files
# and extract a .jpg which can be used as thumbnail
#

# default dir to be searched
defdir=$HOME/Bilder/Diakaesten


dir=${1:-$defdir}

# the ffmpeg converter

tomp4=ffmpeg

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
        echo $tomp4 -i "$i" "$fn"
        $tomp4 -i "$i" "$fn"
        # ls -l "$dn/${bn}".*
    fi
    ic="$dn/${bn}.jpg"
    if [[ ! -f "$ic" ]]
    then
        echo $tomp4 -i "$fn" "$ic"
        $tomp4 -ss 2 -i "$fn" -qscale:v 4 -frames:v 1 "$ic"
        ls -l "$dn/${bn}".*
    fi

done
