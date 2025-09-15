#!/bin/bash

# set -x

# rsync all media files from "Diakaesten" with a media backup

function die() {
    echo $1 1>&2
    exit 1
}

function trc() {
    echo $* 1>&2
}

istest="yes"

if [[ "$istest" != "no" ]]
then
    defaultSrcDir="$HOME/haskell/Diakaesten"
    defaultDstDir="$HOME/tmp/photos"
else
    defaultDisk="5TB-Diakasten"
    defaultSrcDir="$HOME/Bilder/Diakaesten"
    defaultDstDir="/Volumes/$defaultDisk/home/uwe/Bilder/catalog/photos"
fi

dry="--dry-run"
if [[ "$1" = "-x" ]]
then
    run=1
    shift
    dry=''
fi

srcDir="$defaultSrcDir"
dstDir="$defaultDstDir"

[[ -d "$srcDir" ]] || die "source directory not fount: $srcDir"
[[ -d "$dstDir" ]] || die "destination directory not fount: $dstDir"

excludes=""
function buildExcludes() {
    for i in $(find "$srcDir" -type f | \
                   sed -e 's|^.*[.]|.|'| \
                   sort -u | \
                   grep -i -v -E '.jpg|.jpeg|.gif|.md|.mp4|.tiff?|.png|.pbm|.pgm|.ppm|.md|.txt' | \
                   cat )
    do
        trc "files with extension \"$i\" are excluded"
        excludes="$excludes --exclude=\'$i\'"
    done
}

buildExcludes

# output rsync command
echo "rsync $dry -av" $excludes "$srcDir/" "$dstDir"
