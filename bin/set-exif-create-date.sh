#!/bin/bash

# set EXIF CreateDate tag in jpg' and mov/mp4 to file create date
#
# used for images with suppressed metadata from mobile phones

verbose=-v

log () {
    if [[ "$verbose" = "-v" ]]
    then
        echo $@ 1>&2
    fi
}

setcdate() {
    cda=$(exiftool -CreateDate "$1")
    if [[ "$cda" = "" ]]
    then
        cda=$(exiftool "$1" | grep '^File Modification' | sed -e 's/File.*: //')
        exiftool -CreateDate="$cda" "$1" && touch -r "$1_original" "$1"
        log exiftool -CreateDate="$cda" "$1"
    fi

}

for i in $(find . -maxdepth 1 -name "*.jpg" -or -name "*.mov" -or -name "*.mp4" -or -name "*.jpeg")
do
    setcdate "$i"
done
