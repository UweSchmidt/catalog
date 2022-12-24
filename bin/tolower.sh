#!/bin/bash

# set -x

for i in $(find . -maxdepth 1 -name "*.NEF" -or -name "*.XMP" -or -name "*.xmp" -or -name "*.dop" -or -name "*.JPG" -or -name "*.jpg" -or -name "*.JPEG" -or -name "*.jpeg" -or -name "*.MP4" -or -name "*.MOV" -or -name "*.HEIC")
do
    if [[ -f "$i" ]]
    then
        chmod 664 "$i"
        if basename $i | sed -E 's/^((.*)[.]...)$/\2/' | grep -q -E '^[A-Z0-9_.]+$'
           then
               newi=$(echo $i | tr A-Z a-z)
               if [[ "$i" != "$newi" ]]
               then
                   mv "$i" "$i.tmp"
                   mv "$i.tmp" "$newi"
               fi
        fi
    fi
done

ls -l
