#!/bin/bash

set -u

server="scheibe"
port="3001"
host="http://$server:$port"

format="img/1x1"
collpx="/archive/collections"
ext="jpg"
coll="$collpx/${1:-clipboard}"  # default collection is clipboard


# get collection in json format

# coll="/archive/collections/albums/2019/2019-10-05.Regenbogen"  # the collection path

op="get/collection"                    # path prefix: read a collection
data="[]"                               # extra json data
url="$host/$op$coll"
dir=$(basename "$coll")

export LANG=C

response=$(curl -v --data "$data" "$url")

function imglist() {
    cat \
        | sed 's|^.*entries.:\[||' \
        | sed 's|],.ImgNode.*$||' \
        | sed $'s|},|\\\n|g' | sed 's|{||g' | sed 's|}||g' \
        | grep -n '.*' \
        | grep 'ColEntry.:.IMG' \
        | perl -p -e 's|^([0-9]*):.*part.:.(.*).,.ref.:.(.*)/.*$|\1:\3|'
}

function getimage() {
    cnt=$(echo $1 | perl -p -e 's|:.*$||')
    cnt=$(expr $cnt - 1)
    path=$(echo $1 | perl -p -e 's|^.*:||')
    pic=$(printf "pic-%04d.$ext" $cnt)
    url="$host/docs/$format$coll/$pic"
    file="$dir/$pic"
    [[ -d "$dir" ]] || mkdir "$dir"
    echo curl -o "$file" "$url"
    curl -o "$file" "$url"
}

# echo $response | imglist

for i in $(echo $response | imglist)
do
    getimage $i
done
