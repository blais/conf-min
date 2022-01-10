#!/bin/bash
# Functions to print and edit paths.

function ppath () {
    if [ -z "$*" ]; then
        var=$PATH
    else
        var=$(printenv $1)
    fi
    echo $var | tr : \\n
}

function epath () {
    local tmp=/tmp/epath.tmp
    eval "echo \$$1" | tr : \\n > $tmp
    vim $tmp
    newpath=$(cat $tmp | tr \\n : | sed -e 's/::/:/g;s/:$//')
    echo $newpath
    eval "$1=$newpath"
    rm -f $tmp
}

complete -A variable ppath
complete -A variable epath
