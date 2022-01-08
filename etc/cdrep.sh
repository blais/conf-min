# Changing directory while making a replacement in current path.
function cdrep() {
    local newdir=${PWD/$1/$2}
    if [ "x$PWD" = "x$newdir" ]; then
        echo "Error: no change." 1>&2
    elif [ ! -d "$newdir" ]; then
        echo "Error: no such directory $newdir" 1>&2
    else
        echo $newdir
        cd $newdir
    fi
}
