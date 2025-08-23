# Efficiently find the current branch, if any.
function find_branch()
{
    DIR=$PWD
    while [ "$DIR" != "/" -a "$DIR" != "" ]; do
        if [ -d "$DIR/.hg" ]; then
            VC="hg"
            if [ -f "$DIR/.hg/branch" ]; then
                BRANCH=$(cat "$DIR/.hg/branch")
            fi
            break
        elif [ -d "$DIR/.git" ]; then
            VC="git"
            BRANCH=$(cd $DIR ; git rev-parse --abbrev-ref HEAD 2>/dev/null)
        fi
        DIR=$(dirname "$DIR")
    done

    if [ -n "$VC" ]; then
        echo " [$VC|$BRANCH]"
    fi
}

# Prompt, remove a number of components from the physical path.
PS1='\h$(find_branch) $(echo "\w" | sed -e "s+$HOME+~+;s+\(/[@a-zA-Z0-9_]*/\).*\(/[@a-zA-Z0-9_]*/[@a-zA-Z0-9_]*\)+\1...\2+g")\$ '
export PS1

# TODO: Try out "Oh My Posh" colors.
