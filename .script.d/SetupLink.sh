#!/bin/sh
############################################################
##
## INPUT)
##  1 : target directory
##  2 : src directory
##  3 : file/directory name
## OUTPUT)
##  none
##
############################################################
SetupLink()
{
    SRC=${2}/${3}
    DST=${1}/${3}

    # exists (file or directory)
    if [ -f ${DST} ] || [ -d ${DST} ]; then
        echo "[x]${SRC}"
        echo "   Target already exists."
        echo "   Please change to the name with '.local' appended to the end of the file"
    # empty
    else
        echo "[o]${SRC}"
        echo "   ${DST} --> ${SRC}"
        if [ -f ${SRC} ]; then
            ln --logical ${SRC} ${DST}
        else
            ln --symbolic ${SRC} ${DST}
        fi
    fi
}

