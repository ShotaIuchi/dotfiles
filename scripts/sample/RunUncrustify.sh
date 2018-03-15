#!/bash/sh
##
## DEPENDENT)
##  * ~/.script.d/FindTargetFile.sh
##     <https://github.com/ShotaIuchi/dotfiles>
##  * uncrustify
##     <https://github.com/uncrustify/uncrustify>
##

# ==================================================
#
#   debug (default-off)
#
# ==================================================
DEBUG=0


# ==================================================
#
#   command
#
# ==================================================
CC_UNCRUSTIFY=uncrustify


# ==================================================
#
#   target
#
# ==================================================
TARGET_EXTENSION=(
c cc cpp
h hh hpp
)
TARGET_FILES=(
)

# ==================================================
#
#   exclusion
#
# ==================================================
## dir
EXCLUSION_TARGET_DIRS=(
.git .svn
)
## file
EXCLUSION_TARGET_FILES=(
)


# ==================================================
#
#   get target file
#
# ==================================================
FindTargetFile
if [ $DEBUG -eq 1 ]; then
    echo $FINAL_FILES
fi


# ==================================================
#
#   format
#
# ==================================================
for vFile in ${FINAL_FILES[@]}
do
    echo " === TARGET : $vFile === "
    errCount=0
    while true
    do
        $CC_UNCRUSTIFY --check $vFile > /dev/null
        if [ $? -eq 0 ]; then
            echo "[ pass] $vFile"
            break
        else
            if [ $errCount -ge 3 ]; then
                echo "[error] $vFile"
                break;
            fi
            (( errCount++ ))
        fi
        $CC_UNCRUSTIFY --no-backup $vFile
    done
    echo ""
done

set +f
