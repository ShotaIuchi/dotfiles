#!/bash/sh
############################################################
##
## INPUT) GLOBAL)
##  TARGET_EXTENSION
##  TARGET_FILES
##  EXCLUSION_TARGET_DIRS
##  EXCLUSION_TARGET_FILES
##
## OUTPUT) GLOBAL)
##  FINAL_FILES
##
############################################################
function FindTargetFile()
{
    # debug (default-off)
    DEBUG=0

    ## ------------------------------------------------------
    ## debug log
    if [ $DEBUG -eq 1 ]; then
        echo "# =================================================="
        echo "#"
        echo "#   args"
        echo "#"
        echo "# =================================================="
        echo "TARGET_EXTENSION          : $TARGET_EXTENSION"
        echo "TARGET_FILES              : $TARGET_FILES"
        echo "EXCLUSION_TARGET_DIRS     : $EXCLUSION_TARGET_DIRS"
        echo "EXCLUSION_TARGET_FILES    : $EXCLUSION_TARGET_FILES"
    fi

    ## shell option (ON)
    set -f

    ## commands
    CC_FIND=find

    # note)
    #   find ./ \( EXCLUSION_TARGET_DIRS \) -or \( \( TARGET_EXTENSION \) -and  \( TARGET_EXTENSION, EXCLUSION_TARGET_FILES \) \)

    ## ------------------------------------------------------
    ## exclusion - dir
    FIND_CMD="\( "
    for vDir in ${EXCLUSION_TARGET_DIRS[@]}
    do
        FIND_CMD+="-type d -name $vDir -prune -not -name $vDir "
    done

    ## ------------------------------------------------------
    ## exclusion - file
    if [ ${#ARGET_EXTENSION[@]} -le 0 ]; then
        FIND_CMD+="\) -or \( "
    else
        FIND_CMD+="\) -or \( \( "
        for vFile in ${EXCLUSION_TARGET_FILES[@]}
        do
            echo $vFile
            FIND_CMD+="-type f -not -name $vFile "
        done
        FIND_CMD+="\) -and \( "
    fi

    ## ------------------------------------------------------
    ## exclusion - file
    isFirst=1
    for vFormat in ${TARGET_EXTENSION[@]}
    do
        if [ $isFirst -eq 1 ]; then
            isFirst=0
        else
            FIND_CMD+="-o "
        fi
        FIND_CMD+="-type f -iname \"*.$vFormat\" "
    done

    ## ------------------------------------------------------
    ## exclusion - file
    isFirst=1
    for vFile in ${TARGET_FILES[@]}
    do
        if [ $isFirst -eq 1 ]; then
            isFirst=0
        else
            FIND_CMD+="-o "
        fi
        FIND_CMD+="-o -type f -name $vFile "
    done

    ## ------------------------------------------------------
    ## exclusion - file ( \) )
    if [ ${#ARGET_EXTENSION[@]} -le 0 ]; then
        FIND_CMD+="\)"
    else
        FIND_CMD+="\) \)"
    fi

    ## ------------------------------------------------------
    ## debug log
    if [ $DEBUG -eq 1 ]; then
        echo ""
        echo "# =================================================="
        echo "#"
        echo "#   command"
        echo "#"
        echo "# =================================================="
        echo "$CC_FIND ./ $FIND_CMD"
    fi

    ## ------------------------------------------------------
    ## find-file
    FINAL_FILES=`eval "$CC_FIND ./ $FIND_CMD"`

    ## shell option (OFF) (default:comment-out)
    # set +f
}
