function ExportDiff()
{
    if [ $# -lt 1 ]; then
        local o=HEAD~
        local n=HEAD
        local d=ARK_DIFF
    elif [ $# -lt 2 ]; then
        local o=${1}~
        local n=${1}
        local d=ARK_DIFF
    elif [ $# -lt 3 ]; then
        local o=${1}
        local n=${2}
        local d=ARK_DIFF
    else
        local o=${1}
        local n=${2}
        local d=${3}
    fi

    echo "--------------------------------------------"
    echo " OLD : ${o}"
    echo " NEW : ${n}"
    echo " OUT : ${d}"
    echo "--------------------------------------------"

    if [ -e ${d} ]; then
        #rm -rI ${d}
        rm -rf ${d}
    fi
    mkdir ${d}

    local o_file=`eval git diff --name-only --diff-filter=ACRM ${n} ${o}`
    local n_file=`eval git diff --name-only --diff-filter=ACRM ${o} ${n}`
    if [ -z "${o_file[0]}" ]; then
        cd "./${d}/"
        mkdir 00_old
        cd ..
    else
        git archive --format=zip --prefix=00_old/ ${o} ${o_file} -o "./${d}/00_old.zip"
        cd "./${d}/"
        unzip ./00_old.zip
        rm ./00_old.zip
        cd ..
    fi

    if [ -z "${n_file[0]}" ]; then
        cd "./${d}/"
        mkdir 99_new
        cd ..
    else
        git archive --format=zip --prefix=99_new/ ${n} ${n_file} -o "./${d}/99_new.zip"
        cd "./${d}/"
        unzip ./99_new.zip
        rm ./99_new.zip
        cd ..
    fi
}

ExportDiff $@
