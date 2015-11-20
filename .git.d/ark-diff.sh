function ArkDiff()
{
    rm -rf __DIFF__
    
	local L=$1~
	local R=$1
	local D=__DIFF__

	if [ $# -ge 2 ]; then
		D=$2
	fi

    local diff="git diff --name-only $L $R"
    echo "$files"

    local file=`eval $diff`
    echo "$file"

    mkdir "$D"
    git archive --format=zip --prefix=00_BASE/ $L $file -o "./$D/00_BASE.zip"
    git archive --format=zip --prefix=01_WORK/ $R $file -o "./$D/01_WORK.zip"

    cd "./$D/"
	pwd
    unzip ./00_BASE.zip
    unzip ./01_WORK.zip

    rm ./00_BASE.zip
    rm ./01_WORK.zip

    cd ..
}

ArkDiff $@
