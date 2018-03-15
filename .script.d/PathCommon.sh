#!/bin/sh

#
# $ pwd
# $ /home/hoge/hoge.git/.git
#
# $ RelativeLevel1
# $ ../../../../
#
RelativeLevel1()
{
    TMP=`pwd | sed -e 's@[^/]@@g'`
    RET=`echo ${TMP} | sed -e 's@/@../@g'`
    echo ${RET}
}

#
# $ pwd
# $ /home/hoge/hoge.git/.git
#
# $ RelativeLevel2
# $ ../../../..
#
RelativeLevel2()
{
    TMP=`RelativeLevel1`
    RET=`echo ${TMP} | sed -e 's@/$@@g'`
    echo ${RET}
}
