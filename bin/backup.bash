#!/bin/bash

dat=`date +"%d.%m.%Y_%H.%M"`
if [ "$1" != "" ]; then
	dat+="_""$1"
fi
filn=${dat}".tgz"

outf=".BACKUP/${filn}"
CMD="tar cvz --exclude='.BACKUP' --exclude='ori' --exclude='*.csv' --exclude='*.lock*' --exclude='data' --exclude='.git' -f ${outf} ."
echo $CMD
eval $CMD > /dev/null
if [ $? -ne 0 ]; then echo "$0 : command has failed '${CMD}'."; exit 1; fi

echo "$0 : done backup in '${outf}'"
