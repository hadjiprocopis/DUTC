#!/bin/bash

rm -f RMARKDOWN_OUT_REPORTS/*

for aRfile in RMARKDOWN_IN/*.R; do
#	CMD="bin/rmarkdown.bash '${aRfile}' pdf_document"
#	echo "$0 : executing ${CMD}"
#	eval ${CMD}
#	if [ $? -ne 0 ]; then echo "$0 : call to ${CMD} has failed."; exit 1; fi	

	CMD="bin/rmarkdown.bash '${aRfile}' html_document"
	echo "$0 : executing ${CMD} ..."
	eval ${CMD}
	if [ $? -ne 0 ]; then echo "$0 : call to ${CMD} has failed."; exit 1; fi
done

str=""
for aRfile in RMARKDOWN_OUT_REPORTS/*.pdf; do
	str="${str} ${aRfile}"
done
CMD="convert ${str} RMARKDOWN_OUT_REPORTS/final_report.pdf"
echo "$0 : executing ${CMD}..."
eval ${CMD}
if [ $? -ne 0 ]; then echo "$0 : call to ${CMD} has failed."; exit 1; fi
