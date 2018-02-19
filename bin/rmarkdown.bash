#!/bin/bash

outdir='RMARKDOWN_OUT_REPORTS'; if [ ! -e "${outdir}" ]; then mkdir "${outdir}"; if [ ! -d "${outdir}" ]; then echo "$0 : output dir '${outdir}' is not a dir or can not be created."; exit 1; fi; fi
if [ "$1" == "" ]; then echo "Usage : $0 infile.R [pdf|html]"; exit 1; fi
infile=$1
outformat='all'
if [ "$2" != "" ]; then outformat="$2"; fi

R --no-save<<EOC
	library(rmarkdown)
	rmarkdown::render(
		input="${infile}",
		 knit_root_dir='${PWD}',
		output_dir='${outdir}',
		output_format='${outformat}'
	)
EOC
st=$?
if [ ${st} -eq 0 ]; then
	echo "$0 : done, output in $outdir"
else
	echo "$0 : failed with exit code ${st}."
fi
exit $st
