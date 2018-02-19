#!/bin/bash

#infile='cleaned_data/dat1.clean.csv.k=4'
infile='cleaned_data/dat1.clean.csv.k=4'
NUMBINS=5
declare -a INCOLS=(1 2 3 4 5 6)
OUTDIR='correlation_between_2_columns_histogram_based'

#####
BINDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
NCOLS=${#INCOLS[@]}

rm -rf "${OUTDIR}"; mkdir -p "${OUTDIR}"
if [ ! -d "${OUTDIR}" ]; then echo "$0 : can not create outdir '$OUTDIR'."; exit 1; fi

num_done=0
for((c1=1;c1<=${NCOLS};c1++)); do
	c21=$((c1+1))
	for((c2=${c21};c2<=${NCOLS};c2++)); do
		CMD="\
${BINDIR}/correlation_between_2_columns_histogram_based_in_R.bash \
-i '${infile}' \
-e ',' \
-h \
-o '${OUTDIR}/${c1}_${c2}_b=$NUMBINS' \
-c ${c1} -b ${NUMBINS} \
-c ${c2} -b ${NUMBINS}"
		echo "$0 : executing command ${CMD}"
		eval ${CMD}
		if [ $? -ne 0 ]; then echo "$0 : command has failed."; exit 1; fi
		num_done=$((num_done+1))
	done
done

echo "$0 : success, done ${num_done} cases."
