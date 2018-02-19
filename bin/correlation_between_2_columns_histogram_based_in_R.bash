#!/bin/bash

# program by Andreas Hadjiprocopis
# livantes at soi.city.ac.uk
# Institute of Cancer Research
# London, 2010
# Free for everyone to copy, use, modify / quote author

appname=`basename "$0"`
bindir=$( cd "$( dirname "$0" )" && pwd )
now=`date`
rand=$$
_internal_arrays_ifs_="_::_" # stupid bash arrays!!!
param_ifs=':'
defaultInputFilesIFS='\t'
defaultInputFilesHaveHeader='FALSE'
seed=$$
png_width=1024
png_height=768
png_fontsize=18
box_labels_fontsize='4.0'
theta=10
phi=50
default_number_of_bins=50
density_num_points=1024
num_dimensions=2
num_threads=1

function display_help_exit {
	echo "Usage : $1 -i input_file -o output_file [-c col_num -c | -z col_name -z col_name] [-e input_data_separator] [-E output_data_separator] [-R R_output_script_name] [-2 seed] [-3 png_width:png_height:png_fontsize]"
	echo "other params: [-T theta] [-P phi] [-b bins_for_column1 [-b bins_for_column2...]] [-g] [-G] [-X xlab] [-Y ylab] [-t title] [-x min_x:max_x] [-y min_y:max_y] [-I] [-L] [-r] [-u] [-U]"
	echo "This script will calculate upnormal statistical correlation between binned inputs by comparing the frequency when occuring independently and when occuring together. That is it will construct a histogram for each input column independently and compare the counts/probabilities of each bin to a histogram over all input columns (i.e. an N-dimensional histogram for N columns). For example, if event 0.2<x<0.3 occurs P(0.2<x<0.3)=20% of the time for the first column (x) and P(0.2<y<0.3)=10% of the time for the second column (y), then the probability of P(0.2<x<0.3, 0.2<y<0.3) should be P(expected)=20% x 10%, IF the two columns are completely independent. The deviation from independence will be calculated as 'log( (observed/(1-observed)) / (expected/(1-expected)) )' after expected and observed probabilities are modified so that 0.0 becomes 0.001 and 1.0 becomes 0.999 in order to avoid NAs."
	echo "The output consists of an image of the histograms and the heatmap of the N-dimensional histogram, a 'counts.txt' file which is the sparse N-dimensional histogram in text form - sparse means that zero-count bins are omitted. This file contains the label for each bin, the numeric range, its count, probability, expected probability and a measure of correlation (fold_probability). Optionally (with -u flag) the unsparsed histogram can be saved too (but can be large if large number of bins given) into 'counts.unsparsed.txt'. Also an 'info.txt' file which contains mean/stdev of counts, mean/stdev of counts, mutual information (MI) between the counts vector of inputs (must be exactly 2 inputs, same number of bins), covariance (COV) same as before, t-test and levene-test pvalues of expected and observed probabilities of the common, N-dimensional histogram to differ significantly."
	echo "Caution: correlation metric does not include any confidence test, so correlation is sometimes high for low count bins because a small fluctuation is seen dramatic because of the ratio."
	echo "In the output image, each box represents a combination of the bins of the input columns, e.g. if each input column has 10 bins, there will be 100 boxes. Each box will contain 3 numbers. At the top is the metric of the FOLD change in expected and observed probabilities. At the bottom is a pair of numbers in the form 'A/B', A: the observed counts, B: the expected counts."
	echo "If the image gets too clatter increase its size using e.g. -3 2048:1536:22 -L 5.5"
	echo "The -3 sets image dimensions and font size W:H:FS, the -L sets box font size."
	echo "OPTIONS:"
	echo "-x xmin:xmax, -y ymin:ymax : specify the limits of the plot"
	echo "-I : Discretize (bin) the input data by applying the bins of the calculated histogram and dump it to a file outputFile.discrete. What this means is that each input value will be replaced by the middle of the bin it is falling to."
	echo "-L : label the input data as above but instead of bin mid, output the label/id of bin, e.g. first bin->1 etc. then dump it to a file outputFile.labels"
	echo "-b nb1 [-b nb2 ...] : specify the number of bins for each input column. If no bins number or the number of '-b nb' parameters is not equal to the number of columns specified, then automatic number of bins calculation will be employed for the missing nb's. Default number of bins is ${default_number_of_bins}. Warning: in order to compare individual histograms using mutual information and covariance, the same number of bins for each column is required."
#	echo "-B breaks1 [-B breaks2 ...] : specify the actual breaks for each column as a comma separated list separated by colon, e.g. -B '1,2,3':'4,5,6'"
	echo "-Q : (either with number of bins given or not given, in which case is automatically calculated) : calculate the bins (breaks) so that each bin contains an equal number of counts. This only applies to 2D and is useful to show correlations between data."
	echo "-N points : if automatic bin width is required (i.e. missing number of bins (-b) or none at all), then this will require density estimation of the inputs which will use the specified resolution (number of points) specified here. Default number is ${density_num_points}."
	echo "-n num_dims : the number of dimensions is the number of input columns to test together for correlation, the power of the test is reduced exponentially with the number of dimensions if the data rows number is kept constant. Default is ${num_dimensions}."
	echo "-r : flag to indicate that probabilities and not counts should be used for the analysis. Without this flag, (histogram bins) counts will be used."
	echo "-g : flag to indicate to take the logarithm of each column prior to any calculations"
	echo "-u : additionally to all other outputs, save the unsparsed histogram as well (the one saved is a sparse histogram meaning that zero-count bins are omitted)."
	echo "AESTHETICS OPTIONS:"
	echo "-T title : specify the title of the plot (at the top)"
	echo "-t subtitle : specify the subtitle of the plot (at the bottom), works only with 3D plots"
	echo "-A theta : the theta value, rotation of the 3D plot to adjust perspective"
	echo "-P phi : the phi value, rotation of the 3D plot to adjust perspective"
	echo "-X xlabel, -Y ylabel: label of the rerspective axis in 2D and 3D"
	echo "-x min:max, -y min:max : the range of the respective axis, only in 2D"
	echo "-L box_labels_fontsize : the fontsize of the box labels showing probability or other quantity for each box. Reasonable values are 3.5 to 5.5. Set it to zero to remove box labels if too cluttered, default is ${box_labels_fontsize}. A VALUE OF ZERO will disable box labels, useful when the number of bins is too large and the text clatters everything."
	echo "-U : flag to indicate that text, axes and border around each heatmap cell should be removed so as to unclatter the area."
	echo ""
	echo " -i inputFile"
	echo "	defines the input file."
	echo " -o outputFile"
	echo "	defines the output file."
	echo " -c col_num"
	echo "	choose the column number you wish to operate as a comma-separated list of column numbers (starting from 1) - repeat -c options if necessary, all columns will be processed unless this option is specified"
	echo " -z col_name"
	echo "	choose the column name you wish to operate - repeat -z options if necessary, all columns will be processed unless this option is specified. Use unique separator (-1), default is '${param_ifs}' or repeat -z options"
	echo " -2 random_number_generator_seed"
	echo "	useful to repeat experiments, otherwise seed is selected at random using the process id"
	echo " -3 png_width:png_height:png_fontsize : the size of the output image if any in pixels and font size, default: ${png_width}:${png_height}:${png_fontsize}"
# standard features
	echo " -p num_threads"
	echo "  if there are mutliple data to be processed, and num_threads > 1, then this processing will be done in parallel. Default num_threads is ${num_threads}."
	echo " -h"
	echo "	optional, that the first row is not numerical but it holds column names, these names will be copied to the output. If the script fails, this could be the reason (i.e. calculations with column names)."
	echo " -e sep"
	echo "	      optional, 'sep' is the separator between columns in the input file. If the script fails, this could be the reason (i.e. spaces are used as col separator but in actual terms they are tab separated)."
	echo " -R filename"
	echo "	      specify the name of the file to contain the R script to execute (because this bash file"
	echo "	      creates an R script which then runs. If this option is not used, then the output script name will be deleted."
	echo "	Otherwise, the R script will not be deleted and you can  re-run it again using R < script_name"
	echo ""
	echo "Other options:"
	echo "program by Andreas Hadjiprocopis"
	echo "andreashad2 at gmail.com"
	echo "Institute of Cancer Research"
	echo "London, 2010"
	echo "Free for everyone to copy, use, modify / quote author"
	echo ""
	exit 1
}

function column_names_to_column_numbers {
	local   _inputfile="$1"; shift # must have header
	local   _ifs="$1"; shift
	local   _column_names=${@}
	local   _rand=$$
	local   _i _ret="" _ifs2=""
	if [ "${_ifs}" != "" ]; then _ifs2="--ifs '${_ifs}'"; fi
	# dont echo anything apart from the last one before return (unless > /dev/stderr)
#	echo extract_columns.pl --input "${_inputfile}" --output /tmp/${_rand}.cn2cn --include_column_names_in_output --have_column_names_header ${_ifs2} --print_columns_and_exit
cat << EOC | sh
	extract_columns.pl --input "${_inputfile}" --output /tmp/${_rand}.cn2cn --include_column_names_in_output --have_column_names_header ${_ifs2} --print_columns_and_exit >& /dev/null
EOC
	if [ $? -ne 0 ]; then
		echo "$appname : ${FUNCNAME}, line ${LINENO} : call to the following command has failed"
		echo extract_columns.pl --input \"${_inputfile}\" --output /tmp/${_rand}.cn2cn --include_column_names_in_output --have_column_names_header ${_ifs2} --print_columns_and_exit
		return 1
	fi
	oldIFS="${IFS}"; IFS=${param_ifs}
	for _a_col_name in ${_column_names[*]}; do
#	      echo "checking for ${_a_col_name} (/tmp/${_rand}.cn2cn)" > /dev/stderr
		_a_col=`awk -F '\t' -v n="${_a_col_name}" '{if($4==n){printf ($2+1)"'"${param_ifs}"'"}}' /tmp/${_rand}.cn2cn`
		if [ "${_a_col}" == "" ]; then
#			echo "$appname : ${FUNCNAME}, line ${LINENO} : could not find column name '${_a_col_name}' in the header of the input file '${_inputfile}'."
#			return 1
			continue # baby
		fi
#	       echo "found ${_a_col}" > /dev/stderr
		_ret+="${param_ifs}${_a_col%${param_ifs}}"
	done
	IFS="${oldIFS}"
	echo "${_ret#${param_ifs}}"
	rm -f /tmp/${_rand}.cn2cn
	return 0
}
inputFilesStr=""
inputFilesLabelsStr="" # in legends and titles, use these labels instead of the filename
inputFilesHaveHeaderStr=""
inputFilesIFSStr=""; currentInputFileIFS=${defaultInputFilesIFS}
doColumnsStr=""
dontDoColumnsStr=""
_doColumnsStr=""
_dontDoColumnsStr=""
delete_Rscript_when_done=1
outputFile=""
# for standard arguments
separatorOut='\t'
Rscript_name="${rand}.R"
outputPrecision="5"
delete_Rscript_when_done=1

num_dimensions=0
declare -a bins=();
col1=
col2=
log_col='F'
main_title=""
xlab=""
ylab=""
do_what="probability"
limitsX=""; limitsY=""; limitsZ=""
assign_data_to_bins_and_save_it_to_file=0
label_input_data_and_dump_it=0
equal_height_bins='FALSE'
probabilities_2d='FALSE'
show_box_labels='TRUE'
save_unsparsed_histogram_too='FALSE'
unclatter_heatmap='FALSE'
if [ "$*" = "" ]; then
	display_help_exit $appname
fi

lastInputFile=""
while getopts "c:z:i:o:2:3:4:R:Hhe:E:p:b:d:gN:n:Qrx:y:X:Y:T:L:uUC:Z:F:" OPTION; do
	case $OPTION in
		i)
			if [ "${inputFilesStr}" == "" ]; then
				inputFilesStr="${OPTARG}"
			else
				inputFilesStr+="${_internal_arrays_ifs_}$OPTARG"
				doColumnsStr+=${_doColumnsStr#${param_ifs}}${_internal_arrays_ifs_}; _doColumnsStr=""
				dontDoColumnsStr+=${_dontDoColumnsStr#${param_ifs}}${_internal_arrays_ifs_}; _dontDoColumnsStr=""
				inputFilesHaveHeaderStr+=${_internal_arrays_ifs_}
				inputFilesIFSStr+=${_internal_arrays_ifs_}
				inputFilesLabelsStr+=${_internal_arrays_ifs_}
			fi
			lastInputFile="$OPTARG"
			;;
		F)
			if [ "${lastInputFile}" == "" ]; then echo "${appname} : an input file for which the column labels specified here apply to must be given (-i) BEFORE THIS option."; exit 1; fi
			inputFilesLabelsStr+="${OPTARG}"
			;;
		c)
			_doColumnsStr="${_doColumnsStr}${param_ifs}$OPTARG"
			;;
		C)
			_dontDoColumnsStr="${_dontDoColumnsStr}${param_ifs}$OPTARG"
			;;
		z)
			if [ "${lastInputFile}" == "" ]; then echo "${appname} : an input file for which the column names specified here apply to must be given (-i) BEFORE THIS option."; exit 1; fi
			crap=$(column_names_to_column_numbers "${lastInputFile}" "${currentInputFileIFS}" "${OPTARG}")
			if [ $? -ne 0 ]; then echo "${appname} : could not find column name '${OPTARG}' in input file '${lastInputFile}', got: ${crap}"; exit 1; fi
			if [ "${crap}" != "" ]; then if [ "${_doColumnsStr}" != "" ] ; then _doColumnsStr+=${param_ifs}; fi; _doColumnsStr+=${crap}; fi
			;;
		Z)
			if [ "${lastInputFile}" == "" ]; then echo "${appname} : an input file for which the column names specified here apply to must be given (-i) BEFORE THIS option."; exit 1; fi
			crap=$(column_names_to_column_numbers "${lastInputFile}" "${currentInputFileIFS}" "${OPTARG}")
			if [ $? -ne 0 ]; then echo "${appname} : could not find column name '${OPTARG}' in input file '${lastInputFile}', got: ${crap}"; exit 1; fi
			if [ "${crap}" != "" ]; then if [ "${_dontDoColumnsStr}" != "" ] ; then _dontDoColumnsStr+=${param_ifs}; fi; _dontDoColumnsStr+=${crap}; fi
			;;
		h)
			# in case there is a header (column names at the first row of file)
			if [ "${lastInputFile}" == "" ]; then echo "${appname} : setting default 'have header' to TRUE (for all files not having corresponding -h)."; defaultInputFilesHaveHeader="TRUE"
			else inputFilesHaveHeaderStr+="TRUE"; fi
			;;
		e)
			if [ "${lastInputFile}" == "" ]; then echo "${appname} : setting default input files IFS to '${OPTARG}' (for all files not having corresponding -e)."; defaultInputFilesIFS="${OPTARG}"
			else inputFilesIFSStr+="${OPTARG}"; fi
			currentInputFileIFS="${OPTARG}"
			;;
		o)
			if [ "$outputFile" != "" ]; then
				echo "$appname : you can specify only one output folder"
				exit 1
			fi
			outputFile=$OPTARG
			;;
		# standard arguments
		2)
			seed=$OPTARG
			;;
		3)
			png_width=`echo "${OPTARG}" | cut -d':' -f1`
			png_height=`echo "${OPTARG}" | cut -d':' -f2`
			png_fontsize=`echo "${OPTARG}" | cut -d':' -f3`
			;;
		R)
			delete_Rscript_when_done=0
			Rscript_name="$OPTARG"
			;;
		H)
			display_help_exit "$appname"
			;;
		E)
			separatorOut=$OPTARG
			;;
		4)
			outputPrecision=$OPTARG
			;;
		p)
			num_threads=$OPTARG
			;;
		# other options
		b)
			bins+=($OPTARG)
			;;
		d)
			details=$OPTARG
			;;
		g)
			log_col=T
			;;
		N)
			density_num_points=$OPTARG
			;;
		n)
			num_dimensions=$OPTARG
			;;
		Q)
			equal_height_bins="'bins'"
			;;
		r)
			probabilities_2d="T"
			;;
                x)
                        minx=`echo "$OPTARG"|cut -d':' -f1`
                        maxx=`echo "$OPTARG"|cut -d':' -f2`
                        limitsX=",xlim=c($minx,$maxx)"
                        ;;
                y)
                        miny=`echo "$OPTARG"|cut -d':' -f1`
                        maxy=`echo "$OPTARG"|cut -d':' -f2`
                        limitsY=",ylim=c($miny,$maxy)"
                        ;;
                X)
                        xlab=$OPTARG
                        ;;
                Y)
                        ylab=$OPTARG
                        ;;
                T)
                        main_title=$OPTARG
                        ;;
		L)
			box_labels_fontsize=$OPTARG
			;;
		u)
			save_unsparsed_histogram_too="TRUE"
			;;
		U)
			unclatter_heatmap="TRUE"
			;;
	esac
done
doColumnsStr+=${_doColumnsStr#${param_ifs}}${_internal_arrays_ifs_}
dontDoColumnsStr+=${_dontDoColumnsStr#${param_ifs}}${_internal_arrays_ifs_}
inputFilesHaveHeaderStr+=${_internal_arrays_ifs_}
inputFilesIFSStr+=${_internal_arrays_ifs_}
inputFilesLabelsStr+=${_internal_arrays_ifs_}

if [ "${inputFilesStr}" == "" ]; then
	echo "$0 : an input file must be specified using -i."
	exit 1
fi
if [ "$Rscript_name" = "" ]; then
	echo "$0 : script file name is not defined (use the -R option to do it)."
	exit 1
fi
if [ "$outputFile" = "" ]; then
	echo "$0 : you need to specify an output file using the '-o' option."
	exit 1
fi

if [ ${unclatter_heatmap} == "TRUE" ]; then box_labels_fontsize=0; fi

bins_cmd="breaks=vector('list',length=num_dimensions); numbins=rep(NA,num_dimensions);"; if [ ${#bins[@]} -gt 0 ]; then for((i=0;i<${#bins[@]};i++)); do if [[ "${bins[i]}" =~ ^[0-9]+$ && ! "${bins[i]}" =~ , ]]; then bins_cmd+="numbins[$((i+1))]=${bins[i]};"; else bins_cmd+="breaks[[$((i+1))]]=c(${bins[i]});"; fi; done; fi

cat << EOC > "$Rscript_name"
# R script by $appname on $now

set.seed($seed); write("$appname : starting R, seed is $seed", stderr());
suppressMessages(library(entropy))
#suppressMessages(require(scales))
suppressMessages(require(ggplot2))
source("${bindir}/library_in_R.R")

# precision and when this precision is not enough
options(digits=$outputPrecision, scipen=100)

do_processing <- function(columns_to_do_in, oridata_in, num_paddings, column_names, file_labels, include_column_numbers_in_output_file=TRUE){
# oridata_in is a list of the_files
# columns_to_do_in : pairs of as many dimensions (fileN, colN)
	n = length(columns_to_do_in)
	the_files = columns_to_do_in[seq(from=1,to=n,by=2)]
	the_columns = columns_to_do_in[seq(from=2,to=n,by=2)]
	num_dimensions = length(the_files)
	if( include_column_numbers_in_output_file == TRUE ){
		output_basename = "${outputFile}"
		for(i in 1:num_dimensions){ output_basename = paste(output_basename, sprintf(paste("%0", num_paddings[the_files[i]], "d", sep=''), the_columns[i]), sep='.') }
	} else { output_basename=paste("${outputFile}",sep="") }
	write(paste("do_processing : i am called for columns: ", paste(the_columns, collapse=",",sep=""), ", output basename: '", output_basename, "'.", sep=""), stderr())

	details=NULL; a_title=NULL
	if( "${details}" != "" ){ details="${details}" }; if( "${main_title}" != "" ){ a_title="${main_title}" }
	data_to_do = list(); for(i in 1:num_dimensions){
		data_col=oridata_in[[the_files[i]]][!is.na(oridata_in[[the_files[i]]][,the_columns[i]]),the_columns[i]]
		num_rows=length(data_col)
		if( num_rows < 2 ){ write(paste("$appname : number of rows in column ", the_columns[i], " of file '", file_labels[the_files[i]], "' are less than 2, can not continue.", sep=""), stderr()); return(FALSE); }
		stdev = sd(data_col)
		if( is.na(stdev) ){ write(paste("$appname : stdev of column ", the_columns[i], " of file '", file_labels[the_files[i]], "' could not be calculated (maybe because there are NAs or the column contains non-numbers), will not continue.", sep=""), stderr()); return(FALSE); }
#		if( stdev < 0.000001 ){ write(paste("$appname : column ", the_columns[i], " has no variation, will not continue.", sep=""), stderr()); return(FALSE) }

		if( ${log_col} == T ){ data_to_do[[i]] = log(data_col-min(data_col,na.rm=T)+1) } else { data_to_do[[i]]=data_col }
		data_col=NULL
		if( "${details}" != "" ){ details=gsub(sprintf("%%cnumber%d%%", i), the_columns[i], gsub(sprintf("%%fname%d%%", i), file_labels[the_files[i]], gsub("%cname1%", column_names[[the_files[i]]][the_columns[1]], gsub("%cname2%", column_names[[the_files[i]]][the_columns[2]], details)))) }
		if( "${main_title}" != "" ){ a_title=gsub(sprintf("%%cnumber%d%%", i), the_columns[i],gsub(sprintf("%%fname%d%%", i), file_labels[the_files[i]],gsub("%cname1%", column_names[[the_files[i]]][the_columns[1]], gsub("%cname2%", column_names[[the_files[i]]][the_columns[2]], a_title)))) }
	}
	if( !is.null(details) ){ details=paste(details, "\n", sep="") }
	if( is.null(a_title) ){ a_title=paste(unique(file_labels), collapse=' vs ', sep='') }

	if( file_labels[the_files[1]] != file_labels[the_files[2]] ){fl1 = paste(" (", file_labels[the_files[1]], ")", sep=''); fl2 = paste("(", file_labels[the_files[2]], ")", sep='')} else { fl1 = ""; fl2 = "" }
	if( "${xlab}" == "" ){ xlab=paste(column_names[[the_files[1]]][the_columns[1]], "::", the_columns[1], fl1, sep=''); if( ${log_col} == T ){ xlab=paste("log, ", xlab, sep='') } } else { xlab="${xlab}" }
	if( "${ylab}" == "" ){ ylab=paste(column_names[[the_files[2]]][the_columns[2]], "::", the_columns[2], fl2, sep=''); if( ${log_col} == T ){ ylab=paste("log, ", ylab, sep='') } } else { ylab="${ylab}" }
	# bin calculation:
	what_str=""; numbins=vector(mode='integer',length=num_dimensions); errors=vector(mode='double',length=num_dimensions)
	${bins_cmd}
	eqstr=""; if( ${equal_height_bins} == T ){ eqstr="equal-height bins,"; a_title=paste(a_title, ", equal-height bins",sep='') }

	for(i in 1:num_dimensions){
		if( !is.na(numbins[i]) ){
			chb_col=calculate_histogram_breaks(data_to_do[[i]], numbins=numbins[i], density_num_points=${density_num_points}, equal_height_bins=${equal_height_bins}, use.ks=T, calculate.error=T); if( is.null(chb_col) ){ write(paste("$appname : call to calculate_histogram() has failed for chb_col(",i,"), input file '", file_labels[the_files[i]], "', columns: ", paste(the_columns[1],the_columns[2],sep=","), sep=""), stderr()); return(FALSE) }
			breaks[[i]]=chb_col\$breaks; errors[i] = chb_col\$error; numbins[i]=length(breaks[[i]])-1
			what_str="numbins from command line"
		} else if ( !is.null(breaks[[i]]) ){ numbins[i] = length(breaks[[i]])-1; errors[i] = NA; what_str="breaks from command line" }
		else {
			# we need to automatically set number of bins
			chb_col=calculate_histogram_breaks(data_to_do[[i]], numbins=NA, density_num_points=${density_num_points}, equal_height_bins=${equal_height_bins}, use.ks=T, calculate.error=T); if( is.null(chb_col) ){ write(paste("$appname : call to calculate_histogram() has failed for chb_col(",i,"), input file '", file_labels[the_files[i]], "', columns: ", paste(the_columns[1],the_columns[2],sep=","), sep=""), stderr()); return(FALSE) }
			breaks[[i]]=chb_col\$breaks; errors[i] = chb_col\$error; numbins[i]=length(breaks[[i]])-1
			what_str="automatic number of bins calculation"
		}
		write(paste("$appname : dimension ", i,", ", eqstr, what_str, ", error: ", errors[i], ", breaks:(num:",(numbins[i]+1),") : ", paste(breaks[[i]], collapse=",", sep=""), sep=""), stderr())
	}

	# this is the histogram in N-Dim. the result is a matrix of x, y and counts where 0's are omitted, the rownames, is a string of comma separated coordinates (x,y)
	adfr <- as.data.frame(do.call(cbind, data_to_do))
	his <- calculate_histogram(values=adfr, breaks=breaks); if( is.null(his) ){ write(paste("$appname : call to calculate_histogram() has failed for his, input file '", file_labels[the_files[i]], "', columns: ", paste(the_columns[1],the_columns[2],sep=','), sep=""), stderr()); return(FALSE) }

	# metric (nz: non_zero)
	what_to_use=""; if( ${probabilities_2d} == T ){ what_to_use='probabilities' } else { what_to_use='counts' }; counts = his[,what_to_use];
	total_cells=1; for(i in 1:num_dimensions){ total_cells = total_cells * numbins[i] }; have_all_same_numbins=T; for(i in 2:num_dimensions){ if( numbins[i-1] != numbins[i] ){ have_all_same_numbins=F; break } }

	# now do individual histograms
	hists = list(); for(i in 1:num_dimensions){ hists[[i]] <- calculate_histogram(values=adfr[i], breaks=list(breaks[[i]])) }

	if( have_all_same_numbins == F ){
		write(paste("$appname : warning, in order to compare individual histograms using mutual information and covariance, we need the same number of bin - set it via '-b nb' (after each column or file spec) ...", sep=""), stderr())
		MI = NA; COV = NA
	} else {
		# calculate covariance and mutual information only if 2 cols of data specified AND numbins is SAME
		# BUT the returned hists are sparsed and zeros are omitted, need to reconstruct it with zeros added
		indiv = matrix(nrow=numbins[1],ncol=num_dimensions,data=0)
		for(j in 1:num_dimensions){
			for(i in 1:nrow(hists[[j]])){
				ii=hists[[j]][i,'binlabel1']; indiv[ii,j] = hists[[j]][i,what_to_use]
			}
		}
		MI = mi.plugin(indiv)
		COV = cov(indiv[,1], indiv[,2])
		details=paste(details,"MI:",MI,"\nCOV:",COV, sep='')
		indiv <- NULL
	}
	# we will normalise the 2d probabilities over the product of individual probabilities which is the probability that should be happening if they were independent
	# add one more column in the data.frame holding all the 2d hist data, for the expected prob:	
	his = cbind(his, expected_probability=vector(mode='double', length=nrow(his)), fold_probability=vector(mode='double', length=nrow(his)))
	rnames=rownames(his)
	labels=list(); unsparsed_hists=list();
	for(i in 1:num_dimensions){
		labels[[i]] = 1:numbins[i]
		unsparsed_hists[[i]] <- unsparse_histogram(a_histogram=hists[[i]],numbins=numbins[i])
	}
	all_combinations = expand.grid(labels); num_all_combinations = nrow(all_combinations)

	# so far, expected_probability and fold_probability is NA (=0)
	unsparsed_his = unsparse_histogram(his, numbins) # will expand the hist to include values of zero-count bins. the expected_probability and fold_probability will be NA...
	observed_probs=vector(mode='double', length=num_all_combinations);
	expected_probs=vector(mode='double', length=num_all_combinations);

	for(arow in 1:num_all_combinations){
		a_label = paste(all_combinations[arow,], collapse=',')
		observed_probs[arow] = unsparsed_his[a_label, what_to_use]
		expe_prob = 1; for(i in 1:num_dimensions){ expe_prob = expe_prob * unsparsed_hists[[i]][paste(all_combinations[arow,i]),what_to_use] }
		expected_probs[arow] = expe_prob
	}
	# this function calculates the fold change in expe/obse probs
	probability_ratio_function <- function(observed, expected, count=0){
		# natural log (log) gives symmetric results (log(1/x)=-log(x))
		#return( log( (observed/(1-observed)) / (expected/(1-expected)) ) )
	# subtract the probabilities, 0: no change, range: -1,1. Drawback: it is not normalised to size of probabilities, e.g. a change 0.01-0.02=0.98-0.99. the first has doubled!
#		return( 1000*(observed-expected)/(0.5+expected) ) # 0: no change, range: -1,1, this is equivalent to subtracting probabilities, but
#		return( 100*(observed-expected)/(0.5+expected) ) # 0: no change, range: -1,1, this is equivalent to subtracting probabilities, but
	# just a plain old ratio logged for symmetry, if zero then send some predetermined val
# AHP: improve this fucking fold change function!!! (maybe use counts as well as probs?)
		if( (observed==0) && (expected==0) ){ return(0.0) } # easy!
		else if( observed == 0 ){
			if( expected < 0.05 ){ return(0.0) } # assume zero
			else { observed=0.1; expected = expected+0.1 }
		} else if( expected == 0 ){ 
			if( observed < 0.05 ){ return(0.0) } # assume zero 
			else { expected=0.1; observed = observed+0.1 }
		}
		return( log(observed / expected) )
	# subtract probs but scale to size of prob
	#	if( (observed==0) && (expected==0) ){ return(0.0) } else { return( (observed-expected) / (observed+expected) ) }
	#	if( (observed==0.5) && (expected==0.5) ){ return(0.0) } else { return( 2.0 * (expected-observed) / (1+expected+observed) ) }
	}

	# for some probability_ratio_function you may need to shrink them if 0 or 1. This means that probs are never 0 or 1, which removes the possibility of NAs (depending on probability_ratio_function function, above)
	#observed_probs[observed_probs==0] <- 0.00001; observed_probs[observed_probs==1] <- 1-0.00001
	#expected_probs[expected_probs==0] <- 0.00001; expected_probs[expected_probs==1] <- 1-0.00001
	for(arow in 1:num_all_combinations){
		op <- observed_probs[arow]; ep <- expected_probs[arow]
		a_label = paste(all_combinations[arow,], collapse=',')
		unsparsed_his[a_label,'expected_probability'] = ep
		unsparsed_his[a_label,'fold_probability'] = probability_ratio_function(observed=op, expected=ep)
		for(i in 1:num_dimensions){
			what = paste("binrange_from", i, sep='')
			unsparsed_his[a_label,what] = breaks[[i]][all_combinations[arow,i]]
			what = paste("binrange_to", i, sep='')
			unsparsed_his[a_label,what] = breaks[[i]][all_combinations[arow,i]+1]
		}
	#	unsparsed_his[a_label,'fold_probability'] = obse_prob - expe_prob
	#	unsparsed_his[a_label,'fold_probability'] = (2/3)*( ((obse_prob+1) / (expe_prob+1)) - 0.5 )
	#	unsparsed_his[a_label,'fold_probability'] = probability_ratio_function(observed=obse_prob, expected=expe_prob)
	#	unsparsed_his[a_label,'fold_probability'] = (obse_prob-expe_prob) / (obse_prob+expe_prob)
	}
	# update the 'his' (the sparse histogram) of expe and fold probs because this is the one we save into file by default.
	for(a_label in rnames){
		his[a_label,'expected_probability'] = unsparsed_his[a_label, 'expected_probability']
		his[a_label,'fold_probability'] = unsparsed_his[a_label,'fold_probability']
	}

	unsparsed_counts = unsparsed_his[,what_to_use]
	nz_cells = nrow(his); counts=his[,'counts']
	sum_counts = sum(counts); nz_mean = mean(counts); nz_stdev = sd(counts)
	total_mean=1/(1/nz_mean + (total_cells-nz_cells)/sum_counts) # mean by adding the zeros without creating the huge sparse matrix
	total_stdev=sqrt( (sum((total_mean-counts)^2) + (total_cells-nz_cells)*(total_mean^2) ) / (total_cells-1)) # R's sd uses denominator of (n-1)
	min_counts = min(counts); max_counts = max(counts)
	nz_entropy = simple_entropy(counts)
	total_entropy = simple_entropy(unsparsed_counts)

	axis.labels.x = data.frame(breaks=1:numbins[1]+0.5,labels=1:numbins[1]); axis.labels.y = data.frame(breaks=1:numbins[2]+0.5,labels=1:numbins[2])

	# now do a t-test comparing expected and observed probs, do one for sparse and one for unsparsed
	# pvalue == 0 -> difference is significant, BUT remember that this is OVERALL - it may well be that just one bin shows difference and all others are same!
	# this will be evident in the histogram plot (png) file
	t_test_results.unsparsed.pvalue=NA; tryCatch({
		t_test_results.unsparsed = t.test(x=unsparsed_his[,'expected_probability'], y=unsparsed_his[,'probabilities'], conf.level=0.95, alternative='two.sided')
		t_test_results.unsparsed.pvalue = t_test_results.unsparsed\$p.value
	}, error=function(ex){}, warning=function(ex){})
	# levene test, this stupid test requires a melted dataframe!!
	levene_test_results.unsparsed.pvalue=NA; tryCatch({
		levene_test_results.unsparsed = car:::leveneTest(y=c(unsparsed_his[,'expected_probability'],unsparsed_his[,'probabilities']),group=gl(2,num_all_combinations,labels=paste("Group",1:2,sep='')))
		levene_test_results.unsparsed.pvalue = levene_test_results.unsparsed[1,'Pr(>F)']
	}, error=function(ex){}, warning=function(ex){})
	# do one for sparse (i.e. non-zero counts)
	t_test_results.sparse.pvalue=NA; tryCatch({
		t_test_results.sparse = t.test(x=his[,'expected_probability'], y=his[,'probabilities'], conf.level=0.95, alternative='two.sided')
		t_test_results.sparse.pvalue = t_test_results.sparse\$p.value
	}, error=function(ex){}, warning=function(ex){})
	# levene test, this stupid test requires a melted dataframe!!
	levene_test_results.sparse.pvalue=NA; tryCatch({
		levene_test_results.sparse = car:::leveneTest(y=c(his[,'expected_probability'],his[,'probabilities']),group=gl(2,nz_cells,labels=paste("Group",1:2,sep='')))
		levene_test_results.sparse.pvalue = levene_test_results.sparse[1,'Pr(>F)']
	}, error=function(ex){}, warning=function(ex){})

	# the rest only applies to 2 dimensions i.e. print the heatmap as 2d
	details=sprintf("%s\nEntropy: %.2f\n(t-test)pv (sparse/unsparse): %.2f / %.2f\n(levene)pv (sp/unsp): %.2f / %.2f\nbox legend: fold change (observed / expected counts)", details, nz_entropy, t_test_results.sparse.pvalue, levene_test_results.sparse.pvalue, t_test_results.unsparsed.pvalue, levene_test_results.unsparsed.pvalue)

	# on equal_height bins we don't have histograms only the heatmap, so
	# the direction of the legend can be vertical. otherwise horizontal.
	if( ${equal_height_bins} == FALSE ){ legend.direction="horizontal" } else { legend.direction="vertical" }

	if( ${unclatter_heatmap} == TRUE ){
		axis.labels.x=NULL
		axis.labels.y=NULL
		cell.color=NULL # border around each cell of the heatmap
		gridline.major=element_blank() # remove grid
		gridline.minor=element_blank()
		box.height_labels.show=FALSE
		box.color.outline=NULL
		bins_axis_labels=NULL
	} else {
		box.height_labels.show=T
		cell.color=alpha('black',0.25)
		gridline.major=element_line(colour=alpha('blue',0.15),size=0.2)
		gridline.minor=element_line(colour=alpha('purple',0.05),size=0.2)
		box.color.outline=alpha('black',0.75)
	}
	a_filename=paste(sep='',output_basename, ".png");
	png(filename=a_filename, width=${png_width}, height=${png_height}, pointsize=${png_fontsize})
	pHeatmap <- plot_heatmap(
		datain=data.frame(
			x=unsparsed_his[,'binlabel1']+0.5,
			y=unsparsed_his[,'binlabel2']+0.5,
	# this is printing the metric of fold change
			values=unsparsed_his[,'fold_probability'],
	# this is printing '10/12' meaning 10 were observed but 12 were expected - the 12 may not sum-up to totalvalues as there is roundups see below.
			extra_values=sprintf("%d/%.0f",
				unsparsed_his[,'counts'], # observed counts
				round(unsparsed_his[,'expected_probability'] * sum_counts) # expected counts
#				unsparsed_his[,'expected_probability'] * sum_counts # expected counts
			)
# debug: to see probabilities uncomment this
#			extra_values=sprintf("%.3f/%.3f",
#				unsparsed_his[,'counts'] / sum_counts, # observed counts
#				unsparsed_his[,'expected_probability']
#			)
		),
		box.labels.sprintfFormat="%.2f",
		box.labels.extra_values.sprintfFormat="\n%s",
		fontsize=${png_fontsize},
		axis.label.x=xlab,axis.label.y=ylab ${limitsX/xlim=/axis.limits.x=} ${limitsY/ylim=/axis.limits.y=},
		axis.labels.x=axis.labels.x,
		axis.labels.y=axis.labels.y,
		box.labels.fontsize=${box_labels_fontsize},
		legend.title='change',
	#	box.labels.limits=c(-1,1),
		box.labels.middle=0.0, # where the middle color goes
		plot.title=NULL,
		legend.direction=legend.direction,

#		cell.frame_color=cell.color,
		gridline.major=gridline.major,
		gridline.minor=gridline.minor
	)
	if( ${equal_height_bins} == FALSE ){
		breaksFrom = head(breaks[[1]],-1); breaksTo = breaks[[1]][-1]; if( ${unclatter_heatmap} == FALSE ){ bins_axis_labels = data.frame(breaks=(breaksFrom+breaksTo)/2,labels=1:numbins[1]) }
		(head(breaks[[1]],-1)+breaks[[1]][-1])/2
		pHistogramX <- plot_boxes(
			data.boxes=data.frame(breaksFrom=breaksFrom,breaksTo=breaksTo,counts=unsparsed_hists[[1]][paste(1:numbins[1]),'counts']),
			axis.labels.x=bins_axis_labels,
			axis.label.x=NULL,
			axis.label.y=NULL,
			box.height_labels.fontsize_multiplier=0.275, # size will be smaller by fontsize at 0.275x

			gridline.major=gridline.major,
			gridline.minor=gridline.minor,
			box.height_labels.show=box.height_labels.show,
			box.color.outline=box.color.outline
		)
		p2 <- pHistogramX+theme(legend.position = "none") # remove legend

		breaksFrom = head(breaks[[2]],-1); breaksTo = breaks[[2]][-1]; if( ${unclatter_heatmap} == FALSE ){ bins_axis_labels = data.frame(breaks=(breaksFrom+breaksTo)/2,labels=1:numbins[2]) }
		pHistogramY <- plot_boxes(
			data.boxes=data.frame(breaksFrom=breaksFrom,breaksTo=breaksTo,counts=unsparsed_hists[[2]][paste(1:numbins[2]),'counts']),
			axis.labels.x=bins_axis_labels,
			axis.label.x=NULL,
			axis.label.y=NULL,
			box.height_labels.fontsize_multiplier=0.275, # size will be smaller by fontsize at 0.275x

			gridline.major=gridline.major,
			gridline.minor=gridline.minor,
			box.height_labels.show=box.height_labels.show,
			box.color.outline=box.color.outline
		)
		p3 <- pHistogramY+theme(legend.position = "none")+coord_flip() # remove legend and flip coordinates
		# we will add histograms, so remove legend to be placed in separate quarter
#		legend <- pHeatmap + theme(keep="legend_box",legend.text=element_text(size=12,hjust=0))
# what the fuck, after ggplot 2017
		legend <- extract_legend_from_plot(pHeatmap)
		pHeatmap <- pHeatmap+theme(legend.position = "none") # remove legend from the heatmap but place it later in the grid
		nr <- 3
		if( !is.null(a_title) ){
			multiplot(ncol=2,nrow=4,widths=c(4,4),heights=c(1.0,4.2,3,1), list(pHeatmap,2,1), list(p2,3,1), list(p3,2,2), legend=list(legend,4,2))
			grid.text(a_title, vp=viewport(layout.pos.row=1,layout.pos.col=1:2),just=c('centre','top'))
			nr <- nr + 1
		} else {
			multiplot(ncol=2,nrow=3,widths=c(4,4),heights=c(4.2,3,1), list(pHeatmap,1,1), list(p2,2,1), list(p3,1,2), legend=list(legend,3,2))
		}
		if( !is.null(details) ){ grid.text(details, vp=viewport(layout.pos.row=nr-1,layout.pos.col=2),just=c('centre','top')) }
	} else {
		nr <- 1
		if( !is.null(a_title) ){
			multiplot(ncol=2,nrow=2,widths=c(4.2,1.5),heights=c(1.0,4), list(pHeatmap,2,1))
			grid.text(a_title, vp=viewport(layout.pos.row=1,layout.pos.col=1:2),just=c('centre','top'))
			nr <- nr + 1
		} else {
			multiplot(ncol=2,nrow=1,widths=c(4.2,1.5),heights=c(4), list(pHeatmap,1,1))
		}
		if( !is.null(details) ){ grid.text(details, vp=viewport(layout.pos.row=nr,layout.pos.col=2)) }

	#	legend <- pHeatmap + theme(keep="legend_box",legend.text=element_text(size=12,hjust=0))
	#	pHeatmap <- pHeatmap+theme(legend.position = "none") # remove legend
	#	multiplot(ncol=2,nrow=3,widths=c(4,2),heights=c(0.5,4,4), list(pHeatmap,2:3,1), legend=list(legend,2,2))
	#	if( !is.null(details) ){ grid.text(details, vp=viewport(layout.pos.row=3,layout.pos.col=2)) }
	#	if( !is.null(a_title) ){ grid.text(a_title, vp=viewport(layout.pos.row=1,layout.pos.col=1:2)) }
	}
	dev.off();

	write("$appname : details: ${details}", stderr())
	write("$appname : some statistics:", stderr())
	write(sprintf("non-zero counts : mean: %f, stdev: %f, entropy: %f", nz_mean, nz_stdev, nz_entropy), stderr())
	write(sprintf("including zero counts : mean: %f, stdev: %f, entropy: %f", total_mean, total_stdev, total_entropy), stderr())
	if( !is.na(MI) ){ write(sprintf("mutual information: %f", MI), stderr()) }
	if( !is.na(COV) ){ write(sprintf("covariance: %f", COV), stderr()) }
	write("$appname : the statistical tests below, test that the expected and observed counts/probabilities of the N-dimensional histogram differ significantly OVERALL (i.e. over all bins - not just one bin) at the 5% confidence level. A pvalue < 0.05 yields a significant difference.", stderr())
	write(sprintf("statistical tests for non-zero counts : t-test:%f, levene:%f", t_test_results.sparse.pvalue, levene_test_results.sparse.pvalue), stderr())
	write(sprintf("statistical tests for including zero counts : t-test:%f, levene:%f", t_test_results.unsparsed.pvalue, levene_test_results.unsparsed.pvalue), stderr())

	a_filename=paste(sep='',output_basename, ".counts.txt")
	write(paste(sep='',"$appname : saving sparse histogram in '", a_filename,"'"), stderr())
	write.table(his, file=a_filename, sep="${separatorOut}", col.names=TRUE, row.names=FALSE, append=FALSE, quote=FALSE)
	if( ${save_unsparsed_histogram_too} == TRUE ){
		a_filename=paste(sep='',output_basename, ".counts.unsparsed.txt")
		write(paste(sep='',"$appname : saving unsparsed histogram in '", a_filename, "'"), stderr())
		write.table(unsparsed_his, file=a_filename, sep="${separatorOut}", col.names=TRUE, row.names=FALSE, append=FALSE, quote=FALSE)
	}

	a_filename=paste(sep='',output_basename, ".info.txt")
	write(paste("nz_mean", "nz_stdev", "nz_entropy",
		    "total_mean", "total_stdev", "total_entropy",
		    "MI", "COV",
		    "nz(t-test)pv", "nz(levene)pv",
		    "total(t-test)pv", "total(levene)pv",
		    "details", "output_basename", sep="\t"), file=a_filename, append=FALSE)
	write(paste(	nz_mean, nz_stdev, nz_entropy,
			total_mean, total_stdev, total_entropy,
			MI, COV,
			t_test_results.sparse.pvalue, levene_test_results.sparse.pvalue,
			t_test_results.unsparsed.pvalue, levene_test_results.unsparsed.pvalue,
			"${details}", output_basename, sep="\t"),
		file=a_filename, append=TRUE)
	write(paste("do_processing : finished, for columns: ", paste(the_columns[1],the_columns[2],sep=","), ", output basename: '", output_basename, "'.", sep=""), stderr())
	return(TRUE)
} # end of do_processing function

# R script by $appname on $now
inp_datas <- process_column_specs_and_read_input_files(
        inputFilesStr="${inputFilesStr}",
        inputFilesLabelsStr="${inputFilesLabelsStr}",
        doColumnsStr="${doColumnsStr}",
        dontDoColumnsStr="${dontDoColumnsStr}",
        param_ifs1="${param_ifs}",
        param_ifs2="${_internal_arrays_ifs_}",
        inputFilesHaveHeaderStr="${inputFilesHaveHeaderStr}",
        inputFilesIFSStr="${inputFilesIFSStr}",
        defaultInputFilesHaveHeader=${defaultInputFilesHaveHeader}, # this is boolean!!! dont quote
        defaultInputFilesIFS="${defaultInputFilesIFS}",
        return_back_data_from_files=TRUE
); if( is.null(inp_datas) ){ write("${appname} : call to process_column_specs_and_read_input_files has failed.", stderr()); quit(status=1) }
num_dimensions=inp_datas\$num_columns_to_do

# find all pairwise combinations of columns to do from each input file all mixed up, e.g. f1:1,2 f2:3,4
column_pairs = list() # this is a mixture of file numbers and column in that file number, e.g. 1 2 3 4 (file 1, col2 with fil3, col4)
i=1; for(f1 in 1:inp_datas\$num_input_files){
	for(f2 in 1:inp_datas\$num_input_files){
		if( f1 == f2 ){
			for(p1 in 1:inp_datas\$num_columns_to_do[f1]){
				for(p2 in 1:inp_datas\$num_columns_to_do[f2]){
					if( p2 <= p1 ){ next }
					column_pairs[[i]] = c(f1, inp_datas\$columns_to_do[[f1]][p1], f2, inp_datas\$columns_to_do[[f2]][p2])
					i = i + 1
				}
			}
		} else {
			for(p1 in 1:inp_datas\$num_columns_to_do[f1]){
				for(p2 in 1:inp_datas\$num_columns_to_do[f2]){
					column_pairs[[i]] = c(f1, inp_datas\$columns_to_do[[f1]][p1], f2, inp_datas\$columns_to_do[[f2]][p2])
					i = i + 1
				}
			}
		}
	}
}
#x=column_pairs[[1]]; column_pairs=list(); column_pairs[[1]]=x
num_processes = length(column_pairs)
write(num_processes, stderr())

stime = NULL; res = NULL
if( num_processes > 1 ){
	include_column_numbers_in_output_file=TRUE
	if( ${num_threads} > 1 ){
		suppressMessages(library(parallel))
		write(paste(sep='',"${appname} : parallelising ", num_processes, " processes over ${num_threads} threads..."), stderr())
		stime <- system.time({ res <- mclapply(column_pairs, do_processing, inp_datas\$oridata, inp_datas\$num_paddings, inp_datas\$column_names, inp_datas\$file_labels, include_column_numbers_in_output_file, mc.cores=${num_threads}) })
	} else {
		write(paste(sep='',"${appname} : running sequentially ..."), stderr())
		stime <- system.time({ res <- lapply(column_pairs,  do_processing, inp_datas\$oridata, inp_datas\$num_paddings, inp_datas\$column_names, inp_datas\$file_labels, include_column_numbers_in_output_file) })
	}
} else {
	include_column_numbers_in_output_file=FALSE
	write(paste(sep='',"${appname} : running sequentially ..."), stderr())
	stime <- system.time({ res <- lapply(column_pairs,  do_processing, inp_datas\$oridata, inp_datas\$num_paddings, inp_datas\$column_names, inp_datas\$file_labels, include_column_numbers_in_output_file) })
}
failed=F; for(i in 1:length(res)){ if( (is.logical(res[[i]])==FALSE) || (res[[i]]==FALSE) ){ write(paste("${appname} : processing failed for process ", i, "(col spec:", paste(column_pairs[[i]],sep='',collapse=','),").\n", sep=''), stderr()); failed=T } }
if( failed == T ){
	write(paste(sep='',"\n${appname} : failed."), stderr())
	quit(status=1)
}
write(paste(sep='',"\n${appname} : done all, with times user:", stime[1], ", system:", stime[2], ", elapsed:", stime[3]), stderr())
EOC

echo "$appname : executing R script ($Rscript_name)"
R --vanilla < "$Rscript_name" > /tmp/rlog$rand.txt
if [ $? -ne 0 ]; then
#	cat /tmp/rlog$rand.txt; echo "$appname : error executing R script ($Rscript_name)";
	echo "$appname : error executing R script ($Rscript_name)";
	echo "Command line:"
	echo "$0 ${*}"
	exit 1
fi
echo "Command line:"
echo "$0 ${*}"
rm -f /tmp/rlog$rand.txt /tmp/$rand.* 
if [ "${delete_Rscript_when_done}" -eq 1 ]; then rm -f "$Rscript_name"; fi
exit 0
