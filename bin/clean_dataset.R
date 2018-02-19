#!/usr/bin/env Rscript

#'---
#'title: "Data preparation: filling or removing the NAs"
#' author: Andreas Hadjiprocopis
#' date: February, 2018
#'output:
#'  pdf_document:
#'    toc: true
#'    highlight: zenburn
#'  html_document:
#'    toc: true
#'    theme: united
#'---

source('lib/IO.R');
source('lib/NA.R');

infile='data/dat1.csv'
outdir='cleaned_data';
outcleanfile=file.path(outdir, 'dat1.clean.csv')
outeliminateNAfile=file.path(outdir, 'dat1.eliminateNA.csv')

#' Read the original csv data file (dat1.csv)
dat1 <- data.frame(read_data(
	filename=infile
))
if( is.null(dat1) ){
	cat("call to read_data() has failed for file '",infile,"'.\n", sep='')
	quit(status=1)
}

#' Remove all rows with at least one NA
results <- clean_dataset(
	inp=dat1,
	methods=c('remove_entire_row')
)
if( is.null(results) ){
	cat("call to clean_dataset() has failed for file '",infile,"'.\n", sep='')
	quit(status=1)
}
#' this is the clean data
clean_dat1 <- results$imputed
#' And save it
if( ! save_data(clean_dat1, outeliminateNAfile) ){
	cat("call to save_data() has failed for file '",outeliminateNAfile,"'.\n", sep='')
	quit(status=1)
}

#' Alternatively one can replace (fill) NAs with guesses.
#' See notes at the end of this document.
results <- clean_dataset(
	inp=dat1,
	methods=c("mice", "kNN"),
	rng.seed=1234,
	# how many artificial NAs to introduce?
	random_NAs_to_introduce_in_each_column_percent=5/100,
	# number of repeats for the assessment
	repeats=16,
	# parallelise the procedure over so many CPU cores
	ncores=8
)
if( is.null(results) ){
	cat("call to clean_dataset() has failed for file '",infile,"'.\n", sep='')
	quit(status=1)
}
#' this is the cleaned data as a list
clean_dat1 <- results$imputed

#' Statistics of the filling NAs procedure
stats_of_imputation <- results$stats
print(stats_of_imputation)

#' finally, save the data
if( ! save_data(clean_dat1, outcleanfile) ){
	cat("call to save_data() has failed for file '",outcleanfile,"'.\n", sep='')
	quit(status=1)
}

#' The input data contains quite a few NAs
#' I have created a few functions in lib/NA.R which
#' either remove all rows with at least one NA
#' or try to fill the NA with an appropriate value
#' based on the other values in the row.
#' Remove the NAs is straightforward but filling
#' them out risks distorting the dataset.
#' I have used two methods for filling:
#' 1. kNN
#' 2. mice
#' I have created a function to assess which method works best.
#' This is done as follows.
#' 1. Remove all NAs from the dataset.
#' 2. Create some random NAs in the clean dataset, say 5% of the total items.
#' 3. Apply each method to filling the artificial NAs.
#' 4. Calculate a metric of badness of fill by, say, simple root mean square of the difference between filled and actual values.
#' 5. Report the method yielding the lowest value.
#' 6. Repeat this process N times.
#' Method 'kNN' has done consistently better. And so this is the method used.
#' The main function is clean_dataset() which takes parameters to either
#' eliminate rows with NAs or fill NAs. Additionally, input parameters
#' control the number of repeats should a filling-NAs route was taken,
#' the percentage of artificial NAs to introduce and
#' the methods to assess, and the column names to process.
#' A very important feature of the clean_dataset() function
#' is that assessment, which is quite time-consuming, can be
#' *parallelised* over as many CPU cores as specified.
#' A *parallelisation* option is important to exist in such
#' methods and I always spend a bit more effort in implementing
#' it, and considerably more in ... debugging it. But it is worthy.

#' here is a dump of the NA.R library:
sourcecode <- paste(readLines("lib/NA.R"), collapse="\n")
cat(paste("```r", sourcecode, "```", sep="\n"))
