#!/usr/bin/R

source('lib/IO.R');
source('lib/NA.R');

infile='data/dat1.csv'
outcleanfile='dat1.clean.csv'
dat1 <- data.frame(read_data(
	filename=infile
))
if( is.null(dat1) ){
	cat("call to read_data() has failed for file '",infile,"'.\n", sep='')
	quit(status=1)
}

results <- clean_dataset(
	inp=dat1[1:8000,],
	rng.seed=1234,
	random_NAs_to_introduce_in_each_column_percent=5/100,
	repeats=16,
	ncores=8
)
stats_of_imputation <- results$stats
clean_dat1 <- results$imputed

if( ! save_data(clean_dat1, outcleanfile) ){
	cat("call to save_data() has failed for file '",outcleanfile,"'.\n", sep='')
	quit(status=1)
}
