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

results <- assess_na_methods_repeatedly(
	inp=dat1[1:8000,],
	methods=c("mice", "kNN"),
	rng.seed=1234,
	random_NAs_to_introduce_in_each_column_percent=5/100,
	columns_to_do=c("A","B","C","D","E","F"),
	repeats=16,
	ncores=8
)
print(results)
