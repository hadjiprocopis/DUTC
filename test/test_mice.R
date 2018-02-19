#!/usr/bin/R

source('lib/IO.R');
source('lib/NA.R');

infile='data/dat1.csv'
outcleanfile='dat1.clean.csv'
xdat1 <- data.frame(read_data(
	filename=infile
))
if( is.null(xdat1) ){
	cat("call to read_data() has failed for file '",infile,"'.\n", sep='')
	quit(status=1)
}
dat1 <- xdat1[1:8000,]

results <- deal_with_na_using_mice(
	inp=dat1,
	columns_to_do=c("A","B","C","D","E","F"),
	rng.seed=1234
)
