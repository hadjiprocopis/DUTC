#!/usr/bin/env Rscript

#'---
#'title: "timeseries analysis : normality, q-q plots, distr estimation"
#' author: Andreas Hadjiprocopis
#' date: February, 2018
#'output:
#'  pdf_document:
#'    latex_engine: pdflatex
#'    toc: true
#'    highlight: zenburn
#'  html_document:
#'    toc: true
#'    theme: united
#'---

set.seed(1234)

source('lib/UTIL.R');
source('lib/DATA.R');
source('lib/IO.R');
source('lib/TS.R');
source('lib/MC.R');
source('lib/SEASON.R');
source('lib/MIXTURES.R');

infile='cleaned_data/dat1.eliminateNA.csv'

dat1 <- data.frame(read_data(
	filename=infile
))
if( is.null(dat1) ){
	cat("call to read_data() has failed for file '",infile,"'.\n", sep='')
	quit(status=1)
}
dummy <- remove_columns(inp=dat1, colnames_to_remove=c('id'))
dat1_noid <- dummy[['retained']]
dat1_id <- dummy[['removed']]
dat1_detrended_id <- list(c(dat1_id[['id']][2:length(dat1_id[['id']])]))
names(dat1_detrended_id) <- c('id')
# detrend the data
dat1_detrended <- detrend_dataset(inp=dat1_noid,times=1)

#' Check for normality: do data come from a Gaussian distribution process
#' For the Anderson-Darling and Shapiro-Wilk normality test
#' the p-value refers to the null hypothesis being that data DOES not come
#' from a Gaussian distribution process (i.e. high p-value means 'a high probability that data comes from Gaussian distribution process')
#' Data (detrended or not) do not seem to come from a Gaussian distribution process
#' because the p-values of the test are very very small.
#' First let's test a normally distributed sample just to see
#' what kind of p-values to expect.
library(nortest)
print(ad.test(rnorm(n=1000, mean=5, sd=3)))
print(ad.test(rnorm(n=1000, mean=5, sd=1)))
print(shapiro.test(rnorm(n=1000, mean=5, sd=3)))
print(shapiro.test(rnorm(n=1000, mean=5, sd=1)))
#' it looks like if p-value is not extremely small then we can assume
#' it passes the test of normality.

for(acol in names(dat1_noid)){
	cat("original data, col", acol, "\n", sep='')
	x<-ad.test(dat1_noid[[acol]])
	print(x)
}
for(acol in names(dat1_detrended)){
	cat("detrended data, col", acol, "\n", sep='')
	x<-ad.test(dat1_detrended[[acol]])
	print(x)
}
#' Conclusion: data do not pass the Normality test.
#' This means that non-parameteric statstical tools
#' should be used for assessing the data. For example Pearson's
#' require bivariate normality while Spearman's does not.

#' print summary statistics for original data:
for(acol in names(dat1_noid)){
	x <- dat1_noid[[acol]]
	cat("Original data, column ", acol, ", mean=", mean(x), ", sd=", sd(x), "\n", sep='')
}
#' print summary statistics for detrended data:
for(acol in names(dat1_detrended)){
	x <- dat1_detrended[[acol]]
	cat("Detrended data, column ", acol, ", mean=", mean(x), ", sd=", sd(x), "\n", sep='')
}

#' Boxplot of all of original data's columns in one page for comparison
#' Note: a boxplot shows the minimum and maximum values in the whiskers
#' The interquartile range (between lower and upper quartiles) is
#' the rectangle's width. The median is the indicator line inside the
#' rectangle.
boxplot(
	dat1_noid,
	main=paste0('Original data boxplot', sep='')
)
#' boxplot of original data per column:
for(acol in names(dat1_noid)){
	x <- dat1_noid[[acol]]
	boxplot(
		x,
		main=paste0('Original data, col ', acol, sep='')
	)
}
#' boxplot of all of detrended data's columns in one page for comparison
boxplot(
	dat1_noid,
	main=paste0('Detrended data', sep='')
)
#' boxplot of detrended data per column:
for(acol in names(dat1_noid)){
	x <- dat1_detrended[[acol]]
	boxplot(
		x,
		main=paste0('Detrended data, col ', acol, sep='')
	)
}

#' Do a scatter plot of pairs of variables with a regression line added
#' for the original data
library(ggpubr)
adf <- list2dataframe(dat1_noid)
cnames <- names(adf)
Ncnames <- length(cnames)
for(i in 1:Ncnames){
	acol1 = cnames[i]
	for(j in (i+1):Ncnames){
		acol2 = cnames[j]
		ggscatter(
			adf,
			x=acol1,
			y=acol2,
			add="reg.line",
			conf.int=TRUE,
			cor.coef=TRUE,
			cor.method = "spearman",
			title=paste0('Scatter plot of original data, columns ', acol1, ' and ', acol2,sep='')
		)
	}
}

#' Do a scatter plot of pairs of variables with a regression line added
#' for the detrended data
library(ggpubr)
adf <- list2dataframe(dat1_detrended)
cnames <- names(adf)
Ncnames <- length(cnames)
for(i in 1:Ncnames){
	acol1 = cnames[i]
	for(j in (i+1):Ncnames){
		if( j > Ncnames ){ next }
		acol2 = cnames[j]
		ggscatter(
			adf,
			x=acol1,
			y=acol2,
			add="reg.line",
			conf.int=TRUE,
			cor.coef=TRUE,
			cor.method = "spearman",
			title=paste0('Scatter plot of detrended data, columns ', acol1, ' and ', acol2,sep='')
		)
	}
}

#' Print and plot (Spearman) correlation matrix of original data (pairwise).
#' Correlation tells us whether two variables move together
#' in the same or opposite direction (correlations of +1 and -1) or are their movement
#' is not related at all (correlation of 0).
#' Correlation is related to covariance in that it is essentially covariance
#' normalised over the product of the standard deviations of the two variables.
cnames <- names(dat1_noid)
Ncnames <- length(cnames)
amatr <- matrix(unlist(dat1_noid), ncol=Ncnames, byrow=T)
colnames(amatr) <- cnames
library(corrplot)
cor_matrix <- cor(amatr, method='spearman')
cor_pvalues <- cor.mtest(amatr)
#' the correlation matrix of original columns set:
print("correlation matrix of original data:")
print(cor_matrix)
#' statistical significance of the correlation matrix
print("and its statistical significance:")
colnames(cor_pvalues$p) <- cnames
rownames(cor_pvalues$p) <- cnames
print(cor_pvalues$p)

acolor <- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(
	cor_matrix,
	p.mat=cor_pvalues$p,
	method="ellipse",
	order="original",
	col=acolor,
	type='upper',
	sig.level=0.0,
	insig="p-value",
	title='correlation matrix of original data'
)

#' plot correlation matrix of detrended data (pairwise)
cnames <- names(dat1_detrended)
Ncnames <- length(cnames)
amatr <- matrix(unlist(dat1_detrended), ncol=Ncnames, byrow=T)
colnames(amatr) <- cnames
library(corrplot)
cor_matrix <- cor(amatr, method='spearman')
cor_pvalues <- cor.mtest(amatr)
#' the correlation matrix of detrended columns set:
print("correlation matrix of original data:")
print(cor_matrix)
#' statistical significance of the correlation matrix
colnames(cor_pvalues$p) <- cnames
rownames(cor_pvalues$p) <- cnames
print("and its statistical significance:")
print(cor_pvalues$p)

acolor <- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(
	cor_matrix,
	p.mat=cor_pvalues$p,
	method="ellipse",
	order="original",
	col=acolor,
	type='upper',
	sig.level=0.0,
	insig="p-value",
	title='correlation matrix of original data'
)

#' Correlation summary of original data:
library(PerformanceAnalytics)
chart.Correlation(dat1_noid, histogram=TRUE, pch=19)

#' Correlation summary of detrended data:
library(PerformanceAnalytics)
chart.Correlation(dat1_noid, histogram=TRUE, pch=19)

#' plot covariance matrix of original data (pairwise)
cnames <- names(dat1_noid)
Ncnames <- length(cnames)
amatr <- matrix(unlist(dat1_noid), ncol=Ncnames, byrow=T)
colnames(amatr) <- cnames
cov_matrix <- cov(amatr)
#' the covariance matrix of original columns set:
print("covariance matrix of original data:")
print(cov_matrix)
p <- ggplot_heatmap(
	inp=cov_matrix,
	plot.title='Covariance matrix of original data'
)
print(p)

#' plot covariance matrix of detrended data (pairwise)
#' Covariance is similar to variance. The latter applies to a single variable
#' Covariance applies to two variables and is the sum of the
#' product of each data-point's deviation from the mean
#' $$\sum_{i=1}^{N} \frac{(x_1-\bar(x_1))(x_2-\bar(x_2))}{N-1}$$

cnames <- names(dat1_detrended)
Ncnames <- length(cnames)
amatr <- matrix(unlist(dat1_detrended), ncol=Ncnames, byrow=T)
colnames(amatr) <- cnames
cov_matrix <- cov(amatr)
#' the covariance matrix of detrended columns set:
print("covariance matrix of detrended data:")
print(cov_matrix)
p <- ggplot_heatmap(
	inp=cov_matrix,
	plot.title='Covariance matrix of detrended data'
)
print(p)

#' plot the original data
library(ggplot2)
library(reshape2)
adat = dat1_noid; adat[['id']] = dat1_id[['id']]
for(acol in names(dat1_noid)){
	atitle <- paste0("original data, column ", acol)
	cat("plot of ", atitle, "\n", sep='')
	print(
		ggplot(adat, aes_string(x='id',y=acol))+
		geom_line()+
		ggtitle(atitle)+
		xlab('time')+
		geom_smooth(method='gam')
	)
}
#' plot detrended data
adf <- data.frame(dat1_detrended); adf$id <- dat1_detrended_id[['id']]
for(acol in names(dat1_detrended)){
	atitle <- paste0("detrended data, column ", acol)
	cat("plot of ", atitle, "\n", sep='')
	print(
		ggplot(dat1, aes_string(x='id',y=acol))+
		geom_line()+
		ggtitle(atitle)+
		xlab('time')+
		geom_smooth(method='gam')
	)
}

#' discretise the data to five levels
#' dat1_discrete <- discretise(dat1_noid, levels=5)
#' plot discrete data
if( FALSE ){
for(acol in names(dat1_discrete)){
	atitle <- paste0('Original data, discretised to 5 levels')
	adf <- data.frame(data=dat1_discrete[[acol]]$discretised)
	adf$id <- seq.int(nrow(adf))
	p <- ggplot(
		adf,
		aes_string(x='id',y='data')
	)+geom_line()+ggtitle(atitle)
	print(p)
}
}

#' discretise the detrended data
#' dat1_detrended_discrete <- discretise(dat1_detrended, levels=5)
#' plot detrended discrete data
if( FALSE ){
for(acol in names(dat1_detrended_discrete)){
	atitle <- paste0('Detrended data, discretised to 5 levels')	
	adf <- data.frame(data=dat1_detrended_discrete[[acol]]$discretised); adf$id <- seq.int(nrow(adf))
	p <- ggplot(
		adf,
		aes_string(x='id',y='data')
	)+geom_line()+ggtitle(atitle)
	print(p)
}
} # if( FALSE )

#' find unique values
uniq_vals <- unique_values(inp=dat1_noid)
histo_dat1 <- histo(inp=dat1_noid,unique_vals=uniq_vals)
for(acol in names(dat1_noid)){
	cat("found ", uniq_vals$num_unique_values[1,acol], " unique values in the original data, column ", acol, "\n", sep='')
	barplot(
		table(dat1_noid[[acol]]),
		main=paste0('Original data, column ', acol, ' : ', uniq_vals$num_unique_values[1,acol], ' unique values.', sep=''),
		ylab='unique levels'
	)
}
#print(histo_dat1)

#' find unique values in the detrended data
uniq_vals_detrended <- unique_values(inp=dat1_detrended)
histo_dat1_detrended <- histo(inp=dat1_detrended,unique_vals=uniq_vals_detrended)
for(acol in names(dat1_detrended)){
	cat("found ", uniq_vals_detrended$num_unique_values[1,acol], " unique values in the detrended data, column ", acol, "\n", sep='')
	barplot(
		table(dat1_detrended[[acol]]),
		main=paste0('Detrended data, column ', acol, ' : ', uniq_vals_detrended$num_unique_values[1,acol], ' unique values.', sep=''),
		ylab='unique levels'
	)
}
#print(histo_dat1_detrended)

#' plot the histogram of each column
for(acol in names(dat1_noid)){
	x <- dat1_noid[[acol]]
	h <- hist(
		x=x,
		main=paste0("Histogram of original data, col ", acol, sep=''),
		breaks=uniq_vals$histograms[[acol]]$breaks
	)
	#' and plot a normal density (if it was coming from that), from https://www.statmethods.net/graphs/density.html)
	xfit<-seq(min(x),max(x),length=40)
	yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
	yfit <- yfit*diff(h$mids[1:2])*length(x)
	lines(xfit, yfit, col="blue", lwd=2) 
}
for(acol in names(dat1_detrended)){
	x <- dat1_detrended[[acol]]
	h <- hist(
		x=x,
		main=paste0("Histogram of detrended data, col ", acol, sep=''),
		breaks=uniq_vals_detrended$histograms[[acol]]$breaks
	)
	#' and plot a normal density (if it was coming from that), from https://www.statmethods.net/graphs/density.html)
	xfit<-seq(min(x),max(x),length=40)
	yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
	yfit <- yfit*diff(h$mids[1:2])*length(x)
	lines(xfit, yfit, col="blue", lwd=2) 
}

#' Density plots:
for(acol in names(dat1_noid)){
	plot(
		density(dat1_noid[[acol]]),
		main=paste0("Density plot of original data, col ", acol, sep='')
	)
}
#' comments on the distribution of the generating process of the original data:
#' A : looks like a mixture of Gaussians (maybe 4)
#' B : a lognormal/weibull distrbution
#' C : a mixture of lognormal/weibull distributions or 1 lognormal/weibull and some normal distributions
#' D : a mixture of normal distributions
#' E : a lognormal/weibull (shape ~1)
#' F : a mixture of lognormal/weibull and 1 normal(?)

for(acol in names(dat1_detrended)){
	plot(
		density(dat1_detrended[[acol]]),
		main=paste0("Density plot of detrended data, col ", acol, sep='')
	)
}
#' comments on the distribution of the generating process of the original data:
#' A,B,C,F : mixture of normal distributions
#' D,E : pure normal distributions with very light tails, see below on fitting a distribution on them

#' Let's also do a q-q plot (quantile-quantile plot) comparing
#' with the Normal distribution.
#' A q-q plot is a plot of the quantiles of test data coming e.g. from the Normal distribution
#' against the corresponding quantiles of our data.
#' 'the data in the x% quantile' means the first x% of the data sorted in ascending order.
#' The more the plot coincides with the 45-degree line the stronger the assumption
#' that our data derives from specific distribution is.
#' Other distributions can be tested.
#' note: flat regions indicate agreemenet, curved regions disagreement
#' for example, flat in the middle but curved on the sides means that it
#' probably comes from a Normal distribution but has heavy tails, more
#' data at the tails than Normal.
library(ggpubr)
for(acol in names(dat1_noid)){
#	qqnorm(	y=dat1_noid[[acol]],
#		main=paste0("Q-Q plot of original data, col ", acol, sep='')
#	); qqline(dat1_noid[[acol]]);
	ggqqplot(
		dat1_noid[[acol]],
		main=paste0("Q-Q plot of original data, col ", acol, sep='')
	)
}
for(acol in names(dat1_detrended)){
#	qqnorm(	y=dat1_detrended[[acol]],
#		main=paste0("Q-Q plot of detrended data, col ", acol, sep='')
#	); qqline(dat1_detrended[[acol]])
	ggqqplot(
		dat1_detrended[[acol]],
		main=paste0("Q-Q plot of original data, col ", acol, sep='')
	)
}

#' One can also try to fit a distribution to our data
#' for example, in the detrended column D (which appears to have a pure normal distribution)
library(fitdistrplus)
acol='D'
x <- dat1_detrended[[acol]]
afit <- fitdistr(x, 'normal')
print(afit)
plot(
	density(x),
	main=paste0('Density plot of detrended data, col ', acol, ' superimposed with fitted Normal dist.', sep='')
)
lines(
	density(
		rnorm(
			n=50000,
			mean=afit$estimate['mean'],
			sd=afit$estimate['sd']
		)
	),
	col='red',
	pch=22
)
#' the q-q plot
qqplot(
	x=x,
	y=rlnorm(n=50000, afit$estimate),
	main=paste0('Q-Q plot of original data, col ', acol, sep='')
)
#' In this case (column D), we can see the peak of the data is much higher than that which comes from the fitted distribution

#' Here is column E (original) which appears as coming from lognormal distribution
acol='E'
x <- dat1_noid[[acol]]+0.02 #' must be positive!
afit <- fitdistr(x, 'lognormal')
print(afit)
#' and plot
plot(
	density(x),
	main=paste0('Density plot of original data, col ', acol, ' superimposed with fitted Normal dist.', sep='')
)
lines(density(rlnorm(n=50000, afit$estimate)), col='red', pch=22)
#' the q-q plot
qqplot(
	x=x,
	y=rlnorm(n=50000, afit$estimate),
	main=paste0('Q-Q plot of original data, col ', acol, sep='')
)
#' which is not bad except the tail is too heavy!

#' For those columns that I think derive from mixture processes we can use a mixture model estimator
library(mclust)
acol='C'
x <- dat1_detrended[[acol]]
afit <- densityMclust(x)
plot(
	afit,
	what="density",
	data=x,
	breaks=50,
	main=paste0('Fitting a mixture of Gaussian distributions to detrended data, col, ', acol, sep='')
)
#' and here is a q-q plot of original and fitted distribution
#' notice that drawing samples from mclust fit is handled by function
#' sample_from_mclust() in library lib/MIXTURES.R
qqplot(
	x,
	sample_from_mclust(mclustobj=afit, N=10000),
	main=paste0('Q-Q plot of 10,000 samples from the fitted distr. and detrended data, col, ', acol, sep='')
)

#' Finally do a multi-variate fit using mclust.
#' We used only 2 columns as it is better to visualise.
#' All variables could have been used.
acol1='A'; acol2='B'
dat2 = cbind(
	dat1_detrended[[acol1]],
	dat1_detrended[[acol2]]
)
afit <- densityMclust(dat2)
plot(
	afit,
	what = "density",
	type = "persp",
	main=paste0('Multi-variate mixture fit for detrended data, cols ', acol1, ',', acol2, '.', sep='')
)

#' A mixture model for detrended column C with 6 Gaussians
#' using package Rmixmod.
library(Rmixmod)
acol='C'
nGaussians=6
x <- data.frame(data=(dat1_detrended[[acol]] - min(dat1_detrended[[acol]]) + 0.02))
afit <- mixmodCluster(x, nbCluster=nGaussians)
summary(afit)
histCluster(
	x=afit["bestResult"],
	data=x,
	breaks=100,
	# CHECK THIS:
	main=c(paste0('Histogram of detrended data, col ', acol, ' fitted with ', nGaussians, ' Gaussians.', sep=''))
)

#' Here we estimate a mixture of *different* distribution families
#' For example Gaussian and Gamma for original data, column C
library(flexmix)
adat <- list()
acol='C'
adat[[acol]] = dat1_noid[[acol]]-min(dat1_noid[[acol]])+10
#' we will a mix of 3 Gaussians and 1 Gamma
mymodels <- list(
	FLXMRglm(family = "gaussian"),
	FLXMRglm(family = "gaussian"),
	FLXMRglm(family = "gaussian"),
	FLXMRglm(family = "gaussian")
)
#' use the data from column C (original) for k=6 components
st<- system.time(
	afit <- flexmix(
		C ~ 1,
		data=adat,
		k=6,
		model=mymodels
	)
)
cat("done, model estimated in ", st[2], " seconds.\n", sep='')
print(parameters(afit))
#' obtain additional information on the estimated model using refit()
arfit <- refit(afit)
plot(
	arfit,
	bycluster=F,
	main=paste0('Mixture Model Components for orig. data, col ', acol, sep='')
)

#' compare densities, sample from our estimated model using rflexmix() :
sam <- rflexmix(afit)
plot(
	density(adat[[1]]),
	main=paste0('Fitting a mixture of different distributions to detr. data, col ', acol, sep='')
)
lines(density(sam$y[[1]]), col='red', pch=22)
#' not a bad fit at all!
