#!/usr/bin/env Rscript

#'---
#'title: "PCA analysis"
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

#' PCA
#' In preparing this report I have followed the route of
#' http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(FactoMineR)
library(factoextra)
adf <- list2dataframe(dat1_detrended)
ncols=ncol(adf)
pcaobj <- PCA(
	adf,
	scale.unit=T, # unit variance, zero mean
	graph=F,
	ncp=ncols
)
print(get_eigenvalue(pcaobj))
#' it does not look like there are redundant variables perhaps with
#' the exception of the last component which only contributes 4%
#' to the total variance. If we had to reduce the dimensionality
#' of the original data I would recommend keeping the first 4 components.

#' visualisng the contribution of each component to the variance of the data:
fviz_eig(pcaobj, addlabels = TRUE, ylim = c(0, 50), main='Contribution of the PCs to detrended data')

#' A plot of how is each variable represented by each principal component:
library("corrplot")
fviz_cos2(pcaobj, choice = "var", axes = 1:2)
var <- get_pca_var(pcaobj)
corrplot(var$cos2, is.corr=FALSE)

#' visualisation: a plot of the first 2 components.
fviz_pca_ind(pcaobj)
#' variables with shorter vectors are less important for the first components
#' variables with aligned vectors are positively correlated.
fviz_pca_var(
	pcaobj, col.var = "contrib",
	gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
	repel = TRUE
)

#' contribution of the variables to the variance of the data (i.e. importance of the 'real' variables):
print(var$contrib)
corrplot(var$contrib, is.corr=FALSE)
#' Contributions of variables to PC1
fviz_contrib(pcaobj, choice = "var", axes = 1, top = 10)
#' Contributions of variables to PC2
fviz_contrib(pcaobj, choice = "var", axes = 2, top = 10)
