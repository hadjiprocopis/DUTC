# discretises a list of vectors
# for each column we get a list of (discretised and breaks)
discretise <- function(
	inp=NULL, # an input list with cols 'A', 'B' etc.
	levels=5, # number of discrete levels
	method='mltools'
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	ret=vector("list", ncnames)
	names(ret) <- cnames
	for(acolname in cnames){
		ret[[acolname]] = discretise_vector(
			x=inp[[acolname]],
			levels=levels,
			method=method
		)
	}
	return(ret)
}

# discretises a single vector
# returned is a list of (discretised and breaks)
discretise_vector <- function(
	x=NULL, # an input vector
	levels=5, # number of discrete levels
	method='mltools'
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	if( method == 'arules' ){
		library(arules)
		thecuts=arules::discretize(
			x=x,
			method="frequency",
			categories=levels,
			onlycuts = T
		)
		if( is.null(thecuts) ){
			cat(whoami, " : call to arules::discretize() has failed.\n")
			return(NULL)
		}
	} else if( method == 'mltools' ){
		library(mltools)
		library(stringr)
		dummy <- mltools::bin_data(
			x=x,
			bins=levels,
			binType='quantile',
			returnDT=TRUE
		)
		if( is.null(dummy) ){
			cat(whoami, " : call to mltools::bin_data() has failed.\n")
			return(NULL)
		}
		badR = levels(dummy$Bin) # these are text levels!!!!
		thecuts=c()
		for(alevel in badR){
			regres <- str_match(alevel,"[\\[\\(](.*?),\\s*(.*?)[\\]\\)]$")
			thecuts=append(thecuts, as.numeric(regres[2]))
		}
		thecuts=append(thecuts, as.numeric(regres[3]))
	} else {
		cat(whoami, " : method '", method, "' is not recognised.\n")
		return(NULL)
	}
	# extract unique breaks!
	unique_cuts = c()
	for(aval in thecuts){
		if( ! (aval %in% unique_cuts) ){
			unique_cuts=append(unique_cuts, aval)
		}
	}
	# NA's are introduced at the boundaries so extends a bit the breaks
	unique_cuts[1] = unique_cuts[1]-0.1
	unique_cuts[length(unique_cuts)] = unique_cuts[length(unique_cuts)]+0.1
	discre_data <- as.numeric(cut(x, breaks=unique_cuts))
	ret=list()
	ret[['discretised']] = discre_data
	ret[['breaks']] = unique_cuts
	ret[['frequencies']] = table(cut(x,unique_cuts, include.lowest=TRUE))
	return(ret)
}
histo <- function(
	inp=NULL, # input list
	unique_vals=NULL # optional unique_vals list
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	ret=vector("list", ncnames)
	names(ret) <- cnames
	for(acolname in cnames){
		ret[[acolname]] = histo_vector(
			x=inp[[acolname]],
			breaks=unique_vals[['histograms']][[acolname]]$breaks
		)
	}
	return(ret)
}
histo_vector <- function(
	x=NULL, # input vector
	breaks=NULL # optional breaks
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	ahist = NULL
	if( is.null(breaks) ){
		ahist <- hist(x=x, plot=F)
	} else {
		ahist <- hist(x=x,breaks=breaks, plot=F)
	}
	ncounts = length(ahist$counts)
	ret=matrix(ncol=2,nrow=ncounts)
	colnames(ret) <- c('value', 'count')
	for(i in 1:ncounts){
		ret[i, 'value'] = ahist$breaks[i]+(ahist$breaks[i+1]-ahist$breaks[i])/2
		ret[i, 'count'] = ahist$counts[i]
	}
	return(ret)
}

unique_values <- function(
	inp=NULL
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	unix=vector("list", ncnames)
	names(unix) <- cnames
	num_unix=matrix(nrow=1,ncol=ncnames)
	histos=vector("list", ncnames)
	colnames(num_unix) <- cnames
	for(acolname in cnames){
		unix[[acolname]] = unique(inp[[acolname]])
		num_unix[,acolname] = length(unix[[acolname]])
		histos[[acolname]] = hist(inp[[acolname]], breaks=unix[[acolname]], plot=F)
	}
	ret=list()
	ret[['unique_values']] = unix
	ret[['num_unique_values']] = num_unix
	ret[['histograms']] = histos
	return(ret)
}
unique_values_vector <- function(
	x=NULL
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	ret=list()
	ret[['unique_values']] = unique(x)
	ret[['num_unique_values']] = length(ret[['unique_values']])
	ret[['histograms']] = hist(x=x, breaks=ret[['unique_values']], plot=F)
	return(ret)
}
