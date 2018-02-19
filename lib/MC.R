markov_chain_analysis <- function(
	inp=NULL,
	levels=5,
	discretisation_method='mltools'
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	ret=vector("list", ncnames)
	names(ret) <- cnames
	for(acolname in cnames){
		ret[[acolname]] = markov_chain_analysis_vector(
			x=inp[[acolname]],
			levels=levels,
			discretisation_method=discretisation_method
		)
	}
	return(ret)
}
markov_chain_analysis_vector <- function(
	x=NULL,
	levels=5,
	discretisation_method='mltools'
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	discr <- discretise_vector(
		x=x,
		levels=levels,
		method=discretisation_method
	)
	if( is.null(discr) ){
		cat(whoami, " : call to discretise_vector() has failed.\n", sep='')
		return(NULL)
	}
	library(markovchain)
	hmc_fit <- markovchainFit(
		data=discr[['discretised']],
		method="mle",
		name="mle"
	)
	if( is.null(hmc_fit) ){
		cat(whoami, " : call to markovchainFit() has failed.\n", sep='')
		return(NULL)
	}	
	return(hmc_fit)
}
