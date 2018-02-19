detrend_dataset <- function(
	inp=NULL,
	times=1
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	ret=vector("list", ncnames)
	names(ret) <- cnames
	for(acolname in cnames){
		ret[[acolname]] = diff(inp[[acolname]])
	}
	if( times > 1 ){
		return(detrend_dataset(inp=ret,times=times-1))
	} else {
		return(ret)
	}
}
