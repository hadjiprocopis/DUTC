seasonality_analysis <- function(
	inp=NULL, # a list like our dataset
	N=0 # optional, return only the N-most significant frequencies (power highest)
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	ret=vector("list", ncnames)
	names(ret) <- cnames
	for(acolname in cnames){
		ret[[acolname]] = seasonality_analysis_vector(x=inp[[acolname]])
	}
	return(ret)
}
fourier_transform_analysis <- function(
	inp=NULL, # a list like our dataset
	N=0 # optional, return only the N-most significant frequencies (power highest)
){
	whoami=paste0(match.call()[[1]],'()',collapse='')
	cnames = names(inp)
	ncnames = length(cnames)
	ret=vector("list", ncnames)
	names(ret) <- cnames
	for(acolname in cnames){
		ret[[acolname]] = fourier_transform_analysis_vector(x=inp[[acolname]])
	}
	return(ret)
}
seasonality_analysis_vector <- function(
	x=NULL,
	N=0 # optional, return only the N-most significant frequencies (power highest)
){
	library(TSA)
	p = periodogram(x, plot=F)
	# this returns a list o frequencies and their signal power
	# the output data structure
	# consists of POWER,log10(POWER), FREQUENCY,TIME
	adf = data.frame(power=p$spec, log10power=log10(p$spec), freq=p$freq, time=1/p$freq)

	# sort wrt to signal power ascending to get the top
	# frequecies for seasonality (if any)
	sorted = adf[order(adf$power, adf$time), ]
	if( N > 0 ){
		# get to N most powerful frequencies:
		# (remember sort ascending, so most power at the bottom)
		sorted = tail(sorted, N)
	}
	return(sorted)
}
fourier_transform_analysis_vector <- function(
	x=NULL,
	N=0, # optional, return only the N-most significant frequencies (power highest)
	Fs=1 # sampling rate of input vector - we assume is fixed and we know it, else 1
){
	library(stats)
	ff <- fft(z=x)
	L = length(ff) # the size of the FFT

	# for a real signal ignore all the frequencies at the end-half of the output
	ff = ff[1:(length(ff)/2)]

	# the power of the component is the modulus of the complex number (func Mod())

	# this is the frequency 'step' given sampling freq
	# and the size of the input
	Q = Fs / L
	# the output data structure
	# consists of POWER,FREQUENCY,TIME
	adf = data.frame(power=Mod(ff), log10power=log10(Mod(ff)), freq=seq.int(length(ff))*Q)
	adf$time = 1 / adf$freq

	# sort wrt to signal power ascending to get the top
	# frequecies for seasonality (if any)
	sorted = adf[order(adf$power), ]
	if( N > 0 ){
		# get to N most powerful frequencies:
		# (remember sort ascending, so most power at the bottom)
		sorted = tail(sorted, N)
	}
	return(sorted)
}
