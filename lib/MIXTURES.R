sample_from_mclust <- function(
	mclustobj=NULL, # mclust obj
	N=1 # number of samples
){
	# from https://stats.stackexchange.com/questions/70855/generating-random-variables-from-a-mixture-of-normal-distributions
	mixture_probs = mclustobj$parameters$pro
	gaussian_means = mclustobj$parameters$mean

	Ngaussians = length(mixture_probs)
	components <- sample(
		1:Ngaussians,
		prob=mixture_probs,
		size=N,
		replace=TRUE
	)
	sds <- rep(mclustobj$parameters$variance$sigmasq, Ngaussians)
	samples <- rnorm(
		n=N,
		mean=gaussian_means[components],
		sd=sds[components]
	)
	return(samples)
}
