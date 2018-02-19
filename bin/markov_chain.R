#!/usr/bin/env Rscript

source('lib/DATA.R');
source('lib/IO.R');
source('lib/TS.R');
source('lib/MC.R');

infile='cleaned_data/dat1.eliminateNA.csv'
outdir='markov_chain'; dir.create(outdir, showWarnings=F)

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

dat1_detrended <- detrend_dataset(inp=dat1_noid,times=1)
uniq_vals <- unique_values(inp=dat1_noid)
uniq_vals_detrended <- unique_values(inp=dat1_detrended)
histo_dat1 <- histo(inp=dat1_noid,unique_vals=uniq_vals)
histo_dat1_detrended <- histo(inp=dat1_detrended,unique_vals=uniq_vals_detrended)
dat1_discrete <- discretise(dat1_noid, levels=5)
dat1_detrended_discrete <- discretise(dat1_detrended, levels=5)

hmc_discrete <- markov_chain_analysis(dat1_noid)
for(acol in names(hmc_discrete)){
	png(file.path(outdir, paste0(acol, '_hmc_plot_discrete', '.png')))
	plot(hmc_discrete[[acol]]$estimate)
	dev.off()
}

hmc_detrended_discrete <- markov_chain_analysis(dat1_detrended)
for(acol in names(hmc_detrended_discrete)){
	png(file.path(outdir, paste0(acol, '_hmc_plot_detrended_discrete', '.png')))
	plot(hmc_detrended_discrete[[acol]]$estimate)
	dev.off()
}
