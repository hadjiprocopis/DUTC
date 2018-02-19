# convert a vector to a time-stamped dataframe
# where column 'ds' is a Date (2017-12-31)
# and 'y' is the column with all the values
# if your vector does associate with dates then use the 
# 'dates' input parameter to specify a vector of dates (R Date obj)
# corresponding to each data point in the input vector.
# If your vector is not associated with any dates, then this function
# will create some for you (see comments below)
to_prophet_data_format <- function (
	x=NULL,
	dates=NULL
){
	library(xts)

	# create a date sequence from today to the past
	# to attach to it data point
	if( is.null(dates) ){
		# make our own dates for the input vector starting from
		# today and spanning back one day for every data-point
		# for large data, it may takes us back to the caves...
		dates <- rev(
			seq.Date(
				from=Sys.Date(),
				length.out=length(x),
				by=-1
			)
		)
	}
	# and here is the data as an 'xts' (timeseries) object
	adat <- xts(x, dates)

	# prophet expects a dataframe with cols 'ds' and 'y'
	adf <- data.frame(ds=index(adat), y=coredata(adat))
	return(adf)
}
# plots the heatmap of a matrix
ggplot_heatmap <- function(
	inp=NULL, # a matrix of input
	labels.font.size=6,
	labels.angle=45,
	labels.significant.digits=0,
	cells.palette=NULL,
	plot.title=NULL,
	plot.xlab=NULL,
	plot.ylab=NULL
){
	library(ggplot2)
	library(reshape2)
	library(RColorBrewer)

	if( is.null(cells.palette) ){
		cells.palette = colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
	}
	# melt the data
	amelted_data <- melt(inp, na.rm=TRUE)
	# heatmap plot from https://www.analytics-tuts.com/correlation-matrix-heatmap-r
	p <- ggplot(
	        data=amelted_data,
	        aes(x=Var1, y=Var2, fill=value, label= value)
	)+geom_tile()+
	scale_fill_gradientn(colours = cells.palette(100))+
	geom_text(
		aes(label=round(value, labels.significant.digits)),
		size=labels.font.size,
		angle=labels.angle
	)
	if( ! is.null(plot.title) ){ p <- p+ggtitle(plot.title) }
	if( ! is.null(plot.xlab) ){ p <- p+xlab(plot.xlab) }
	if( ! is.null(plot.ylab) ){ p <- p+ylabe(plot.ylab) }
	return(p)
}
# convert a list to a dataframe
list2dataframe <- function(
	L=NULL
){
	df <- do.call(cbind.data.frame, L)
	colnames(df) <- names(L)
	return(df)
}
