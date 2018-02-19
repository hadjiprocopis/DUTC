# breaks is a vector of length numbins+1, the first element is min(dat) and last element is max(dat)
# this is what hist and hist.su output ($breaks)
discretize_data <- function(breaks, dat){
	num_items = length(dat);
	ret=vector(mode='integer', length=num_items)
	num_breaks=length(breaks)
	avrg = vector(mode='double', length=num_breaks-1)
	for(i in 1:(num_breaks-1)){ avrg[i] = (breaks[i]+breaks[i+1])/2.0 }
	for(j in 1:num_items){
		for(i in 2:num_breaks){
			if( dat[j] < breaks[i] ){ ret[j] = avrg[i-1]; break }
		}
	}
	return(ret)
} 
# as above but instead of output the mean value of the bin which is the hist$mids,
# it ASSUMES mids are sorted!!! in ascending order
# it outputs the id of the bin, e.g. 1, 2, etc.
# labels start from 1
# breaks start from the values min value or less and end with max value or more (like output hist$breaks)
label_data <- function(breaksin, dat){
	num_items = length(dat);
	ret=vector(mode='integer', length=num_items)
	num_breaksin=length(breaksin)
	for(j in 1:num_items){
		for(i in 2:num_breaksin){
			if( dat[j] < breaksin[i] ){ ret[j] = i-1; break }
		}
	}
	return(ret)
}
# calculate bins(breaks) for vector optionally specify: numbins or breaks or nothing or equal height
calculate_histogram_breaks <- function(values, numbins=NA, equal_height_bins=FALSE, density_num_points=512, density_kernel='biweight', dens=NULL, use.ks=TRUE, calculate.error=FALSE){
	ret = NULL; den = dens; error = NA
	spn = diff(range(values, na.rm=T)); mini=min(values, na.rm=T); maxi=max(values, na.rm=T)
	if( is.na(numbins) ){
#		if( is.null(den) ){ den = density(values,n=density_num_points,na.rm=T,kernel=density_kernel) }; numbins = den$bw
		ah=hist(values,breaks="FD", plot=F); numbins=length(ah$mids)
	}
	if( equal_height_bins == FALSE ){
		ret = seq(from=mini, to=maxi, by=(spn/numbins)); ret[1] = 0.98*mini; ret[length(ret)] = 1.02*maxi
		return(list(breaks=ret,density=NULL,error=NA))
	} else if( equal_height_bins == "bins" ){
		ret=vector(mode='double',length=(numbins+1))
		len = length(values)
		sortvalues = sort(values)
		avecounts = as.integer(len/numbins+0.5) # counts per bin average
		j = 1; for(i in seq(1,len,avecounts)){ ret[j] = sortvalues[i]; j = j + 1 }
		ret[1] = 0.98*mini; ret[length(ret)] = 1.02*maxi
		return(list(breaks=ret,density=NULL,error=error))
	}
	write(paste("calculate_histogram_breaks : don't know what to do with this equal_height_bins value : '",equal_height_bins,"', can be FALSE, 'bin', 'density'. But 'density' is obsolete.", sep=""), stderr())
}
# values: a matrix of N-dimensional vectors 
# breaks : a list of N vectors of possibly different length, length=number of breaks
# each vector represents the breaks in a given dimension 1..N
# for one dimension, this works as:
# values = matrix(...)
# breaks = l(c(...))
# colprefix = is what to prefix the columns (1..N) holding the bin number. e.g. with default is label1, label2, ...labelN
# rangeprefix = is what to prefix the columns (1..N) holding the range for the corresponding bin number (label).
calculate_histogram <-function(values, breaks, colprefix="binlabel", rangeprefix="binrange"){
	ndims <- ncol(values); nrows <- nrow(values)
	nbreaks <- lapply(breaks, length)
	if( length(breaks) != ndims ){ write(paste("/home/andreas/bin/library_in_R.R::calculate_histogram : number of columns of matrix 'values' (",ndims,") must be the same as the number of vectors in the list 'breaks' (",length(breaks),"). Can't continue", sep=""), stderr()); return }
# this is the structure of the returned matrix, add new cols here but also change the dims of the matrix (ncol) below
# the matrix is created with one dummy row (removeme)
	counts <- matrix(ncol=1+1+ndims+ndims+ndims,dimnames=list(
		c('removeme'), # the row name
	# these are the col names:
		c('counts', 'probabilities',
			paste(colprefix,seq(1,ndims,1),sep=''), # the histogram labels (i.e. bin number) for each input col
			paste(paste(rangeprefix,"_from",sep=''), seq(1,ndims,1),sep=''), # the FROM range for the bin (from:to) for each input col
			paste(paste(rangeprefix,"_to",sep=''), seq(1,ndims,1),sep='') # the FROM range for the bin (from:to) for each input col
		)
	)) # first is a string of indices - one to each dimension - to the bin, second is the count, e.g. "1,2,3" 5 means box i=1,j=2,k=3 (i,j,k=dimensions) count=5
	# we will label each dimension to its own breaks, then use the labels as index to array of counts
	labels <- list()
	for(i in 1:ndims){ labels[[i]] <- label_data(breaksin=breaks[[i]], dat=unlist(values[i])) }
	ind = vector(mode='integer', length=ndims)
	ranges_from = vector(mode='numeric', length=ndims)
	ranges_to = vector(mode='numeric', length=ndims)
	strind = "" ; num_adds=1
	for(r in 1:nrows){
		for(i in 1:ndims){
			ind[i] = labels[[i]][r]
			ranges_from[i] = breaks[[i]][ind[i]]
			ranges_to[i] = breaks[[i]][ind[i]+1]
		}
		strind = paste(ind, collapse=",", sep="")
		if( strind %in% rownames(counts) ){
			counts[strind,'counts'] = counts[strind,'counts'] + 1
		} else {
			num_adds = num_adds + 1
			counts=rbind(
				counts, c(
				# these are the default values (when creating a new empty row) for each col of output, length is important, if you add more columns (see above) you need to add more things here
					1, # counts
					NA, # probabilities
					ind, # labels
					ranges_from, # ranges from
					ranges_to # ranges to
				)
			)
			rownames(counts)[num_adds] <- strind
		}
	}
	counts[,'probabilities'] = counts[,'counts'] / sum(counts[,'counts'], na.rm=T)
	return(counts[-1,])
	# the result is a sparse matrix, i.e. those bins with zero count are not included!!!
}
# the returned histogram from calculate_histogram does not include zero-count bins - this functions corrects this if you need it.
# a_histogram : a matrix representing a histogram previously calculated using calculate_histogram function
# numbins : a vector of bin-numbers per input dimension of the histogram.
unsparse_histogram <- function(a_histogram, numbins){
	ret <- a_histogram
	labels = list()
	for(i in 1:length(numbins)){ labels[[i]] = seq(1,numbins[i],1) }
	all_combinations = expand.grid(labels)
	extra_columns = ncol(a_histogram)-2-length(numbins)
	rnames = rownames(a_histogram); num_adds=length(rnames)+1
	for(arow in 1:nrow(all_combinations)){
		comb_label = paste(all_combinations[arow,], collapse=',')
		if( !(comb_label %in% rnames) ){
			if( extra_columns > 0 ){
				ret = rbind(ret, c(0,0.0,unlist(all_combinations[arow,]),rep(x=NA,times=extra_columns)))
			} else { 
				ret = rbind(ret, c(0,0.0,unlist(all_combinations[arow,])))
			}
			rownames(ret)[num_adds] <- comb_label
			num_adds = num_adds + 1
		}
	}
	return(ret)
}

# Entropy of a set of values in a vector representing probabilities (i.e. sum=1, >=0). will be ignored if prob=0
# this stupid R returns a list() whenever all elements are 0 and sum() fails.
#simple_entropy <- function(a_vec){ sum(sapply(a_vec[a_vec>0], function(p){-p*log10(p)}), na.rm=T) }
# so use the for-loop, it looks same time...
simple_entropy <- function(a_vec){ sum=0; for(i in a_vec[a_vec>0]){ sum = sum - i*log10(i) }; return(sum) }

plot_heatmap <- function(
	data, # the dataframe with 3 entries: x, y and values
	gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2), # grid color and alpha
	gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),
	background=theme_blank(),
	cell.color=alpha('black',0.25), # the border around each cell
	fontsize=20,
	axis.label.x="x",
	axis.label.y="y",
	axis.limits.x=NULL,
	axis.limits.y=NULL,
	axis.labels.x=data.frame(breaks=NULL,labels=NULL),
	axis.labels.y=data.frame(breaks=NULL,labels=NULL),
	box.labels.bgcol="green", # how to draw the boxes
	box.labels.bgfill="skyblue",
	box.labels.bgalpha=0.8,
	box.labels.fontsize=3.5, # fontsize of the box
#	box.labels.palette.start=alpha('cornflowerblue',0.2), # the 3 colors for the gradient of the boxes
#	box.labels.palette.middle=alpha('white',0.2),
#	box.labels.palette.end=alpha('red',0.2),
	box.labels.palette.start='blue', # the 3 colors for the gradient of the boxes
	box.labels.palette.middle='white',
	box.labels.palette.end='red',
	box.labels.limits=c(-1,1), # max and min of the legend values
	box.labels.midpoint=0, # the midpoint of the range where the mid color will be mapped
	box.labels.breaks=NULL, # a c(1,2,3) representing the legend values (i.e. here 1, 2 and 3), default is auto
	title=NULL,
	legend.fontsize=12,
	legend.direction="horizontal", # horizontal or vertical legend (the gradient) to the colors
	legend.title=NULL
){
	suppressMessages(require(scales))
	suppressMessages(require(ggplot2))
	p <-ggplot()
	if( is.null(cell.color) ){ p <- p + geom_rect(data=data,aes(xmin=x,xmax=x+1,ymin=y,ymax=y+1,fill=values)) } else { p <- p + geom_rect(data=data,aes(xmin=x,xmax=x+1,ymin=y,ymax=y+1,fill=values),color=cell.color) }
	if( is.null(axis.labels.x) ){
		# don't show x-axis or its labels or tickmarks (show x-axis title)
		# unfortunately hiding tickmarks is for both X,Y axes
		p <- p + opts(axis.text.x=theme_blank(),axis.ticks=theme_blank())+scale_x_continuous(breaks=NULL)
	} else {
		p <- p + opts(axis.text.x=theme_text(size=fontsize*0.8))+scale_x_continuous(breaks=axis.labels.x$breaks,labels=axis.labels.x$labels)
	}
	if( is.null(axis.labels.y) ){
		# don't show y-axis or its labels or tickmarks (show y-axis title)
		p <- p + opts(axis.text.y=theme_blank(),axis.ticks=theme_blank())+scale_y_continuous(breaks=NULL)
	} else {
		p <- p + opts(axis.text.y=theme_text(size=fontsize*0.8))+scale_y_continuous(breaks=axis.labels.y$breaks,labels=axis.labels.y$labels)
	}
	p <- p + opts(panel.background=background,panel.grid.major=gridline.major,panel.grid.minor=gridline.minor)+
	xlab(axis.label.x)+ylab(axis.label.y)+
	opts(axis.title.x=theme_text(size=fontsize),axis.title.y=theme_text(size=fontsize,angle=90,hjust=0.5)) # magic numbers

	p <- p + coord_cartesian(ylim=axis.limits.y,xlim=axis.limits.x)+
#	scale_fill_continuous(name=legend.title)+scale_alpha_continuous(name=legend.title)+
	scale_alpha_continuous(guide=FALSE)+
#	scale_fill_gradient2(name=legend.title,limits=box.labels.limits,low=box.labels.palette.start,mid=box.labels.palette.middle,high=box.labels.palette.end,breaks=box.labels.breaks)+
	scale_fill_gradient2(name=legend.title,midpoint=box.labels.midpoint,low=box.labels.palette.start,mid=box.labels.palette.middle,high=box.labels.palette.end,breaks=box.labels.breaks)+
#c(rgb(colorRamp(c(box.labels.palette.start,box.labels.palette.end))(seq(0,1,length=box.labels.palette.num_shades)), maxColorValue=255)))+
	opts(title=title,legend.direction=legend.direction)
	if( box.labels.fontsize > 0.0 ){
		p <- p + geom_text(data=data,aes(x=x+0.5,y=y+0.5,label=sprintf("%.2f\n%s",values, extra_values)),size=box.labels.fontsize,bgcol=box.labels.bgcol,bgalpha=box.labels.bgalpha)
	}
	return(p);
}

# plot multiple y-values with a common x-value each as pair as a point, e.g. x=1, y1=1,y2=2, or just x=1,y=1, etc.
plot_points <- function(
	data, # a list of dataframes each with a x and y
	gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2), # grid color and alpha
	gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),
	class.labels=NULL, # data ontains 'class' col which may contain string labels or integers
			   # if it's integers, then supply this vector of labels to be placed in the legend
			   # if class.labels is not null, it overwrites the class cols
	class.different_colors=TRUE, # FALSE (for no different colors), TRUE (for different colors, automatic setup) or a vector of colors for manual setup
	class.different_shapes=FALSE, # specify if each class is drawn with different shape
	points.size=0.5,
	points.alpha=1.0,
	background=theme_blank(),
	point.color=c(alpha('black',0.25)), # the color of each point
	fontsize=20,
	axis.label.x="x",
	axis.label.y="y",
	axis.limits.x=NULL,
	axis.limits.y=NULL,
	axis.labels.x=data.frame(breaks=NULL,labels=NULL),
	axis.labels.y=data.frame(breaks=NULL,labels=NULL),
	title=NULL,
	legend.title=NULL,
	legend.show=TRUE
){
	suppressMessages(require(scales))
	suppressMessages(require(ggplot2))

	num_classes <- length(data)
	if( is.null(class.labels) ){ all_labels <- 1:num_classes } else { all_labels <- class.labels }
	str = "a_list<-list("; for(i in 1:num_classes){ str = paste(str,"df",all_labels[i],"=data[[",i,"]],",sep='') }; str=substr(str, 1, nchar(str)-1); str=paste(str,")",sep='')
	eval(parse(text=str))
	newData = melt(a_list, id.vars='x')
	p <- ggplot()+
	geom_line(data=newData,aes(x=x,y=value,color=L1),size=points.size,alpha=points.alpha)+
	opts(panel.grid.major=gridline.major,panel.grid.minor=gridline.minor,panel.background=background)+
	opts(axis.text.x=theme_text(size=fontsize*0.8)) + # these are magic numbers for just and pointsize
	opts(axis.text.y=theme_text(size=fontsize*0.8,hjust=1))+
	opts(axis.title.x=theme_text(size=fontsize))+
	opts(axis.title.y=theme_text(size=fontsize,angle=90,hjust=0.5))+
	coord_cartesian(ylim=axis.limits.y,xlim=axis.limits.x)+
	xlab(axis.label.x)+scale_x_continuous(breaks=axis.labels.x$breaks,labels=axis.labels.x$labels)+
	ylab(axis.label.y)+scale_y_continuous(breaks=axis.labels.y$breaks,labels=axis.labels.y$labels)+
	opts(title=title)
	if( !is.null(class.different_colors) ){
		if( legend.show == T ){
			p <- p + scale_color_manual(breaks=paste("df",all_labels,sep=''),labels=all_labels,values=class.different_colors,name=legend.title)
		} else {
			p <- p + scale_color_manual(breaks=paste("df",all_labels,sep=''),labels=all_labels,values=class.different_colors,name=legend.title,guide=FALSE)
		}
	} else {
		if( legend.show == T ){
			p <- p + scale_color_manual()
		} else { 
			p <- p + scale_color_manual(guide=FALSE)
		}
		# it is really a big mistake to use R. really big mistake, but it is convenient for 5 mins
		# for longer term, you will cut your wrists
	}
	return(p);
}
# like histogram bars
plot_boxes <- function(
	data.boxes, # the dataframe with 3 entries: breaksFrom,breaksTo, counts (e.g. (0,1,2),(1,2,3) and 5,7,8: between 0,1->5, 1,2->7, 2,3->8)
	data.lines=NULL, # optional other data to be plotted, this is a list of lists, each list contains: a dataframe, ($data) of (x,y) pairs
			# optional colour name ($color), the legend name is taken from the list's $, e.g. data.lines$fuck, data.lines$fick
	gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2),
	gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),
	background=theme_blank(),
	box.color.outline=alpha('black',0.75),
	box.color.fill=alpha('#D55E00',0.75),
	fontsize=20,
	axis.label.x="x",
	axis.label.y="y",
	axis.limits.x=NULL,
	axis.limits.y=NULL,
	axis.labels.x=data.frame(breaks=NULL,labels=NULL),
	axis.labels.y=data.frame(breaks=NULL,labels=NULL),
	box.height_labels.show=TRUE,
	box.height_labels.fontsize_multiplier=0.25, # multiply fontsize by this parameter to get fontsize of height labels, may be you want them smaller
	title=NULL,
	legend.title=NULL
){
	suppressMessages(require(scales))
	suppressMessages(require(ggplot2))
	p <- ggplot(data=data.boxes)
	if( box.height_labels.show == TRUE ){
		# superhack: aes can not read variables from environment so do that to add mxc
		p <- p + geom_text(eval(parse(text=paste('aes(x=(breaksFrom+breaksTo)/2,y=counts+',(0.1*max(data.boxes$counts)),',label=counts)', sep=''))),size=box.height_labels.fontsize_multiplier*fontsize)
	}
	# this is totally stupid but...
	if( is.null(box.color.outline) ){
		if( is.null(box.color.fill) ){
			p <- p +geom_rect(aes(xmin=breaksFrom,xmax=breaksTo,ymin=0,ymax=counts))
		} else { 
			p <- p +geom_rect(aes(xmin=breaksFrom,xmax=breaksTo,ymin=0,ymax=counts),fill=box.color.fill)
		}
	} else { 
		if( is.null(box.color.fill) ){
			p <- p +geom_rect(aes(xmin=breaksFrom,xmax=breaksTo,ymin=0,ymax=counts),colour=box.color.outline)
		} else { 
			p <- p +geom_rect(aes(xmin=breaksFrom,xmax=breaksTo,ymin=0,ymax=counts),colour=box.color.outline,fill=box.color.fill)
		}
	}
	p <- p + opts(panel.background=background,panel.grid.major=gridline.major,panel.grid.minor=gridline.minor)+
	xlab(axis.label.x)+ylab(axis.label.y)+
	opts(axis.title.x=theme_text(size=fontsize),axis.title.y=theme_text(size=fontsize,angle=90,hjust=0.5)) # magic numbers

	if( is.null(axis.labels.x) ){
		# don't show x-axis or its labels or tickmarks (show x-axis title)
		# unfortunately hiding tickmarks is for both X,Y axes
		p <- p + opts(axis.text.x=theme_blank(),axis.ticks=theme_blank())+scale_x_continuous(breaks=NULL)
	} else {
		p <- p + opts(axis.text.x=theme_text(size=fontsize*0.8))+scale_x_continuous(breaks=axis.labels.x$breaks,labels=axis.labels.x$labels)
	}
	if( is.null(axis.labels.y) ){
		# don't show y-axis or its labels or tickmarks (show y-axis title)
		p <- p + opts(axis.text.y=theme_blank(),axis.ticks=theme_blank())+scale_y_continuous(breaks=NULL)
	} else {
		p <- p + opts(axis.text.y=theme_text(size=fontsize*0.8))+scale_y_continuous(breaks=axis.labels.y$breaks,labels=axis.labels.y$labels)
	}

	p <- p + coord_cartesian(ylim=axis.limits.y,xlim=axis.limits.x)+
	scale_fill_continuous(name=legend.title)+
	scale_alpha_continuous(guide=FALSE)+
	opts(title=title)

	if( !is.null(data.lines) ){
		rnames = names(data.lines)
		for(a_line_name in names(data.lines)){
			if( 'color' %in% names(a_line) ){
				p <- p +
				geom_line(data=a_line$data,aes(x=x,y=y),color=a_line$color)
			} else {
				p <- p +
				geom_line(data=a_line$data,aes(x=x,y=y))+scale_colour_brewer("clarity")
			}
		}
	}
	return(p)
}

plot_scatterplot <- function(
	data, # the dataframe with 3 entries: x, y and class, class says which class the (x,y) pair belongs to for annotation
	gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2), # grid color and alpha
	gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),
	class.labels=NULL, # data ontains 'class' col which may contain string labels or integers
			   # if it's integers, then supply this vector of labels to be placed in the legend
			   # if class.labels is not null, it overwrites the class cols
	class.colors.manual=NULL, # a manual color scale for each class in the data, NULL will calculate this scale itself using the min and max colors below
	class.colors.scale.begin='red',
	class.colors.scale.end='green',
	class.different_colors=TRUE, # FALSE (for no different colors), TRUE (for different colors, automatic setup) or a vector of colors for manual setup
	class.different_shapes=FALSE, # specify if each class is drawn with different shape
	points.size=2,
	points.alpha=0.75,
	background=theme_blank(),
	fontsize=20,
	axis.label.x="x",
	axis.label.y="y",
	axis.limits.x=NULL,
	axis.limits.y=NULL,
	axis.labels.x=data.frame(breaks=NULL,labels=NULL),
	axis.labels.y=data.frame(breaks=NULL,labels=NULL),
	title=NULL,
	legend.title=NULL,
	legend.fontsize=12,
	legend.show=TRUE,
	correlation.method="", # calculate and print a correlation coefficient
	correlation.test.method=correlation.method,
	add.bestfit.line=FALSE, # add best-fit lines and curves to show the trend
	add.bestfit.curve=FALSE, # add best-fit lines and curves to show the trend
	render=TRUE
){
	suppressMessages(require(scales))
	suppressMessages(require(ggplot2))
	all_classes = unique(as.vector(data$class)); num_classes = length(all_classes)
	if( is.null(class.labels) ){ all_labels = all_classes } else { all_labels = class.labels }
	if( correlation.method != "" ){
		corres = list(length=num_classes)
		for(i in 1:num_classes){
			aclass=all_classes[i]
			corres[i] = list(calculate_correlation(data$x[data$class==aclass], data$y[data$class==aclass], correlation.method=correlation.method, correlation.test.method=correlation.test.method))
			all_labels[i] = paste(all_labels[i], ", R: ", corres[[i]]$correlation, ", p-value:", corres[[i]]$p.value, sep="")
		}
	}

	if( class.different_colors == TRUE ){
		if( class.different_shapes == FALSE ){ p <- ggplot(data=data,aes(x=x,y=y,color=class)) }
		else { p <- ggplot(data=data,aes(x=x,y=y,color=class,shape=class)) }
	} else {
		if( class.different_shapes == FALSE ){ p <- ggplot(data=data,aes(x=x,y=y)) }
		else { p <- ggplot(data=data,aes(x=x,y=y,shape=class)) }
	}			
	if( class.different_colors == TRUE ){
		if( class.different_shapes == FALSE ){ p <- p + geom_point(size=points.size,alpha=points.alpha)+scale_shape_manual(guide=FALSE) }
		else { p <- p + geom_point(size=points.size,alpha=points.alpha)+scale_shape_discrete(breaks=c(1:num_classes),labels=all_labels,name=legend.title) }
		if( is.null(class.colors.manual) ){
#			p <- p + scale_color_discrete(breaks=c(1:num_classes),labels=all_labels,name=legend.title,low=class.colors.scale.begin,high=class.colors.scale.end)
			p <- p + scale_color_discrete(breaks=c(1:num_classes),labels=all_labels,name=legend.title)
		} else {
			if( legend.show == TRUE ){
				p <- p + scale_color_manual(breaks=c(1:num_classes),labels=all_labels,values=class.colors.manual)
			} else {
				p <- p + scale_color_manual(breaks=c(1:num_classes),labels=all_labels,values=class.colors.manual,guide=FALSE)
			}
		}
	} else {
		if( class.different_shapes == FALSE ){ p <- p + geom_point(size=points.size,alpha=points.alpha)+scale_shape_manual(guide=FALSE) }
		else {
			p <- p + geom_point(size=points.size,alpha=points.alpha)+scale_shape_discrete(breaks=c(1:num_classes),labels=all_labels,name=legend.title)
		}
	}
	p <- p +
	opts(panel.grid.major=gridline.major,panel.grid.minor=gridline.minor,panel.background=background)+
	opts(axis.text.x=theme_text(size=fontsize*0.8)) + # these are magic numbers for just and pointsize
	opts(axis.text.y=theme_text(size=fontsize*0.8,hjust=1))+
	opts(axis.title.x=theme_text(size=fontsize))+
	opts(axis.title.y=theme_text(size=fontsize,angle=90,hjust=0.5))+
	coord_cartesian(ylim=axis.limits.y,xlim=axis.limits.x)+
	xlab(axis.label.x)+scale_x_continuous(breaks=axis.labels.x$breaks,labels=axis.labels.x$labels)+
	ylab(axis.label.y)+scale_y_continuous(breaks=axis.labels.y$breaks,labels=axis.labels.y$labels)+
	scale_alpha_continuous(guide=FALSE)+opts(legend.position="bottom",legend.text=theme_text(size=legend.fontsize,hjust=0),title=title)
	if( (add.bestfit.line == TRUE) && (add.bestfit.curve == FALSE) ){
		p <- p + geom_smooth(data=data,aes(x=x,y=y,color=class),method=lm,alpha=0.35,fill=alpha('#D55E00',0.105))
	} else if( (add.bestfit.line == FALSE) && (add.bestfit.curve == TRUE) ){
		p <- p + geom_smooth(data=data,aes(x=x,y=y,color=class),alpha=0.35,fill=alpha('#D55E00',0.105))
	} else if( (add.bestfit.line == TRUE) && (add.bestfit.curve == TRUE) ){
		p <- p + geom_smooth(data=data,aes(x=x,y=y,color=class),method=lm,alpha=0.35,fill=alpha('#D55E00',0.105),se=T)+
			 geom_smooth(data=data,aes(x=x,y=y,color=class),alpha=0.35,fill=alpha('#D55E00',0.105),se=F)
	}
#	legend <- p + opts(keep="legend_box",title=title,legend.text=theme_text(size=legend.fontsize,hjust=0),legend.justification="center")
#	p <- p + opts(legend.position = "none") # remove the legend and put it underneath
	return(p)
}

# plot multiple y-values with a common x-value each as pair as a point, e.g. x=1, y1=1,y2=2, or just x=1,y=1, etc.
plot_scatterplot_and_density_of_many_groups <- function(
	data, # dataframe contains x, y and class - class could be filename and x:col1, y:col2
	class.labels=NULL, # data contains 'class' col which may contain string labels or integers
			   # if it's integers, then supply this vector of labels to be placed in the legend
			   # if class.labels is not null, it overwrites the class cols
	scatterplot.background=theme_blank(),
	scatterplot.gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2), # grid color and alpha
	scatterplot.gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),
	density_top.background=theme_blank(),
	density_top.gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2), # grid color and alpha
	density_top.gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),
	density_right.background=theme_blank(),
	density_right.gridline.major=theme_line(colour=alpha('blue',0.15),size=0.2), # grid color and alpha
	density_right.gridline.minor=theme_line(colour=alpha('purple',0.05),size=0.2),

	class.different_colors=TRUE, # FALSE (for no different colors), TRUE (for different colors, automatic setup) or a vector of colors for manual setup
	class.different_shapes=FALSE, # specify if each class is drawn with different shape
	class.colors.scale.begin='red',
	class.colors.scale.end='green',
	scatterplot.points.size=1.5,
	scatterplot.points.alpha=0.35,
	density_right.points.size=0.5,
	density_right.points.alpha=1.0,
	density_top.points.size=0.5,
	density_top.points.alpha=1.0,
	fontsize=20,
	axis.label.x="x",
	axis.label.y="y",
	axis.limits.x=NULL,
	axis.limits.y=NULL,
	axis.labels.x=data.frame(breaks=NULL,labels=NULL),
	axis.labels.y=data.frame(breaks=NULL,labels=NULL),
	title=NULL,
	legend.title=NULL,
	legend.fontsize=12,
	legend.show=TRUE,
	correlation.method="", # calculate and print a correlation coefficient
	correlation.test.method=correlation.method,
	add.bestfit.line=FALSE, # add best-fit lines and curves to show the trend
	add.bestfit.curve=FALSE, # add best-fit lines and curves to show the trend
	render=TRUE
){
	suppressMessages(require(scales))
	suppressMessages(require(ggplot2))
	p <- plot_scatterplot(
		data=data,
		axis.label.x=axis.label.x,
		axis.label.y=axis.label.y,
		axis.labels.x=axis.labels.x,
		axis.labels.y=axis.labels.y,
		axis.limits.x=axis.limits.x,
		axis.limits.y=axis.limits.y,
		class.labels=class.labels,

		class.colors.scale.begin=class.colors.scale.begin,
		class.colors.scale.end=class.colors.scale.end,
		class.different_colors=class.different_colors,

		class.different_shapes=class.different_shapes,
		background=scatterplot.background,gridline.major=scatterplot.gridline.major,gridline.minor=scatterplot.gridline.minor,
		points.size=scatterplot.points.size,points.alpha=scatterplot.points.alpha,
		fontsize=fontsize,

		correlation.method=correlation.method,
		correlation.test.method=correlation.method,
		add.bestfit.line=add.bestfit.line, # add best-fit lines and curves to show the trend
		add.bestfit.curve=add.bestfit.curve, # add best-fit lines and curves to show the trend
	)
	legend <- p + opts(keep="legend_box",title=title,legend.text=theme_text(size=legend.fontsize,hjust=0))

	p1 <- p + opts(legend.position = "none") # remove legend
	suppressMessages(library(KernSmooth))
	dat <- alldata[alldata[,'class']==1,'x']; rang <- range(dat); denF1x <- bkde(dat, range.x=rang)
	dat <- alldata[alldata[,'class']==1,'y']; rang <- range(dat); denF1y <- bkde(dat, range.x=rang);
	if( 2 %in% alldata[,'class'] ){
		dat <- alldata[alldata[,'class']==2,'x']; rang <- range(dat); denF2x <- bkde(dat, range.x=rang)
		dat <- alldata[alldata[,'class']==2,'y']; rang <- range(dat); denF2y <- bkde(dat, range.x=rang);
		m=max(denF1x$y,denF2x$y); s=seq(0,m,length=4); density_top.ylabels = data.frame(breaks=s,labels=c(0,format(s[-1],scientific=T,digits=2)))
		m=max(denF1y$y,denF2y$y); s=seq(0,m,length=3); density_right.ylabels = data.frame(breaks=s,labels=c(0,format(s[-1],scientific=T,digits=2)))
		l1 <- list(data.frame(x=denF1x$x, y=denF1x$y), data.frame(x=denF2x$x,y=denF2x$y))
		l2 <- list(data.frame(x=denF1y$x, y=denF1y$y), data.frame(x=denF2y$x,y=denF2y$y))
	} else {
		denF2x <- NULL; denF2y <- NULL
		m=max(denF1x$y); s=seq(0,m,length=4); density_top.ylabels = data.frame(breaks=s,labels=c(0,format(s[-1],scientific=T,digits=2)))
		m=max(denF1y$y); s=seq(0,m,length=3); density_right.ylabels = data.frame(breaks=s,labels=c(0,format(s[-1],scientific=T,digits=2)))
		l1 <- list(data.frame(x=denF1x$x, y=denF1x$y))
		l2 <- list(data.frame(x=denF1y$x, y=denF1y$y))
	}
	p2 <- plot_points(
		data=l1,
		legend.show=FALSE,
		axis.label.x='',
		axis.label.y='',
		axis.labels.x=axis.labels.x,
		axis.labels.y=density_top.ylabels,
		axis.limits.x=axis.limits.x,
		class.different_colors=c('red','green'),
		background=density_top.background,gridline.major=density_top.gridline.major,gridline.minor=density_top.gridline.minor,
		points.size=density_top.points.size,points.alpha=density_top.points.alpha,
		fontsize=fontsize
	)
	p3 <- plot_points(
		data=l2,
		legend.show=FALSE,
		axis.label.x='',
		axis.label.y='',
		axis.labels.x=axis.labels.x,
		axis.labels.y=density_right.ylabels,
		axis.limits.x=axis.limits.x,
		class.different_colors=c('red','green'),
		background=density_right.background,gridline.major=density_right.gridline.major,gridline.minor=density_right.gridline.minor,
		points.size=density_right.points.size,points.alpha=density_right.points.alpha,
		fontsize=fontsize
	); p3 <- p3 + coord_flip()
	if( render == TRUE ){
		if( is.null(title) ){ 
			multiplot(ncol=2,nrow=3,widths=c(7,3),heights=c(3,7,2.5),list(p1,2,1), list(p2,1,1), list(p3,2,2), legend=list(legend,3,1:2))
		} else {
			multiplot(ncol=2,nrow=4,widths=c(7,3),heights=c(1.5,3,7,2.5),legend=list(legend,4,1),list(p1,3,1), list(p2,2,1), list(p3,3,2))
			grid.text(title, vp=viewport(layout.pos.row=1,layout.pos.col=1:2),just=c('centre','top'))
		}
	}
	return(list(scatterplot=p1,density_top=p2,density_right=p3,legend=legend))
}
multiplot <- function(
	ncol,	# number of columns
	nrow,	# number of rows
	widths=NULL,	# widths as a vector of widths for each column, e.g. c(1,2)
	heights=NULL,# hights for each row e.g. c(1,2)
	legend=NULL, # a list (like the ones below) which contains a legend element (can extract from a ggplot like: legend <- p + opts(keep = "legend_box",title='title')
	... # one or more lists: e.g. list(p1,1,2),list(p2,1,3) **** first is ROW, second is COL *** each with the ggplot object and x,y coords to be placed on the (nrow,ncol) matrix
) {
	# plot a scatter plot or heatmap with marginal histograms/densities on bottom, right etc. leave null for not having one
	source('/home/andreas/bin/library_align.R')
	suppressMessages(library(grid))
	if( is.null(widths) ){ widths = rep(1,times=ncol) }
	if( is.null(heights) ){ heights = rep(1,times=nrow) }
	grid_layout <- grid.layout(nrow=nrow, ncol=ncol, widths=widths, heights=heights)
	grid.newpage()
	pushViewport( viewport( layout=grid_layout, width=1, height=1 ) )
	align.plots(grid_layout, ...)
	if( !is.null(legend) ){ print(legend[[1]], vp=viewport(layout.pos.row=legend[[2]],layout.pos.col=legend[[3]])) }
}

# a ggplot plot title formatter use it as:
# opts(title=title, plot.title=format_title(width=unit(20, "cm")))
# it produces the formatting basically
ggplot_string_formatter <- function (width=unit(1, "npc"), family = "", face = "plain", colour ="black", size = 10,hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1.1)
{
	vj <- vjust
	hj <- hjust
	structure(function(label, x = hj, y = vj, ..., vjust = vj,
	hjust = hj, default.units = "npc") {
		textGrob(splitString(label, width), x, y, hjust = hjust, vjust= vjust, ...,default.units = default.units, gp = gpar(fontsize = size,col = colour, fontfamily = family, fontface = face,lineheight = lineheight), rot = angle)
	}, class = "theme", type = "title", call = match.call())
}

# will calculate correlation (pearson or spearman), do a test to get a p-value for this correlation and
# best-fit a line to the data (will return intercept and slope)
calculate_correlation <- function(
	x,
	y,
	correlation.method="pearson",
	correlation.test.method="pearson"
){
	# correlation value
	corellis = cor(x, y, method=correlation.method)
	# test of significance
	signi = cor.test(x, y, method=correlation.test.method)

	# fit a line to the two data
	corelifit = lm(x~y)
	return(list(
		correlation=corellis,
		p.value=signi$p.value,
		lm.intercept=corelifit$coefficients[1],
		lm.slope=corelifit$coefficients[2]
	))
	# the line can be plot using abline(coef=c(intercept,slope)) etc.
}

#gridplot32 <- function(title="", p1,p2,p3,legend=NULL){
#	Layout <- gridplot32.vplayout(title=title)
#	print(p1, vp=gridplot32.subplot(2,1))
#	print(p2, vp=gridplot32.subplot(1,1))
#	print(p3, vp=gridplot32.subplot(2,2))
#	if( !is.null(legend) ){ print(legend, vp=subplot(1,2)) }
#}
#gridplot32.subplot <- function(x,y){ viewport(layout.pos.row=x, layout.pos.col=y) }
#gridplot32.vplayout <- function(
#	title=""
#){
#	nr=3; if( title=="" ){ nr=nr-1 }
#	lay<-grid.layout(nrow=nr,ncol=2,widths=unit(c(2,1,3),c("null","null","null")),heights=unit(c(1,2,0.5),c("null","null","null")))
#	grid.newpage(); pushViewport(viewport(layout=lay))
#	if( nr == 3 ){ grid.text(title, vp = viewport(layout.pos.row = nr, layout.pos.col = 1:2)) }
#	return(lay)
#}
##	geom_text2(data=data,aes(x=x+0.5,y=y+0.5,label=sprintf("%.2f",values)),size=box.labels.fontsize, expand=box.labels.expand,bgcol=box.labels.bgcol,bgalpha=box.labels.bgalpha)+
## from http://stackoverflow.com/questions/7660893/boxed-geom-text-with-ggplot2
#suppressMessages(require(ggplot2))
#GeomText2 <- proto(GeomText, {
#  objname <- "text2"
#  draw <- function(., data, scales, coordinates, ..., parse = FALSE,
#		   expand = 1.2, bgcol = "grey50", bgfill = NA, bgalpha = 1) {
#    lab <- data$label
#    if (parse) {
#      lab <- parse(text = lab)
#    }
#    with(coordinates$transform(data, scales), {
#      sizes <- llply(1:nrow(data),
#	function(i) with(data[i, ], {
#	  grobs <- textGrob(lab[i], default.units="native", rot=angle, gp=gpar(fontsize=size * .pt))
#	  list(w = grobWidth(grobs), h = grobHeight(grobs))
#	}))
#
#      gList(rectGrob(x, y,
#		     width = do.call(unit.c, lapply(sizes, "[[", "w")) * expand,
#		     height = do.call(unit.c, lapply(sizes, "[[", "h")) * expand,
#		     gp = gpar(col = alpha(bgcol, bgalpha), fill = alpha(bgfill, bgalpha))),
#	    .super$draw(., data, scales, coordinates, ..., parse))
#    })
#  }
#})
##geom_text2 <- GeomText2$build_accessor()
#
#
