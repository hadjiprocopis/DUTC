# removes specified columns
# returns a list of 'removed' and 'retained' columns
# columns can be added back using merge_datasets()
remove_columns <- function(
	inp=NULL,
	colnames_to_remove=NULL
){
	ret=list()
	ret[['removed']] = list()
	for(acolname in colnames_to_remove){
		ret[['removed']][[acolname]] = inp[[acolname]]
		inp[[acolname]] <- NULL
	}
	ret[['retained']] = inp
	return(ret)
}
merge_datasets <- function(
	inp1=NULL,
	inp2=NULL
){
	return(c(inp1, inp2))
}


# returns NULL on failure or
# the read data on success
read_data <- function(
	filename=NULL
){
	whoami=sub('\\(.*$', '()', deparse(sys.call()))
	dat = read.table(filename, sep=',', header=TRUE)
	if( is.null(dat) ){
		cat(whoami, ": failed to read data from file '",filename,"'.\n")
		return(NULL)
	}
	cat(whoami, ": data read from file '", filename,"'.\n", sep='')
	return(dat)
}
# returns 1 on success, 0 on failure
save_data <- function(
	dat=adat,
	filename=NULL
){
	whoami=sub('\\(.*$', '()', deparse(sys.call()))
	status <- try(
		write.table(dat, filename, sep=',',
			col.names=TRUE, row.names=FALSE
		)
	)
	if( ! is.null(status) ){
		cat(whoami, ": failed to save data to file '",filename,"'.\n", sep='')
		return(0)
	}
	cat(whoami, ": data saved to file '", filename,"'.\n", sep='')
	return(1)
}
