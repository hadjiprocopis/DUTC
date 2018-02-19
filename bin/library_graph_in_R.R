suppressMessages(library(igraph))

#source('~/bin/library_graph_in_R.R')
#a_graph <- igraph_read_string_links_file(file='/home/andreas/data/STRING/9606.protein.links.v9.0.txt')
# E(a_graph)[from('9606.ENSP00000000233')]
# lc <- get.all.shortest.paths(a_graph,from=1); subgraph(a_graph, lc[[1]])

igraph_get_numerical_indices <- function(
	agraph=NULL
){
	
	return(indices)
}
	
get.all.shortest.paths.symbolic <- function(graph, from, to = V(graph), mode = c("all", "out","in"))
{
	if(!is.igraph(graph)){stop("Not a graph object") }
	mode <- igraph:::igraph.match.arg(mode)
	mode <- switch(mode, out = 1, `in` = 2, all = 3)
	on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
	.Call("R_igraph_get_all_shortest_paths", graph, igraph:::as.igraph.vs(graph, from),igraph:::as.igraph.vs(graph, to), as.numeric(mode), PACKAGE = "igraph")
}
igraph_read_string_links_file <- function(
	# model file is /home/andreas/data/STRING/9606.protein.links.v9.0.txt
	file=NULL,
	sep=' ',
	header=TRUE,
	directed=TRUE,
	detailed=FALSE,
	header_spec=list(
		protein1='protein1', # from the header of the file
		protein2='protein2',
		combined_score='combined_score'
	)
){
	if( detailed == TRUE ){
		header_spec=list(
			protein1='protein1',
			protein2='protein2',
			neighborhood='neighborhood',
			fusion='fusion',
			cooccurence='cooccurence',
			coexpression='coexpression',
			experimental='experimental',
			database='database',
			textmining='textmining',
			combined_score='combined_score'
		)
	}
	write(paste("library_graph_in_R.R::igraph_read_string_links_file : reading from file '", file, "'", sep=""), stderr())
	data_from_file <- NULL
	tryCatch({data_from_file <- read.table(file=file, sep=sep, header=header)},
		error=function(ex){write(paste("library_graph_in_R.R::igraph_read_string_links_file : could not read file '", file, "'", sep=""), stderr()); return(NULL) },
		warning=function(ex){write(paste("library_graph_in_R.R::igraph_read_string_links_file : could not read file '", file, "'", sep=""), stderr()); return(NULL) }
	);
	write(paste("library_graph_in_R.R::igraph_read_string_links_file : done.", sep=""), stderr())
	header=names(data_from_file)
	for(a_name in names(header_spec)){ if( ! a_name %in% header ){ write(paste("library_graph_in_R.R::igraph_read_string_links_file : could not find column name '",a_name,"' in file '", file, "'",  sep=""), stderr()); return(NULL) } }

	a_graph <- graph.data.frame(d=data_from_file, directed=directed) # the first 2 are from,to, rest are attributes
	return(a_graph)
}
igraph_read_string_actions_file <- function(
	# model file is /home/andreas/data/STRING/9606.protein.actions.v9.0.txt
	file=NULL,
	sep=' ',
	header=TRUE,
	directed=TRUE,
	detailed=FALSE,
	header_spec=list(
		item_id_a='item_id_a', # from the header of the file
		item_id_b='item_id_b',
		mode='mode',
		action='action',
		a_is_acting='a_is_acting',
		score='score'
	)
){
	if( detailed == TRUE ){
		header_spec=list(
			item_id_a='item_id_a',
			item_id_b='item_id_b',
			mode='mode',
			action='action',
			a_is_acting='a_is_acting',
			score='score',
			sources='sources',
			transferred_sources='transferred_sources'
		)
	}
	write(paste("library_graph_in_R.R::igraph_read_string_actions_file : reading from file '", file, "'", sep=""), stderr())
	data_from_file <- NULL
	tryCatch({data_from_file <- read.table(file=file, sep=sep, header=header)},
		error=function(ex){write(paste("library_graph_in_R.R::igraph_read_string_actions_file : could not read file '", file, "'", sep=""), stderr()); return(NULL) },
		warning=function(ex){write(paste("library_graph_in_R.R::igraph_read_string_actions_file : could not read file '", file, "'", sep=""), stderr()); return(NULL) }
	);
	write(paste("library_graph_in_R.R::igraph_read_string_actions_file : done.", sep=""), stderr())
	header=names(data_from_file)
	for(a_name in names(header_spec)){ if( ! a_name %in% header ){ write(paste("library_graph_in_R.R::igraph_read_string_actions_file : could not find column name '",a_name,"' in file '", file, "'",  sep=""), stderr()); return(NULL) } }

	a_graph <- graph.data.frame(d=data_from_file, directed=directed) # the first 2 are from,to, rest are attributes
	return(a_graph)
}

