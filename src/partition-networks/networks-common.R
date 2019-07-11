#############################################################################################
# Functions used by several network-related scripts.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("igraph")



#############################################################################################
# Loads all three versions of the graphs contained in the specified folder: signed, positive
# and complementary negative.
#
# score: name of the score table used to process the agreement index.
# thresh: thresholds used for network extraction (vector of two values).
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period.
# comp: whether to return the complementary negative graph (for clustering) or just the
#		negative graph (to assess partitions).
#
# returns: a list containing the three graphs (named: signed, pos, neg).
#############################################################################################
retrieve.graphs <- function(score, thresh, country, group, domain, period, comp)
{	folder <- get.networks.path(score, thresh, country, group, domain, period)
	
	# load the original graph
	graph.file <- file.path(folder,paste0(SIGNED.FILE,".graphml"))
	g <- NA
	if(!file.exists(graph.file))
		tlog("........WARNING: Graph file ",graph.file," not found")
	else
		g <- suppressWarnings(read.graph(file=graph.file, format="graphml"))
	
	# load the positive graph
	graph.file.pos <- file.path(folder,paste0(POSITIVE.FILE,".graphml"))
	g.pos <- NA
	if(!file.exists(graph.file.pos))
		tlog("........WARNING: Graph file ",graph.file.pos," not found")
	else
		g.pos <- suppressWarnings(read.graph(file=graph.file.pos, format="graphml"))
	
	if(comp)
	{	# load the complementary negative graph
		graph.file.neg <- file.path(folder,paste0(COMP.NEGATIVE.FILE,".graphml"))
		g.neg <- NA
		if(!file.exists(graph.file.neg))
			tlog("........WARNING: Graph file ",graph.file.neg," not found")
		else
			g.neg <- suppressWarnings(read.graph(file=graph.file.neg, format="graphml"))
	}
	else
	{	# build the negative graph
		g.neg <- subgraph.edges(graph=g, eids=which(E(g)$weight<0), delete.vertices=FALSE)
	}
	
	# build and return the result list
	result <- list(neg=g.neg, pos=g.pos, signed=g)
	return(result)
}



#############################################################################################
# Receives a list of matrices, all with the same dimension, and processes individual mean
# and standard deviation of each matrix term.
# 
# l: list of same-sized matrices.
#
# returns: a list containing an "avg" and a "stdev" matrices, corresponding to the average
#          and standard deviation of each list of matrix elements, respectively.
#############################################################################################
average.matrix.list <- function(l)
{	# init result
	avg <- matrix(0,nrow=nrow(l[[1]]),ncol=ncol(l[[1]]))
	rownames(avg) <- rownames(l[[1]])
	colnames(avg) <- colnames(l[[1]])
	stdev <- matrix(0,nrow=nrow(l[[1]]),ncol=ncol(l[[1]]))
	rownames(stdev) <- rownames(l[[1]])
	colnames(stdev) <- colnames(l[[1]])
	
	# process mean
	cnt <- matrix(0,nrow=nrow(l[[1]]),ncol=ncol(l[[1]]))
	for(i in 1:length(l))
	{	m <- l[[i]]
		# retain only numerical values
		vals <- m
		vals[is.na(vals) | is.nan(vals) | is.infinite(vals)] <- 0
		avg <- avg + vals
		# count numerical values
		cnts <- matrix(1,nrow=nrow(m),ncol=ncol(m))
		cnts[is.na(m) | is.nan(m) | is.infinite(m)] <- 0
		cnt <- cnt + cnts
	}
	avg <- avg / cnt
	
	# process standard devation
	for(i in 1:length(l))
	{	m <- l[[i]]
		# retain only numerical values
		vals <- (m - avg)^2
		vals[is.na(vals) | is.nan(vals) | is.infinite(vals)] <- 0
		stdev <- stdev + vals
	}
	stdev <- sqrt(stdev/(cnt-1))
	
	# set up result
	result <- list(avg=avg, stdev=stdev)
	return(result)
}






#############################################################################
## First use the "has.any.isolated.nodes" method: If TRUE returns, use this method
## 
#############################################################################
#get.isolated.nodes.indx = function(g){
#	iso.index = which(degree(g) == 0)
#	return(iso.index)
#}
#
#
#
#########################################################################
## network.path is needed for a graph object
#########################################################################
#post.proc.membership.for.isolated.nodes = function(g, membership, option.no=1){
#
#	# when option.no=1, each isolated node will be placed into a different cluster (to enumerate them)
#	# when option.no=2, every isolated node will be placed into the same cluster
#	
#	has.neg.weight = any(which(membership == -1)) # if it is a solution from KMBS, the partition might contain -1 values
#	
#	iso.index = get.isolated.nodes.indx(g)
#	nb.iso.node = length(iso.index)
#	if(nb.iso.node > 0){
#		if(option.no == 1){
#			# detect the main cluster numbers (when we omit isoalted nodes)
#			nb.clu.without.iso.nodes = length(unique(membership[-iso.index]))
#			if(has.neg.weight)
#				nb.clu.without.iso.nodes = nb.clu.without.iso.nodes - 1	
#			
#			membership[iso.index] = seq(nb.clu.without.iso.nodes+1, nb.clu.without.iso.nodes + nb.iso.node)
#			# nb.clu=2 and length(iso.index)=4, so we need: seq(2+1, 2+4) in order to enumerate the isolated nodes
#		} else { # if(option.no == 2){
#			# detect the main cluster numbers (when we omit isoalted nodes)
#			nb.clu.without.iso.nodes = length(unique(membership[-iso.index]))
#			if(has.neg.weight)
#				nb.clu.without.iso.nodes = nb.clu.without.iso.nodes - 1
#			
#			CLU.NO.FOR.ALL.ISOLATED.NODES = nb.clu.without.iso.nodes + 1
#			membership[iso.index] = CLU.NO.FOR.ALL.ISOLATED.NODES
#		}
#	}
#	
#	return(membership)
#}