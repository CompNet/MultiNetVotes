#############################################################################################
# Defines common functions for all scripts.
# 
# 04/2019 Nejat Arinik
#############################################################################################




#############################################################################################
# Logs the specified message on screen, adding current date and time, and possible some
# offset (to represent the hierarchy of function calls).
#
# offset: number of "." used to represent the hierarchical level of the message.
# ...: parameters fetched to the cat function.
#############################################################################################
tlog <- function(offset=NA, ...)
{	prefix <- paste0("[",format(Sys.time(),"%a %d %b %Y %X"),"] ")
	if(!is.na(offset))
	{	if(is.numeric(offset))
		{	os <- paste(rep(".",offset), sep="", collapse="")
			prefix <- paste0(prefix, os)
		}
		else
			prefix <- paste0(prefix, offset)
	}
	cat(prefix, ..., "\n", sep="")
}


#############################################################################################
# It checks if a prefix constitutes the beginning part of the string (here, 'x') in question.
#   Depending on the R version, this function may not exist in the R base.
#   Ex: startsWith("aabbccdef", "aab")
#
# x: the main string
# prefix: the prefix to be checked for the beginning part of 'x'
#
# returns:  TRUE if the prefix constitutes the beginning part of the string
#			FALSE otherwise
#############################################################################################
startsWith <- function(x, prefix){
	return( substring(x, 1, nchar(prefix)) == prefix )
}




########################################################################
# It converts a weighted graph (.G graph format) into unweighted graph, then writes into a new file
#   by respecting .G graph format. This function is specifically used for kMBS.
#
# network.path: the path of the network to be converted
# unweighted.network.path: new file path for the converted network
#
# return: -
########################################################################
convert.weight.into.unweight.input.graph = 
		function(network.path, unweighted.network.path)
{
	network = file(network.path, 'r')  # connection for reading 
	first.line = readLines(network, n = 1) 
	t <- read.table(network, header = FALSE, skip=1) # skip first line
	close(network) 
	
	weights = t$V3
	converted.weights = sapply(weights, 
			function(w){
				if(w > 0)
					return(1)
				else if(w < 0)
					return(-1)
			}
	)
	t$V3 = converted.weights
	
	write(first.line, unweighted.network.path)
	write.table(
			t, 
			unweighted.network.path, 
			sep="\t", 
			append=TRUE, 
			row.names=FALSE, 
			col.names=FALSE
	)
}




############################################################################
# It computes imbalance for the Correlation Clustering problem from an input graph and a partition information.
#	Imbalance simply means misplaced links regarding a partition. 
#	Misplaced links are the negative ones inside the clusters, and the positive ones between the clusters.
#	It is possible to get the imbalance value in terms of:
#		- imbalance count value at graph level, 
#   	- imbalance percentage value at graph level
#		- imbalance count value at node level.
#
# g: graph
# membership: the vector containing partition information
# output.type: 3 values are possible:
#			   "value" for imbalance count value at graph level. This produces a numerical value.
#			   "percentage" for imbalance percentage value at graph level. This produces a numerical value.
#			   "node.imbalance" for imbalance count value at node level. This might be interesting when one wants to know 
#					imbalance contribution of the nodes. This produces a numerical vector with the same size of the considered graph.
#
# return: imbalance value(s)
############################################################################
# output.type = {"value", "percentage"}
compute.imbalance.from.membership = function(g, membership, output.type = "value"){
	
	membership = as.integer(membership)
	edge.mat <- get.edgelist(g)
	clus.mat <- cbind(membership[as.integer(edge.mat[,1])], membership[as.integer(edge.mat[,2])])
	
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>=0
	
	misplaced <- (clus.mat[,1]==clus.mat[,2] & neg.links) | (clus.mat[,1]!=clus.mat[,2] & pos.links)
	imb.val = sum(abs(E(g)$weight[misplaced]))
	
	# --------------------------------
	lpos.imbalance <- E(g)$weight * as.numeric(misplaced & pos.links)
	lneg.imbalance <- abs(E(g)$weight) * as.numeric(misplaced & neg.links)
	npos.imbalance <- sapply(1:vcount(g), function(u) 
			{	idx <- which(edge.mat[,1]==u | edge.mat[,2]==u)
				result <- sum(lpos.imbalance[idx])
				return(result)
			})
	nneg.imbalance <- sapply(1:vcount(g), function(u) 
			{	idx <- which(edge.mat[,1]==u | edge.mat[,2]==u)
				result <- sum(lneg.imbalance[idx])
				return(result)
			})
	
	max.val = max(c(npos.imbalance,nneg.imbalance))
	if(max.val != 0){ # if the situation has some imbalance
		npos.imbalance <- npos.imbalance / max.val # normalized
		nneg.imbalance <- nneg.imbalance / max.val # normalized
	}
	# --------------------------------
	
	# make them explicit
	n.in.clu.imb = nneg.imbalance # negative misplaced links are the misplaced link insde clusters
	n.betw.clu.imb = npos.imbalance # pisitive misplaced links are the misplaced link between clusters
	
	# ========================
	# ========================
	
	if(output.type == "value")
		return(format(round(imb.val, 3), nsmall = 3)) # 3 decimal floating
	else if(output.type == "percentage"){
		perc = (imb.val/ sum(abs(E(g)$weight)))*100
		return(format(round(perc, 3), nsmall = 3))
	} else if(output.type == "node.imbalance") # normalized
		return( list(in.imb=n.in.clu.imb, betw.imb=n.betw.clu.imb) )
 	else
		return(NA)
	
}



############################################################################
# It computes imbalance for the relaxed version of Correlation Clustering problem from an input graph and a partition information.
#	Imbalance simply means misplaced links regarding a partition. 
#	Misplaced links are the negative ones inside the clusters, and the positive ones between the clusters.
#	It is possible to get the imbalance value in terms of:
#		- imbalance count value at graph level, 
#   	- imbalance percentage value at graph level
#		- imbalance count value at node level.
#
# g: graph
# membership: the vector containing partition information
# output.type: 3 values are possible:
#			   "value" for imbalance count value at graph level. This produces a numerical value.
#			   "percentage" for imbalance percentage value at graph level. This produces a numerical value.
#			   "node.imbalance" for imbalance count value at node level. This might be interesting when one wants to know 
#					imbalance contribution of the nodes. This produces a numerical vector with the same size of the considered graph.
#
# return: imbalance value(s)
############################################################################
compute.relaxed.imbalance.from.membership = function(g, membership, output.type = "value"){
	
	edge.mat <- get.edgelist(g)
	clus.mat <- cbind(membership[edge.mat[,1]], membership[edge.mat[,2]])
	
	#compare link signs and positions 
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>=0
	
	nb.clu = length(unique(membership))
	
	# compute the imbalance (i.e. cost) of intra-edges
	imb.val=0
	n.in.clu.imb = rep(0, vcount(g))
	for(clu in seq_len(nb.clu)){
		in.edges = (clus.mat[, 1] == clu & clus.mat[, 2] == clu)
		
		pos.misplaced = pos.links & in.edges
		neg.misplaced = neg.links & in.edges
		pos.cost = sum(E(g)$weight[pos.misplaced])
		neg.cost = sum(abs(E(g)$weight[neg.misplaced]))
		
		
		l.imbalance=NA
		if(neg.cost > pos.cost){ # if positive links are dominant, take the negative ones
			l.imbalance <- E(g)$weight * as.numeric(pos.misplaced)
			imb.val = imb.val + pos.cost
		}
		else{ # if neg.cost=pos.cost, the neg.cost will be chosen
			l.imbalance <- abs(E(g)$weight) * as.numeric(neg.misplaced)
			imb.val = imb.val + neg.cost
		}
		
		
		# node imbalance
		n.imbalance <- sapply(1:vcount(g), function(u) 
				{	idx <- which(edge.mat[,1]==u | edge.mat[,2]==u)
					result <- sum(l.imbalance[idx])
					return(result)
				})
		n.in.clu.imb = n.in.clu.imb + n.imbalance
	}
	
	
	# --------------------------------------------------------------
	# --------------------------------------------------------------
	
	
	# compute the imbalance (i.e. cost) of inter-edges if nb.clu > 1
	n.betw.clu.imb = rep(0, vcount(g))
	if(nb.clu > 1){
		pair.list = combn(x=nb.clu, m=2) # x'in m'li combinasyonu
		nb.pair = length(pair.list)/2
		
		for(i in seq_len(nb.pair)){
			clu1 = pair.list[1, i]
			clu2 = pair.list[2, i]
			
			betw.edges = (clus.mat[, 1] == clu1 & clus.mat[, 2] == clu2) | (clus.mat[, 1] == clu2 & clus.mat[, 2] == clu1)
			
			pos.misplaced = pos.links & betw.edges
			neg.misplaced = neg.links & betw.edges
			pos.cost = sum(E(g)$weight[pos.misplaced])
			neg.cost = sum(abs(E(g)$weight[neg.misplaced]))
			
			
			l.imbalance=NA
			if(pos.cost > neg.cost){ # if positive links are dominant, take the negative ones
				l.imbalance <- abs(E(g)$weight) * as.numeric(neg.misplaced)
				imb.val = imb.val + neg.cost
			}
			else{  # if neg.cost=pos.cost, the pos.cost will be chosen
				l.imbalance <- E(g)$weight * as.numeric(pos.misplaced)
				imb.val = imb.val + pos.cost
			}
			
			# node imbalance
			n.imbalance <- sapply(1:vcount(g), function(u) 
					{	idx <- which(edge.mat[,1]==u | edge.mat[,2]==u)
						result <- sum(l.imbalance[idx])
						return(result)
					})
			n.betw.clu.imb = n.betw.clu.imb + n.imbalance
		}
	}
	
	max.val = max(c(n.in.clu.imb,n.betw.clu.imb))
	if(max.val != 0){ # if the situation has some imbalance
		n.in.clu.imb <- n.in.clu.imb / max.val # normalized
		n.betw.clu.imb <- n.betw.clu.imb / max.val # normalized
	}
	
	
	# ========================
	# ========================
	
	if(output.type == "value")
		return(format(round(imb.val, 3), nsmall = 3)) # 3 decimal floating
	else if(output.type == "percentage"){
		perc = (imb.val/ sum(abs(E(g)$weight)))*100
		return(format(round(perc, 3), nsmall = 3))
	} else if(output.type == "node.imbalance") # normalized
		return( list(in.imb=n.in.clu.imb, betw.imb=n.betw.clu.imb) )
	else
		return(NA)
}




############################################################################
# It computes the normalized value of MEP absences based on the given 'votes' matrix.
#
# votes: matrix of the considered MEP (rows) votes for the considered documents (columns).
#
# return: a vector containing normalized value of MEP absences 
############################################################################
get.meps.absences = function(votes){
	nb.mep = nrow(votes)
	nb.vote = ncol(votes)
	
	counts=rep(0,nb.mep)
	for(m in 1:nb.mep){
		mep.votes = votes[m,]
		indx.expr = which(mep.votes %in% c(VOTE.ABST,VOTE.AGST,VOTE.FOR))
		counts[m] = nb.vote-length(indx.expr)
	}
	
	return(counts/nb.vote) # normalize
}


############################################################################
# It computes the normalized value of MEP abstentions based on the given 'votes' matrix.
#
# votes: matrix of the considered MEP (rows) votes for the considered documents (columns).
#
# return: a vector containing normalized value of MEP abstentions 
############################################################################
get.meps.abstentions = function(votes){
	nb.mep = nrow(votes)
	nb.vote = ncol(votes)
	
	counts=rep(0,nb.mep)
	for(m in 1:nb.mep){
		mep.votes = votes[m,]
		indx.abst = which(mep.votes == VOTE.ABST)
		counts[m] = length(indx.abst)
	}
	
	return(counts/nb.vote) # normalize
}