############################################################################
# These functions define Purity and inverse Purity measures, which are also used in F-measure.
# 
# You can find a well-documented paper, written by Vincent Labatut, on Purity and related similarity measure on:
#	https://arxiv.org/pdf/1303.5441.pdf
#   Github code source: https://github.com/CompNet/TopoMeasures/blob/master/src/PurityMeasure.R
############################################################################
# main lib, used to represent and process graphs
library("igraph")




############################################################################
# Processes the confusion matrix for the specified partitions, i.e. the matrix 
# whose cell (i,j) contains the number of elements belonging to part #i in 
# partition #1 and to part #j in partition #2.
# 
# partition1: the first partition to consider, represented as an integer vector.
#			  Each value in the vector represents the id of a part. The parts 
#			  must be counted starting from one (not zero).
# partition2: the second partition to consider. Same representation than for
#			  the first one. Both are inter-exchangeable (symmetric measure).
# returns: the confusion matrix, a.k.a. contingency table.
############################################################################
process.confusion.matrix <- function(partition1, partition2)
{	p1 <- factor(partition1, levels=1:max(partition1))
	p2 <- factor(partition2, levels=1:max(partition2))
	result <- as.matrix(table(p1,p2))
	return(result)
}




############################################################################
# It computes the purity measure between obtained partition and a reference one. Note that
# Purity is not symmetric.
#
# partition: the obtained partition from the clustering process
# ground.truth: a reference partition. It can be mono-labeled, as well as multi-labeled.
#				An example of multi-labeling: a roll-call may concern both economical and ecological aspects
# purity.level: level at which purity is performed. It might be at partition level (i.e. purity.level="partition"),
#					or cluster level (i.e. purity.level="cluster"). When purity.level="cluster", 
#					the method outputs the purity value of each cluster in the 'partition' variable.
#					Otherwise, it outputs the general putity associated with the 'partition' variable.
#
# return the purity value
############################################################################


compute.purity <- function(partition, ground.truth, purity.level="partition")
{	# process the confusion matrix
	conf.matrix <- process.confusion.matrix(partition,ground.truth)
	
	
	# init
	total <- 0
	nb.total.in.clu <- rep(0,nrow(conf.matrix))
	purity.per.cluster = c()
	
	# for each part in partition, identify the corresponding parts in ground.truth
	# i.e. those with the largest intersection (there can be several)
	for(r in 1:nrow(conf.matrix)){
		nb.total.in.clu[r] <- sum(conf.matrix[r,])
		max.val = max(conf.matrix[r,])
		purity.per.cluster[r] = max.val/nb.total.in.clu[r]
	}
	
	if(purity.level == "partition"){
		# compute the purity of the partition
		n = sum(conf.matrix)
		for(r in 1:nrow(conf.matrix))
			total <- total + (nb.total.in.clu[r]/n)*purity.per.cluster[r]
		return(total)
	}
	
	# prepare output for writing into csv
	m = cbind(1:length(purity.per.cluster), nb.total.in.clu, purity.per.cluster)
	colnames(m) = c("k", "nb.item.in.clu", "purity")
	
	return(m)
}




############################################################################
# It computes the purity measure between obtained partition and multi-labaled reference one. Note that
# Purity is not symmetric.
# An example:
# partition    = {  1,    1,   2,    1,    2,   1}
# ground.truth = {(A,B), (C), (A), (A,B), (A), (C)}
#      ======= they turn out to be==========>
# new.p  = {1,1, 1, 2, 1,1, 2, 1}
# new.gt = {A,B, C, A, A,B, A, C}
#
# 
# ground.truth: a reference partition. It can be mono-labeled, as well as multi-labeled.
#				An example of multi-labeling: a roll-call may concern both economical and ecological aspects
# partition: the obtained partition from the clustering process
# purity.level: level at which purity is performed. It might be at partition level, or cluster level.
#
# return the purity value
############################################################################
compute.purity.for.partition.vs.multi.labeled.ground.truth = function(partition, ground.truth, purity.level="partition"){
	
	# ================================================================
	# turn this multi-labelling task into mono-labelling task
	new.p = c()
	new.gt = c()
	
	n = length(partition)
	for(i in 1:n){
		labels = ground.truth[[i]]
		nb.label = length(labels)
		new.p = c(new.p, rep(partition[i],nb.label))
		new.gt = c(new.gt, labels)
	}
	# ================================================================
	
	res = compute.purity(new.p, new.gt, purity.level)
	return(res)
}


############################################################################
# It computes the purity measure between multi-labaled reference partition and obtained one. Note that
# Purity is not symmetric.
# An example:
# partition    = {  1,    1,   2,    1,    2,   1}
# ground.truth = {(A,B), (C), (A), (A,B), (A), (C)}
#      ======= they turn out to be==========>
# new.p  = {1,1, 1, 2, 1,1, 2, 1}
# new.gt = {A,B, C, A, A,B, A, C}
#
# 
# ground.truth: a reference partition. It can be mono-labeled, as well as multi-labeled.
#				An example of multi-labeling: a roll-call may concern both economical and ecological aspects
# partition: the obtained partition from the clustering process
# purity.level: level at which purity is performed. It might be at partition level, or cluster level.
#
# return the purity value
############################################################################
compute.purity.for.multi.labeled.ground.truth.vs.partition = function(ground.truth, partition, purity.level="partition"){
	
	# ================================================================
	# turn this multi-labelling task into mono-labelling task
	new.p = c()
	new.gt = c()
	
	n = length(partition)
	for(i in 1:n){
		labels = ground.truth[[i]]
		nb.label = length(labels)
		new.p = c(new.p, rep(partition[i],nb.label))
		new.gt = c(new.gt, labels)
	}
	# ================================================================
	
	res = compute.purity(new.gt, new.p, purity.level)
	return(res)
}
