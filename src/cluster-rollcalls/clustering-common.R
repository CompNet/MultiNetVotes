

#############################################################################################
# It build the similarity matrix. Note that this method calls the method 'compare.partition.pair2()'
# which retreives commun index values, which are not "NA", for each partition and applies the specified measure.
# "NA" is possible if considered vote types are not exhaustive. For instance if a MEP is absent in a roll-call, and
# if the partitions are obtained by considering only For and Against votes, then a "NA" value is appeared.
#
#
# partitions: partitions for the considered roll-calls.
#				Note that each roll-call is partitioned according to considered vote types (e.g. FAA)
# measure: similarity measure in order to calcualte similarity matrix
#
# returns the similarity matrix
#
#############################################################################################
create.intersection.based.similarity.matrix = function(partitions, measure){
	
	nb.partition = length(partitions)
	sim.mtrx = matrix(NA, nrow=nb.partition, ncol=nb.partition)
	for(i in 1:nb.partition){ # row
		sim.mtrx[i,i]=1
		
		for(j in 1:nb.partition){ # column
			if(i<j){ # take the upper diagonal
				partition1 = partitions[[i]]
				partition2 = partitions[[j]]
				
				val <- compare.partition.pair2(partition1, partition2, measure)
				# 'val' is a list. In this case, we need to retreive only the 1st item
				sim.mtrx[i,j]=val[1]
				sim.mtrx[j,i]=val[1] # symetry
			}
		}
	}
	
	if(!is.null(names(partitions))){
		colnames(sim.mtrx) = names(partitions)
		row.names(sim.mtrx) = names(partitions)
	}
	
	return(sim.mtrx)
}



#############################################################################################
# It creates a similarity matrix by comparing pairs of partitions through given similarity measure.
# 
# partitions: partitions for the considered roll-calls.
#				Note that each roll-call is partitioned according to considered vote types (e.g. FAA)
# measure: similarity measure in order to calcualte similarity matrix
#
# returns the similarity matrix
#############################################################################################
create.similarity.matrix = function(partitions, measure){
	
	nb.partition = length(partitions)
	sim.mtrx = matrix(NA, nrow=nb.partition, ncol=nb.partition)
	for(i in 1:nb.partition){ # row
		sim.mtrx[i,i]=1
		
		for(j in 1:nb.partition){ # column
			if(i<j){ # take the upper diagonal
				partition1 = partitions[[i]]
				partition2 = partitions[[j]]
				
				val <- compare.partition.pair(partition1, partition2, measure)
				# 'val' is a list. In this case, we need to retreive only the 1st item
				sim.mtrx[i,j]=val[1]
				sim.mtrx[j,i]=val[1] # symetry
			}
		}
	}
	
	if(!is.null(names(partitions))){
		colnames(sim.mtrx) = names(partitions)
		row.names(sim.mtrx) = names(partitions)
	}
		
	return(sim.mtrx)
}







#############################################################################################
# It keeps only some top clustering results, based on the epsilon value.
#
# rollcall.clu.result.per.k: a matrix where columns correspond to average silhouette score and k value.
#								Each line corresponds to a single (k, silhouette score) pair.
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
#
# returns  filtered silhouette and k values
#############################################################################################
filter.rollcall.clu.result.by.epsilon = function(rollcall.clu.result.per.k, epsilon=NA){
	sil.vals = as.numeric(rollcall.clu.result.per.k[,COL.AVG.SILH])
	best.indx = which.max(sil.vals)
	best.sil.val = sil.vals[best.indx]
	
	if(!is.na(epsilon))
		return( which(sil.vals >= (best.sil.val-epsilon)) )
	else
		return( 1:nrow(rollcall.clu.result.per.k) )  # take all indexes
}





#############################################################################################
# It keeps only some top clustering results, based on a range of k values.
#
# rollcall.clu.result.per.k: a matrix where columns correspond to average silhouette score and k value.
#								Each line corresponds to a single (k, silhouette score) pair.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values. c(NA,NA) means there is not any limit
#
# returns  filtered silhouette and k values
#############################################################################################
filter.rollcall.clu.result.by.k = function(rollcall.clu.result.per.k, k.limits=c(NA,NA)){
	k.minlimit = k.limits[1]
	k.maxlimit = k.limits[2]
	k.vals = as.numeric(rollcall.clu.result.per.k[,COL.K])
	
	if(!is.na(k.minlimit) && is.na(k.maxlimit))
		return( which(k.minlimit<=k.vals) )
	else if(is.na(k.minlimit) == !is.na(k.maxlimit))
		return( which(k.vals<=k.maxlimit) )
	else if(!is.na(k.minlimit) && !is.na(k.maxlimit))
		return( which(k.vals<=k.maxlimit & k.minlimit<=k.vals) )
	else
		return(1:nrow(rollcall.clu.result.per.k)) # take all indexes
}




#############################################################################################
# It keeps only some top clustering results, based on the epsilon value and a range of k values.
#
# rollcall.clu.result.per.k: a matrix where columns correspond to average silhouette score and k value.
#								Each line corresponds to a single (k, silhouette score) pair.
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values. c(NA,NA) means there is not any limit
#
# returns  filtered silhouette and k values
#############################################################################################
filter.rollcall.clu.result.by.epsilon.and.k = function(rollcall.clu.result.per.k, epsilon, k.limits){
	indx1 = filter.rollcall.clu.result.by.epsilon(rollcall.clu.result.per.k, epsilon)
	indx2 = filter.rollcall.clu.result.by.k(rollcall.clu.result.per.k, k.limits)
	indx = intersect(indx1,indx2)
	return(indx)
}




#############################################################################################
# It retreives theme ids associated with the considered roll-calls.
#
# rollcall.details: description of each voted roll-call.
# rollcall.ids: roll-call ids
#
# returns theme ids
#############################################################################################
retreive.rollcall.theme.ids = function(rollcall.details, rollcall.ids){
	
	s.theme.ids = rollcall.details[as.integer(rollcall.ids), COL.THEME.ID]
	theme.ids = strsplit(s.theme.ids, split=",")
	theme.ids = lapply(theme.ids, function(r) as.integer(r))
	
	if(length(unlist(theme.ids)) < length(s.theme.ids))
		return(NA) # that means that some roll-calls do not have associated theme ids
	return(theme.ids)
}





#############################################################################################
# It retreives theme names by theme id.
#
# domain: domain name (e.g. AGRI)
# theme.ids: theme ids
#
# returns theme names
#############################################################################################
retreive.theme.names.by.id = function(domain, theme.ids){
	theme.details = read.csv(file=file.path(OVERALL.FOLDER,"rollcall-theme-details.csv"), sep=";", check.names=FALSE)	
	indx = which(theme.details[,COL.DOMID] == domain)
	dom.based.theme.details = theme.details[indx,]
	
	res = dom.based.theme.details[theme.ids,COL.THEME.NAME]
	return(res)
}




#############################################################################################
# It retreives theme names by domain.
#
# domain: domain name (e.g. AGRI)
#
# returns theme names
#############################################################################################
retreive.theme.names.by.domain = function(domain){
	theme.details = read.csv(file=file.path(OVERALL.FOLDER,"rollcall-theme-details.csv"), sep=";", check.names=FALSE)	
	indx = which(theme.details[,COL.DOMID] == domain)
	return(theme.details[indx,COL.THEME.NAME])
}





#############################################################################################
# It prepares a description string for a given measure name, considered vote types in roll-call clustering (e.g. FAA),
# calculated silhouette values and k values. The form would be in this way: <MEASURE>-<K>-<SILH.VAL>
#
#
# measure: similarity measure in order to calcualte similarity matrix
# vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# sil.vals: considered silhouette values
# k.vals: considered 'k' values regarding roll-call clusteirng process
#
# returns a description string (e.g. 'F.purity-k=2-sil=0.52106/')
#############################################################################################
prepare.descs.for.silhouette.partitions = function(measure=NA, vote.types=NA, sil.vals=NA, k.vals=NA){
	descs = ""
	sil.vals = round(sil.vals, digits = 5)
	
	if(!is.na(measure)) 
		descs = paste(descs,measure,sep="")
	if(!is.na(vote.types))
		descs = paste(descs," ",vote.types,sep="")
	
	if(!all(is.na(sil.vals)))
		descs = paste(descs," ","sil=",sil.vals,sep="")
	if(!all(is.na(k.vals)))
		descs = paste(descs," ","k=",k.vals,sep="")
	
	return(descs)
}
