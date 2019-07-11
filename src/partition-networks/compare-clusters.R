#############################################################################################
# Set of function in charge for comparing previously detected partitions.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
source("src/partition-networks/networks-common.R")


#############################################################################################
# Processes all the specified measures comparing both partitions.
#
# partition1: first partition.
# partition2: second partition.
# measures: vector of measure names: c("vi", "nmi", "split.join", "rand", "adjusted.rand").
#
# returns: vector of values corresponding to each specified measures.
#############################################################################################
compare.partition.pair <- function(partition1, partition2, measures="nmi")
{	# init result vector
	result <- rep(NA,length(measures))
	names(result) <- measures
	
	
	if("adjusted.rand" %in% measures){
		library(clues)
		res = adjustedRand(partition1, partition2)
		result["adjusted.rand"] = (res["HA"]+1)/2 # normalize the value as ARI is between -1 and +1
	} else if("adjusted.rand2" %in% measures){
		library(clues)
		res = adjustedRand(partition1, partition2)
		result["adjusted.rand2"] = res["HA"]
		if(res["HA"]<0) result["adjusted.rand2"] = 0 # as neg values rarely occur, set them 0
	} else if("F.purity" %in% measures){
		library(NMF)
		res1 = NMF::purity(as.factor(partition1), as.factor(partition2))
		res2 = NMF::purity(as.factor(partition2), as.factor(partition1))
		result["F.purity"] = (2*res1*res2)/(res1+res2)
	} else{
		
		# process measures for specified partitions: "nmi" and "rand"
		for(measure in measures)
			result[measure] <- igraph::compare(partition1, partition2, method=measure)
	}
	
	# TODO one can add the processing of other measures here if needed
	
	
	return(result)
}




#############################################################################################
# Processes all the specified measures comparing both partitions.
# The difference between the methods "compare.partition.pair2" and "compare.partition.pair" is that
# this method retreives commun index values, which are not "NA", for each partition and applies the specified measure.
# Because, when some of the values are "NA" in both partitions, it is possible that "NA" values are not commun.
# That is why we need to get the intersection of both partitions.
#
# partition1: first partition. The length(partition1) should be equal to length(partition1) as in the method ""compare.partition.pair""
# partition2: second partition.
# measures: vector of measure names: c("vi", "nmi", "split.join", "rand", "adjusted.rand").
#
# returns: vector of values corresponding to each specified measures.
#############################################################################################
compare.partition.pair2 <- function(partition1, partition2, measures="nmi")
{	# init result vector
	result <- rep(NA,length(measures))
	names(result) <- measures
	
	# ===========================================================
	# get rid of the "NA" values: detect the "NA" indexes and do "AND" operator for intersection
	tmp.p1 = partition1
	tmp.p2 = partition2
	
	indx1 = which(is.na(partition1))
	tmp.p1[indx1] = 0
	tmp.p1[-indx1] = 1
	indx2 = which(is.na(partition2))
	tmp.p2[indx2] = 0
	tmp.p2[-indx2] = 1
	
	commun.indx = tmp.p1 & tmp.p2
	p1 = partition1[commun.indx]
	p2 = partition2[commun.indx]
	# ===========================================================
	
	
	
	
	if("adjusted.rand" %in% measures){
		library(clues)
		res = adjustedRand(p1, p2)
		result["adjusted.rand"] = (res["HA"]+1)/2 # normalize the value as ARI is between -1 and +1
	} else if("adjusted.rand2" %in% measures){
		library(clues)
		res = adjustedRand(p1, p2)
		result["adjusted.rand2"] = res["HA"]
		if(res["HA"]<0) result["adjusted.rand2"] = 0 # as neg values rarelt occur, set them 0
	} else if("F.purity" %in% measures){
		library(NMF)
		res1 = NMF::purity(as.factor(p1), as.factor(p2))
		res2 = NMF::purity(as.factor(p2), as.factor(p1))
		result["F.purity"] = (2*res1*res2)/(res1+res2)
	} else{
		
		# process measures for specified partitions: "nmi" and "rand"
		for(measure in measures)
			result[measure] <- igraph::compare(p1, p2, method=measure)
	}
	
	# TODO one can add the processing of other measures here if needed
	
	
	return(result)
}




#############################################################################################
# Processes the specified measures comparing all specified partitioning algorithms for the
# data contained in the specified folder.
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# comdet.algos: vector of community detection algorithm names.
# corclu.algos: vector of correlation clustering algorithm names.
# measures: vector of comparison measures to process.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
compare.partitions.measures <- function(thresh=NA, score.file, domain, date, country, group, comdet.algos, corclu.algos, measures="nmi", repetitions)
{	# init the list used to process the average
	if(repetitions>1)
	{	avg.list <- list()
		for(meas in measures)
			avg.list[[meas]] <- list()
	}
	
	# the process might be repeated several times
	for(r in 1:repetitions)
	{	tlog("......Processing iteration ",r,"/",repetitions)
		# setup iteration folder
		if(repetitions>1)
			#r.folder <- paste0(folder,r,"/")
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=date, repetition=r)
		else
			#r.folder <- folder
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=date, repetition=NA)
		
		# load partitions
		partitions <- list()
		for(corclu.name in corclu.algos)
		{	tlog("........Loading partition files for algorithm ",corclu.name)
			partition.file <- file.path(part.folder,paste0(corclu.name,"-membership.txt"))
			if(!file.exists(partition.file))
				tlog("........Partition file ",partition.file," not found")
			else
				partitions[[corclu.name]] <- as.matrix(read.table(partition.file))
		}
		for(comdet.name in comdet.algos)
		{	tlog("........Loading partition files for algorithm ",comdet.name)
			partition.file <- file.path(part.folder,paste0(comdet.name,"-membership.txt"))
			if(!file.exists(partition.file))
				tlog("........Partition file ",partition.file," not found")
			else
				partitions[[comdet.name]] <- as.matrix(read.table(partition.file))
		}
		for(comdet.name in comdet.algos)
		{	neg.algo.name <- comdet.algo.ncg.value(comdet.name)
			tlog("........Loading partition files for algorithm ",neg.algo.name)
			partition.file <- file.path(part.folder,paste0(neg.algo.name,"-membership.txt"))
			if(!file.exists(partition.file))
				tlog("........Partition file ",partition.file," not found")
			else
				partitions[[neg.algo.name]] <- as.matrix(read.table(partition.file))
		}
		
		# check that we have at least two partitions to compare
		if(length(partitions)>1)
		{	# init iteration matrices
			mats <- list() 
			for(meas in measures)
			{	m <- matrix(NA, nrow=length(partitions), ncol=length(partitions))
				rownames(m) <- names(partitions)
				colnames(m) <- names(partitions)
				mats[[meas]] <- m
			}
		
			# compare partitions
			for(i in 1:(length(partitions)-1))
			{	#print(i);print(length(partitions))
				partition1 <- partitions[[i]]
				for(j in (i+1):length(partitions))
				{	tlog("........Processing ",names(partitions)[i]," vs ",names(partitions)[j])
					partition2 <- partitions[[j]]
					vals <- compare.partition.pair(partition1, partition2, measures)
					#print(vals)
					if(any(is.nan(vals)))
						tlog("..........WARNING: some measures returned NaN, which will appear as NA in the recorded file")
					for(meas in measures)
					{	mats[[meas]][i,j] <- vals[meas]
						mats[[meas]][j,i] <- vals[meas]
					}
				}
			}
		
			# record iteration matrices
			for(meas in measures)
			{	table.file <- file.path(part.folder,paste0("comparison-",meas,".csv"))
				write.csv2(mats[[meas]], file=table.file, row.names=TRUE)
			}
		
			# update the list used to average
			if(repetitions>1)
			{	for(meas in measures)
					avg.list[[meas]][[r]] <- mats[[meas]]
			}
		}
	}
	
	# record the average tables
	if(repetitions>1)
	{	part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=date, repetition=NA)
		for(meas in measures)
		{	if(length(avg.list[[meas]])>0)
			{	tmp <- average.matrix.list(avg.list[[meas]])
				table.file <- file.path(part.folder,paste0("comparison-mean-",meas,".csv"))
				write.csv2(tmp$avg, file=table.file, row.names=TRUE)
				table.file <- file.path(part.folder,paste0("comparison-stdev-",meas,".csv"))
				write.csv2(tmp$stdev, file=table.file, row.names=TRUE)
			}
		}
	}
}


#############################################################################################
# Compare the partitions for the specified partitioning algorithms, for all possible networks, 
# for all time periods and domains, for the specified thresholds and agreement scores. 
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# measures: vector of comparison measures to process.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
compare.partitions <- function(thresh=NA, score.file, domains, dates, country, group, comdet.algos, corclu.algos, measures, repetitions)
{	# consider each domain individually (including all domains at once)
#	for(dom in domains)
	foreach(dom=domains) %dopar%
	{	source("src/define-imports.R")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	
			#tlog("......Process performance measures for domain ",dom," and period ",DATE.STR.T7[date])
			
			# setup graph folder
			#filtered.folder <- paste0(folder,"/",score.file,
			#		"/","negtr=",thresh[1],"-postr=",thresh[2],
			#		"/",dom,"/",DATE.STR.T7[date],
			#		"/")
			
			# compare algorithm performances
			tlog("......Compare partitioning algorithm performances")
			compare.partitions.measures(thresh, score.file, domain=dom, date, country, group, comdet.algos, corclu.algos, measures, repetitions)
		}
	}
}


#############################################################################################
# Compares the partitions for the specified partitioning algorithms, for all networks, for 
# the whole dataset, by country and by political group, for the specified thresholds and 
# agreement scores. 
#
# mep.details: description of each MEP.
# thresh: thresholds used for network extraction (vector of two values).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# measures: vector of comparison measures to process.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
compare.all.partitions <- function(mep.details, thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, measures, repetitions)
{	tlog("***************************************************")
	tlog("****** COMPARING PARTITIONS")
	tlog("***************************************************")
	
	# networks by political group
	tlog("..Compare performance measures by group")
	for(group in groups)
	{	tlog("....Compare performance measures for group ",group)
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# extract networks
		compare.partitions(thresh, score.file, domains, dates, country=NA, group, comdet.algos, corclu.algos, measures, repetitions)
	}
	
	# networks by home country
	tlog("..Compare performance measures by country")
	for(country in countries)
	{	tlog("....Compare performance measures for country ",country)
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# extract networks
		compare.partitions(thresh, score.file, domains, dates, country, group=NA, comdet.algos, corclu.algos, measures, repetitions)
	}
	
	# extract networks for all data
	if(everything)
	{	tlog("..Compare performance measures for all data")
		compare.partitions(thresh, score.file, domains, dates, country=NA, group=NA, comdet.algos, corclu.algos, measures, repetitions)
	}
}
