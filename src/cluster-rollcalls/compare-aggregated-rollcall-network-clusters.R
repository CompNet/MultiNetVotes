
# source("src/define-constants.R")
source("src/partition-networks/compare-clusters.R")

library(cluster)



#############################################################################################
# It compares the partitions obtained for the aggregated networks for a given roll-call clustering result (e.g. 3-medoids).
#  And it records the similarity scores into a file.
#  If there are 3 clusters, it will partition 3 aggregated networks. The resulting partitions
#  are called 'characteristic voting patterns'.
#
#
# folder: output folder path.
#		  For instance: "out/rollcall-clustering/m3/bycountry/France/AGRI/2012-13/votetypes=FAA/F.purity-k=3-sil=0.65115/graphType=signed/clu1"
# algo.name: partitioning method applied on the extracted aggregated networks.
#							If extracted network is signed, then the appropriate methods should be used.
# nb.clu.in.rollcall.clu: number of detected cluster in roll-call clustering process (e.g. 3 for 3-medoids)
# measure: similarity measure
# desc: a description string containing silhouette score, k value, etc.
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.
#
#############################################################################################
compare.aggregated.rollcall.network.by.cluster = function(folder, algo.name, nb.clu.in.rollcall.clu, measure, desc, absence.thresholds)
{
	
	# if absence threshold is not specified, do not filter, take all the MEPs
	if(any(is.na(absence.thresholds)))
		absence.thresholds = c(1)
	
	partitions = list()
	counter=0
	for(i in 1:nb.clu.in.rollcall.clu){
		folder.clu.no = paste0(folder, "/clu", i)
		
		for(j in 1:absence.thresholds){
			abs.thresh = absence.thresholds[j]
			folder.clu.no.networks = file.path(folder.clu.no, paste0("networks-with-","meps-absence-thresh=",abs.thresh))
			folder.clu.no.partitions = file.path(folder.clu.no, paste0(aggrega.graph.type,"-partitions-with-","meps-absence-thresh=",abs.thresh))
			
			counter=counter+1
			print(counter)
			mem.file = file.path(folder.clu.no.partitions, paste0(algo.name,"-membership.txt"))
			partitions[[counter]] = read.table(mem.file)$V1
			names(partitions)[counter] = paste(desc," ","abs.thresh=",abs.thresh," ","clu=",i, sep="")
		}	
	}
	print(partitions)
	res.mtrx = create.similarity.matrix(partitions, measure)
	#print(file.path(folder,paste0(algo.name,"-comparison-aggregated-network-clusters.csv")))
	write.csv(file=file.path(folder,paste0(SIM.MATRIX.FILE.PREFIX, "-for-aggregated-network-partitions",
							"_algo=", algo.name,"_measure=",measure,".csv")), x=res.mtrx)
}




#############################################################################################
# It compares all aggregated networks with detected partitions (i.e. characteristic voting patterns)
#  for all similarity measures and all roll-call clustering results (e.g. 2-medoids, 3-medoids),
#  by specifying aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the partitioning process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# part.algo.names: partitioning methods to be applied on the extracted aggregated networks.
#							If extracted network is signed, then the appropriate methods should be used.
# dom: political domain to consider when processing the data.
# date: time period to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# measures: similarity measures in order to calcualte similarity matrix
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# aggrega.graph.type: graph type to be used in aggregation process. 2 possibilities: signed graph or unsigned graph.
#						When signed graph is chosen, vote agreement index which also takes into account negative weights is used.
#						For instance, -1 is obtained when For-Against or For-Abstain occurs. On the other hand, when graph type is
#						unsigned, vote agreement index which takes into account only positive weights is used (e.g. FOR-FOR, etc.). 
# absence.thresholds: the frequency threshold to be used to eliminate the MEPs which are frequently absent.
#
#############################################################################################
compare.aggregated.rollcall.network.clusters.by.measure.and.silhouatte.val <- function(score.file, corclst.algos, comdet.algos, dom, date, country, group,
		 measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
{
	# for each similarty measure used for creating similarty matrix in roll-call clustering process
	for(measure in measures){
				
		# take the first letter of vote types. For instance: "F" for FOR, "A" for "AGAINST"
		list.first.letters = sapply(cons.vote.types, function(x) unlist(strsplit(x, split=""))[1])
		cons.vote.types.desc = paste(list.first.letters, collapse="")
		
		
		folder.path = get.rollcall.clustering.vote.type.path(
				score.file, cons.vote.types.desc, country, group, dom, DATE.STR.T7[date])						
		silhouette.scores.file = file.path(folder.path,paste0(SILH.SCORES.FILE.PREFIX,"_measure=",measure,".csv"))
		
		rollcall.clu.result.per.k = read.csv(file=silhouette.scores.file)
		all.sil.vals = rollcall.clu.result.per.k[,COL.AVG.SILH]
		all.k.vals = rollcall.clu.result.per.k[,COL.K]
		
		indx = filter.rollcall.clu.result.by.epsilon.and.k(rollcall.clu.result.per.k, epsilon, k.limits)
		sil.vals = all.sil.vals[indx]
		k.vals = as.character(all.k.vals[indx])
		
		descs = prepare.descs.for.silhouette.partitions(measure, NA, sil.vals, NA)
		
		# for each roll-call clustering result (e.g. 2-medoids, 3-medoids, etc.)
		for(i in 1:length(sil.vals)){
			sil.val = sil.vals[i]
			k.val = k.vals[i]
			desc = descs[i]
					
			folder.clu.path = get.rollcall.clustering.vote.type.silhouette.val.path(
					score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, DATE.STR.T7[date])
	
			if(aggrega.graph.type == "signed"){
				
				for(algo.name in corclst.algos){
					compare.aggregated.rollcall.network.by.cluster(folder.clu.path, paste0(algo.name,"-",SIGNED.FILE),
							k.val, measure, desc, absence.thresholds)	
				}
				
				for(algo.name in comdet.algos){
					compare.aggregated.rollcall.network.by.cluster(folder.clu.path, paste0(algo.name,"-",SIGNED.FILE),
							k.val, measure, desc, absence.thresholds)		
				}
				
			} else if(aggrega.graph.type == "unsigned") {	
				
				for(algo.name in comdet.algos){
					print(UNSIGNED.FILE)
					compare.aggregated.rollcall.network.by.cluster(folder.clu.path, paste0(algo.name,"-",UNSIGNED.FILE), 
							k.val, measure, desc, absence.thresholds)
					
					print(POSITIVE.FILE)
					compare.aggregated.rollcall.network.by.cluster(folder.clu.path, paste0(algo.name,"-",POSITIVE.FILE), 
							k.val, measure, desc, absence.thresholds)
				}
			}

			
		}
		
	}
	
	
}




#############################################################################################
# It compares all aggregated networks with detected partitions (i.e. characteristic voting patterns)
#  for all time periods and domains, by specifying aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the partitioning process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# part.algo.names: partitioning methods to be applied on the extracted aggregated networks.
#							If extracted network is signed, then the appropriate methods should be used.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# measures: similarity measures in order to calcualte similarity matrix
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# aggrega.graph.type: graph type to be used in aggregation process. 2 possibilities: signed graph or unsigned graph.
#						When signed graph is chosen, vote agreement index which also takes into account negative weights is used.
#						For instance, -1 is obtained when For-Against or For-Abstain occurs. On the other hand, when graph type is
#						unsigned, vote agreement index which takes into account only positive weights is used (e.g. FOR-FOR, etc.). 
# absence.thresholds: the frequency threshold to be used to eliminate the MEPs which are frequently absent.
#
#############################################################################################
compare.aggregated.rollcall.network.clusters <- function(score.file, corclst.algos, comdet.algos, domains, dates, country, group,
		measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog(6,"Detect communities for domain ",dom," and period ",DATE.STR.T7[date])
			
			# perform comparison
			compare.aggregated.rollcall.network.clusters.by.measure.and.silhouatte.val(score.file, corclst.algos, comdet.algos, dom, date, country, group,
					measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
		}
	}
}





#############################################################################################
# It compares all aggregated networks with detected partitions (i.e. characteristic voting patterns)
#  for the whole dataset, by country and by political group, for the 
#  specified aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the partitioning process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# part.algo.names: partitioning methods to be applied on the extracted aggregated networks.
#							If extracted network is signed, then the appropriate methods should be used.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# measures: similarity measures in order to calcualte similarity matrix
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# aggrega.graph.type: graph type to be used in aggregation process. 2 possibilities: signed graph or unsigned graph.
#						When signed graph is chosen, vote agreement index which also takes into account negative weights is used.
#						For instance, -1 is obtained when For-Against or For-Abstain occurs. On the other hand, when graph type is
#						unsigned, vote agreement index which takes into account only positive weights is used (e.g. FOR-FOR, etc.). 
# absence.thresholds: the frequency threshold to be used to eliminate the MEPs which are frequently absent.
#
#############################################################################################
compare.all.aggregated.rollcall.network.clusters <- function(score.file, corclst.algos, comdet.algos, domains, dates, everything, countries, groups,
		measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
{	tlog("***************************************************")
	tlog("****** COMPARING ALL AGGREGATED ROLL-CALL PARTITIONS")
	tlog("***************************************************")
	
	# networks by political group
	tlog(2,"Detect communities by group")
	for(group in groups)
	{	tlog("....Detect communities for group ",group)
		
		# extract networks
		compare.aggregated.rollcall.network.clusters(score.file, corclst.algos, comdet.algos, domains, dates, country=NA, group,
				measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
	}
	
	# networks by home country
	tlog(2,"Detect communities by country")
	for(country in countries)
	{	tlog(4,"Detect communities for country ",country)
		
		# extract networks
		compare.aggregated.rollcall.network.clusters(score.file, corclst.algos, comdet.algos, domains, dates, country, group=NA,
				 measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
	}
	
	# extract networks for all data
	if(everything)
	{	tlog(2,"Detect communities for all data")
		compare.aggregated.rollcall.network.clusters(score.file, corclst.algos, comdet.algos, domains, dates, country=NA, group=NA,
				measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
	}
}