


#############################################################################################
# 
#############################################################################################
process.partitioning.commun = function(g, algo.name, absences, abstentions, folder.clu.no.partitions, folder.clu.no.networks,
		country, group, plot.formats, force=FALSE)
{
	# ===================================================================================================
	# this methods also plots the current result with graph (through Circos)
	apply.partitioning.algorithm(g, algo.name, absences, abstentions, folder.clu.no.partitions, folder.clu.no.networks,
			country, group, plot.formats, force=FALSE)
	# ===================================================================================================
	
	f.path = file.path(folder.clu.no.partitions,paste0(algo.name,"-",g$type,"-membership.txt"))
	mbrshp = read.table(file=f.path)$V1	
	g <- set.vertex.attribute(graph=g, name=algo.name, value=mbrshp)
	
	# record graphs (Graphml only) with detected communities, in the partition folder (not the network one)
	f.path=file.path(folder.clu.no.partitions,paste0(g$type,".graphml"))
	write.graph(graph=g, file=f.path, format="graphml")
	
	# at the end, convert graphml file into gephi-graphml file
	# What is specific in the format of gephi-graphml is not using negative edge signs
	# So, we remove negative edge signs and add them as a new edge atrribute in the graphml file
	f.gephi.path=file.path(folder.clu.no.partitions,paste0(g$type,"-gephi.graphml"))
	newGraphDoc = addSignAttrIntoGraphmlFiles(f.path)
	saveXML(newGraphDoc, file=f.gephi.path)			
}






#############################################################################################
# It partitions all aggregated networks for a given roll-call clustering result (e.g. 3-medoids).
#  If there are 3 clsuters, it will partition 3 aggregated networks. The resulting partitions
#  are called 'characteristic voting patterns'.
#
#
# folder: output folder path.
#		  For instance: "out/rollcall-clustering/m3/bycountry/France/AGRI/2012-13/votetypes=FAA/F.purity-k=3-sil=0.65115/graphType=signed/clu1"
# corclst.algos: partitioning methods to be applied on the extracted signed aggregated networks.
# comdet.algos: partitioning methods to be applied on the extracted unsigned aggregated networks.
# nb.clu.in.rollcall.clu: number of detected cluster in roll-call clustering process (e.g. 3 for 3-medoids)
# plot.formats: formats of the plot files.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.
#
#############################################################################################
perform.partitioning.aggregated.rollcall.network = function(folder, aggrega.graph.type, corclst.algos, comdet.algos,
		nb.clu.in.rollcall.clu, plot.formats, country, group, absence.thresholds=NA)
{
	# if absence threshold is not specified, do not filter, take all the MEPs
	if(any(is.na(absence.thresholds)))
		absence.thresholds = c(1)

	# for each cluster detected in roll-call clustering process (e.g. processing 3 clusters in 3-medoids)
	for(i in 1:nb.clu.in.rollcall.clu){
		# record cluster specific results into subfolders
		folder.clu.no = file.path(folder, paste0("clu", i)) # (e.g. will be 'clu1', 'clu2' and 'clu3' for 3-medoids)
		
		# for each threshold value of MEP's absence
		for(abs.thresh in absence.thresholds){
			# record results into a different subfolder for each value of absence threshold
			folder.clu.no.networks = file.path(folder.clu.no, paste0("networks-with-","meps-absence-thresh=",abs.thresh))
			folder.clu.no.partitions = file.path(folder.clu.no, paste0(aggrega.graph.type,"-partitions-with-","meps-absence-thresh=",abs.thresh))
			dir.create(folder.clu.no.partitions, recursive=TRUE, showWarnings=FALSE)
			
			votes=read.table(file=file.path(folder.clu.no, "votes.csv"), header=TRUE,row.names=1,check.names=FALSE,sep=",")
			if(!is.matrix(votes)) # in case that there is 1 vote
				votes = as.matrix(votes) 
			absences = get.meps.absences(votes)
			abstentions = get.meps.abstentions(votes)
			
			# if the created aggregated roll-call network is signed version
			if(aggrega.graph.type == "signed"){
				graph.file <- file.path(folder.clu.no.networks,paste0(SIGNED.FILE,".graphml"))	
				g <- suppressWarnings(read.graph(file=graph.file, format="graphml"))
				g$type = SIGNED.FILE
				
				# apply selected signed graph partitioning algorithms designed for the Correlation Clustering problem
				for(algo.name in corclst.algos){
					process.partitioning.commun(g, algo.name, absences, abstentions, folder.clu.no.partitions,
							folder.clu.no.networks, country, group, plot.formats, force)
				}
				
				# apply selected signed graph partitioning algorithms designed for the Modularity (i.e. community detection)
				for(algo.name in comdet.algos){
					process.partitioning.commun(g, algo.name, absences, abstentions, folder.clu.no.partitions,
							folder.clu.no.networks, country, group, plot.formats, force)
				}
				
			} else if(aggrega.graph.type == "unsigned") {	
				# ---
				# unsigned graph
				graph.file <- file.path(folder.clu.no.networks,paste0(UNSIGNED.FILE,".graphml"))
				g <- suppressWarnings(read.graph(file=graph.file, format="graphml"))
				g$type = UNSIGNED.FILE
				
				# apply selected unsigned graph partitioning algorithms designed for the Modularity (i.e. community detection)
				for(algo.name in comdet.algos){
					process.partitioning.commun(g, algo.name, absences, abstentions, folder.clu.no.partitions,
							folder.clu.no.networks, country, group, plot.formats, force)
				}
				
				# ---
				# positive part of the signed graph
				graph.file <- file.path(folder.clu.no.networks,paste0(POSITIVE.FILE,".graphml"))
				g <- suppressWarnings(read.graph(file=graph.file, format="graphml"))
				g$type = POSITIVE.FILE
				
				# apply selected unsigned graph partitioning algorithms designed for the Modularity (i.e. community detection)
				for(algo.name in comdet.algos){
					process.partitioning.commun(g, algo.name, absences, abstentions, folder.clu.no.partitions,
							folder.clu.no.networks, country, group, plot.formats, force)
				}
			}
			
		}
	}

}





#############################################################################################
# It partitions all aggregated networks for all similarity measures and all roll-call clustering results (e.g. 2-medoids, 3-medoids),
#  by specifying aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the partitioning process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# corclst.algos: partitioning methods to be applied on the extracted signed aggregated networks.
# comdet.algos: partitioning methods to be applied on the extracted unsigned aggregated networks.
# dom: political domain to consider when processing the data.
# date: time period to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# plot.formats: formats of the plot files.
# measures: similarity measures in order to calcualte similarity matrix
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# aggrega.graph.type: graph type to be used in aggregation process. 2 possibilities: signed graph or unsigned graph.
#						When signed graph is chosen, vote agreement index which also takes into account negative weights is used.
#						For instance, -1 is obtained when For-Against or For-Abstain occurs. On the other hand, when graph type is
#						unsigned, vote agreement index which takes into account only positive weights is used (e.g. FOR-FOR, etc.). 
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.
#
#############################################################################################
perform.partitioning.aggregated.rollcall.networks <- function(score.file, corclst.algos, comdet.algos, dom, date, country, group,
		plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
{
	
	# for each similarty measure used for creating similarty matrix in roll-call clustering process
	for(measure in measures){		
		# take the first letter of vote types. For isntance:  "F" for FOR, "A" for "AGAINST"
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
		
		# for each roll-call clustering result (e.g. 2-medoids, 3-medoids, etc.)
		for(i in 1:length(sil.vals)){
			sil.val = sil.vals[i]
			k.val = k.vals[i]
			
			folder.clu.path = get.rollcall.clustering.vote.type.silhouette.val.path(
					score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, DATE.STR.T7[date])
	
			perform.partitioning.aggregated.rollcall.network(folder.clu.path, aggrega.graph.type, corclst.algos, comdet.algos, 
					k.val, plot.formats, country, group, absence.thresholds)
		}
		
	}
	
	
}





#############################################################################################
# It partitions all aggregated networks for all time periods and domains,
# by specifying aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the partitioning process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# corclst.algos: partitioning methods to be applied on the extracted signed aggregated networks.
# comdet.algos: partitioning methods to be applied on the extracted unsigned aggregated networks.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# plot.formats: formats of the plot files.
# measures: similarity measures in order to calcualte similarity matrix
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# aggrega.graph.type: graph type to be used in aggregation process. 2 possibilities: signed graph or unsigned graph.
#						When signed graph is chosen, vote agreement index which also takes into account negative weights is used.
#						For instance, -1 is obtained when For-Against or For-Abstain occurs. On the other hand, when graph type is
#						unsigned, vote agreement index which takes into account only positive weights is used (e.g. FOR-FOR, etc.). 
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.
#
#############################################################################################
partition.aggregated.rollcall.networks <- function(score.file, corclst.algos, comdet.algos, domains, dates, country, group,
		plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
{	# consider each domain individually (including all domains at once)
		
	for(dom in domains)
#	foreach(dom=domains) %dopar%
	{	#source("src/define-imports.R")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog(6,"Detect communities for domain ",dom," and period ",DATE.STR.T7[date])
					
			# perform community detection
			perform.partitioning.aggregated.rollcall.networks(score.file, corclst.algos, comdet.algos, dom, date, country, group,
					plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)

		}
	}
}





#############################################################################################
# It partitions all aggregated networks for the whole dataset, by country and by political group, for the 
# specified aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the partitioning process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# corclst.algos: partitioning methods to be applied on the extracted signed aggregated networks.
# comdet.algos: partitioning methods to be applied on the extracted unsigned aggregated networks.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# plot.formats: formats of the plot files.
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
partition.all.aggregated.rollcall.networks <- function(score.file, corclst.algos, comdet.algos, domains, dates, everything, countries, groups,
		plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
{	tlog("***************************************************")
	tlog("****** PARTITIONING ALL AGGREGATED ROLL-CALL NETWORKS")
	tlog("***************************************************")
	
	# networks by political group
	tlog(2,"Detect communities by group")
	for(group in groups)
	{	tlog("....Detect communities for group ",group)
		
		# extract networks
		partition.aggregated.rollcall.networks(score.file, corclst.algos, comdet.algos, domains, dates, country=NA, group,
				plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
	}
	
	# networks by home country
	tlog(2,"Detect communities by country")
	for(country in countries)
	{	tlog(4,"Detect communities for country ",country)
		
		print("**************")
		# extract networks
		partition.aggregated.rollcall.networks(score.file, corclst.algos, comdet.algos, domains, dates, country, group=NA,
				 plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
	}
	
	# extract networks for all data
	if(everything)
	{	tlog(2,"Detect communities for all data")
		partition.aggregated.rollcall.networks(score.file, corclst.algos, comdet.algos, domains, dates, country=NA, group=NA,
				 plot.formats, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, absence.thresholds)
	}
}
