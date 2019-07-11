
# source("src/define-constants.R")
source("src/build-networks/process-agreement.R")
source("src/build-networks/extract-networks.R")



#############################################################################################
# It performs aggregation of roll-calls and then builds aggregated networks for each cluster detected in clustering process, by
# specifying aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the network extraction process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# folder: output folder path.
#		  For instance: "out/rollcall-clustering/m3/bycountry/France/AGRI/2012-13/votetypes=FAA/F.purity-k=2-sil=0.52106/graphType=signed/"
# votes: individual vote data, i.e. how each MEP voted.
# rollcall.details: description of each voted roll-call.
# mep.details: description of each MEP.
# clu.mbrshp: the obtained partition information in the clustering process.
# rollcall.ids: the corresponding roll-call ids from which the graph is built
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension). The same as the variable 'aggrega.graph.type'.
# rollcall.clu.tresh: Threshold used in filtering step. Value (0,0) means no thresholding at all.
#					   It aims at removing low-weighted links in order to obtain a less dense graph.
# domain: political domain to consider when processing the data.
# graph.name: description of the created graph
# plot.formats: formats of the plot files.
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.
#
#############################################################################################
aggregate.rollcall.networks = function(folder, votes, rollcall.details, mep.details, clu.mbrshp, rollcall.ids, score.file,
		rollcall.clu.tresh, domain, graph.name, plot.formats, absence.thresholds=NA){
		
	# load the agreement scores
	score.table <- load.score.table(score.file)
	# print(score.table)
	
	g.name = NA
	if(startsWith(score.file, "signed"))
		g.name = SIGNED.FILE
	if(startsWith(score.file, "unsigned"))
		g.name = UNSIGNED.FILE
		
	
	nb.clu = length(unique(clu.mbrshp))
	for(i in 1:nb.clu){
		folder.clu.no = file.path(folder, paste0("clu", i))
		dir.create(folder.clu.no, recursive=TRUE, showWarnings=FALSE)
#		folder.clu.no.networks = paste0(folder.clu.no, "/networks")
#		dir.create(folder.clu.no.networks, recursive=TRUE, showWarnings=FALSE)
		
		indx = which(clu.mbrshp == i)
		curr.rollcall.ids = rollcall.ids[indx]
		cols <- match(curr.rollcall.ids, colnames(votes))
		curr.votes <- votes[,cols]
		tlog("nb rollcalls: ", length(curr.rollcall.ids))
#		curr.votes = votes[,curr.rollcall.ids]
		
		
		# when length(curr.rollcall.ids) = 1, the matrix turns into vector.
		# But, the 'process.agreement.index' method needs a matrix for 'votes'
		if(length(curr.rollcall.ids) == 1){
			curr.votes = as.matrix(curr.votes)
			colnames(curr.votes) = curr.rollcall.ids
		}
		# record the vote details into file
		write.csv(file=file.path(folder.clu.no,"votes.csv"), x=curr.votes, row.names=TRUE)

		# =============================================================================
		absences = get.meps.absences(curr.votes)
		abstentions = get.meps.abstentions(curr.votes)
		# record the absence and abstention details into file
		df.abs = cbind(mep.details[,c(COL.MEPID,COL.FULLNAME)], absences)
		df.abst = cbind(mep.details[,c(COL.MEPID,COL.FULLNAME)], abstentions)
		write.csv(file=file.path(folder.clu.no,"mep-absences.csv"), x=df.abs, row.names=TRUE)
		write.csv(file=file.path(folder.clu.no,"mep-abstentions.csv"), x=df.abst, row.names=TRUE)
		# =============================================================================
		
		agreement = process.agreement.index(curr.votes, score.table)
		
		
#		theme.ids.as.list = retreive.rollcall.theme.ids(rollcall.details, curr.rollcall.ids)
#		theme.ids = unlist(theme.ids.as.list)
#		themes = retreive.theme.names.by.id(domain, theme.ids)
#		
#		# ====================================
#		g.name = paste0(graph.name, "\nTotal number of vote roll-calls: ",length(curr.rollcall.ids))		
#		# **********
#		g.name = paste0(g.name, "\nTotal number of themes: ",length(unique(as.character(themes))), "\n")
#		LINE.LIMIT = 4
#		freqs = table(as.character(themes)) # turn factor into vector
#		nb.theme=length(freqs)
#		nb.line = ceiling(nb.theme/LINE.LIMIT)
#		for(i in 1:nb.line){
#			range=seq((i-1)*LINE.LIMIT+1,  min(i*LINE.LIMIT, nb.theme))
#			g.name = paste0(g.name, paste(names(freqs)[range]," (total=",freqs[range],")",collapse=", "), "\n")
#		}
#		# ====================================
		
		
		
		# =============================================================================================
		# if absence threshold is not specified, do not filter, take all the MEPs (i.e; do not show them as absent)
		if(any(is.na(absence.thresholds)))
			absence.thresholds = c(1)
		
		for(abs.thresh in absence.thresholds){
			# cat("abs.thresh:", abs.thresh, "\n")
			indx = which(absences > abs.thresh)
			# print(indx)
			agreement2 = agreement
			
			
			agreement2[,indx]=0 # remove links going to those nodes
			agreement2[indx,]=0 # remove links going from those nodes
			# add "as.matrix" in case of length(indx)=1 and therefore it becomes a single value, not matrix
			if(length(indx)==1)
				agreement2[indx,indx] = 1 # make diagonal values 1
			else
				diag(agreement2[indx,indx] )= 1 # make diagonal values 1
			
			folder.clu.no.networks = file.path(folder.clu.no, paste0("networks-with-","meps-absence-thresh=",abs.thresh))
			# For instance: 'out/rollcall-clustering/m3/bycountry/France/AGRI/2012-13/votetypes=FAA/F.purity-k=2-sil=0.52106/graphType=signed/clu1/meps-absence-thresh=0.5/networks/'
			dir.create(folder.clu.no.networks, recursive=TRUE, showWarnings=FALSE)
			
			rownames(agreement2) = mep.details[,COL.MEPID]
			extract.network(agreement2, mep.details, absences, abstentions, rollcall.clu.tresh, folder.clu.no.networks, g.name, plot.formats)
		}
		# =============================================================================================
		
	}
}



#############################################################################################
# This method is called after the roll-call clustering process.
# It performs aggregation of roll-calls and then builds aggregated networks for all time periods and domains,  by
# specifying aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the network extraction process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# all.votes: individual vote data, i.e. how each MEP voted.
# rollcall.details: description of each voted roll-call.
# mep.details: description of each MEP.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# clu.algo.name: the method to be used for clsutering process. KMEDOIDS is the only option.
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
# plot.formats: formats of the plot files.
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.#
#############################################################################################
aggregate.rollcall.networks.by.cluster <- function(all.votes, rollcall.details, mep.details, score.file, clu.algo.name, 
		domains, dates, country, group, measures, cons.vote.types, epsilon, k.limits, aggrega.graph.type, plot.formats, absence.thresholds)
{	
	
	for(dom in domains)
	{	#source("src/define-imports.R")
		
		# init
		partitions.term = list()
		out.domain.folder <- get.rollcall.clustering.path(score=score.file,country,group,domain=dom)		
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog("....roll-call clustering for domain ",dom," and period ",DATE.STR.T7[date])
			
			par.folder <- get.rollcall.partitions.path(score=score.file,country,group,domain=dom)
			par.folder <- paste0(par.folder,"/",DATE.STR.T7[date])
			out.date.folder <- paste0(out.domain.folder,"/",DATE.STR.T7[date])
			
			
			
			# =========================
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.rollcall.ids <- filter.rollcalls.by.date.and.domain(rollcall.details, 
					start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
					domains=domval)
			# =========================
			
			
			# check if there's enough data remaining
			if(length(filtered.rollcall.ids)>1)
			{	# format data
				cols <- match(filtered.rollcall.ids, colnames(all.votes))
				active.idx <- which(apply(all.votes[,cols],1,function(v) !all(is.na(v))))
				if(length(active.idx)>1)
				{	votes <- all.votes[active.idx,cols]
					colnames(votes) = filtered.rollcall.ids
					mep.details2 = mep.details[active.idx,] # for example: some meps quits after 2010-11. So need to update
					
					
					for(measure in measures){
						
						# =================================================================================================
						
						# take the first letter of vote types => "F" for FOR, "A" for "*AGAINST"
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

						
						for(i in 1:length(k.vals)){
							sil.val = sil.vals[i]
							k.val = k.vals[i]
							
							folder.sil.path = get.rollcall.clustering.vote.type.silhouette.val.path(
									score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, DATE.STR.T7[date])
							mem.file <- file.path(folder.sil.path, paste0(clu.algo.name,"-membership.txt"))
							
							
							mbrshp = read.table(file=mem.file)$V1
							file.desc = prepare.descs.for.silhouette.partitions(measure,cons.vote.types.desc,sil.val, k.val)
							
							
							# ===========================
							if(is.na(country))
								if(is.na(group))
									mode.str <- ""
								else
									mode.str <- paste0(" - group=",group)
							else
								mode.str <- paste0(" - country=",country)
							base.graph.name <- paste0("MEP agreement - score=",score.file,mode.str)
							graph.name <- paste0(base.graph.name," - domain=",dom," - period=",DATE.STR.T7[date],"\n",file.desc)
							# ===========================
	
							folder.clu.path = get.rollcall.clustering.vote.type.silhouette.val.path(
									score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, DATE.STR.T7[date])
	
							ROLLCALL.CLU.TRESH = c(0,0) # do not apply filtering when extracting network
							# ROLLCALL.CLU.TRESH = c(NA,NA) # apply filtering when extracting network
							aggregate.rollcall.networks(folder.clu.path, votes, rollcall.details, mep.details2, mbrshp, as.integer(filtered.rollcall.ids),
									paste0(aggrega.graph.type,"-",cons.vote.types.desc), ROLLCALL.CLU.TRESH, dom, graph.name, plot.formats, absence.thresholds)
							
						}
						# =================================================================================================
						
					}
				}
			}
			
			
			
		} # end for Dates
		
		
	} # end for Domains
}




#############################################################################################
# This method is called after the roll-call clustering process.
# It performs aggregation of roll-calls and then builds aggregated networks for the whole dataset, by country and by political group, for the 
# specified aggregation graph type (signed or unsigned) and absence threshold(s).
#  'epsilon' and 'k.limits' are the additional parameters to restrain the network extraction process 
#  for only some 'k' values (so, not all possible 'k' values).
#
#
# all.votes: individual vote data, i.e. how each MEP voted.
# rollcall.details: description of each voted roll-call.
# mep.details: description of each MEP.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# clu.algo.name: the method to be used for clsutering process. KMEDOIDS is the only option.
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
# plot.formats: formats of the plot files.
# absence.thresholds: Normalized value (between 0 and 1). The frequency threshold to be used to eliminate the MEPs which are frequently absent.
#						when it takes 1, it does not apply any filtering, it takes all the MEPs
#						when it takes 0, it removes the MEPs who are absent at least once.
#
#############################################################################################
aggregate.all.rollcall.networks.by.cluster <- function(all.votes, rollcall.details, mep.details, score.file, clu.algo.name, domains, dates, everything, countries, groups, measures, 
		cons.vote.types, epsilon, k.limits, aggrega.graph.type, plot.formats, absence.thresholds)
{	tlog("***************************************************")
	tlog("****** AGGREGATE ALL ROLL-CALL NETWORKS BY CLUSTER ******")
	tlog("****** score file for aggregation: ", aggrega.graph.type," ******")
	tlog("***************************************************")
	
	# process network extraction by political group
	tlog("..Process roll-call clustering by group")
	for(group in groups)
	{	tlog("....Process roll-call clustering for group ",group)
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.mep.details <- mep.details[idx,]
		mep.ids <- as.integer(grp.mep.details[,COL.MEPID])
		votes = all.votes[mep.ids,]
		rownames(votes) = mep.ids
		
		aggregate.rollcall.networks.by.cluster(votes, rollcall.details, grp.mep.details, score.file, clu.algo.name, domains, dates, country=NA, group, measures,
				cons.vote.types, epsilon, k.limits, aggrega.graph.type, plot.formats, absence.thresholds)
	}
	
	# process agreement by home country
	tlog("..Process roll-call clustering by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process roll-call clustering for country ",country)
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.mep.details <- mep.details[idx,]
		mep.ids <- as.integer(cntr.mep.details[,COL.MEPID])
		votes = all.votes[mep.ids,]
		rownames(votes) = mep.ids
		
		aggregate.rollcall.networks.by.cluster(votes, rollcall.details, cntr.mep.details, score.file, clu.algo.name, domains, dates, country, group=NA, measures,
				cons.vote.types, epsilon, k.limits, aggrega.graph.type, plot.formats, absence.thresholds)
	}
	
	# process for all data
	if(everything)
	{	tlog("..Process roll-call clustering for all data")
		aggregate.rollcall.networks.by.cluster(all.votes, rollcall.details, mep.details, score.file, clu.algo.name, domains, dates, country=NA, group=NA, measures,
				cons.vote.types, epsilon, k.limits, aggrega.graph.type, plot.formats, absence.thresholds)
	}
}
