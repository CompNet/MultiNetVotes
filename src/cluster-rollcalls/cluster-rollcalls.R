
library(cluster)


#############################################################################################
# It copies generated network images and regroups them by cluster.
#  It might be useful when assessing manually the quality of the used similarity measures.
#
# rollcall.part.folder: the folder path where the partitions of the roll-call networks are located
# sil.folder.path: the file path which stores the silhouette scores (not only silhouette, but also other information)
# output.details.df: roll-call details associated with the clustered roll-calls
# k.val: the number of detected cluster in roll-call clustering process (e.g. 3 when 3-medoids)
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
#
#############################################################################################
copy.rollcall.networks.by.cluster = function(rollcall.part.folder, sil.folder.path, output.details.df, k.val, cons.vote.types.desc)
{
	clu.mbrshp = output.details.df[,"clu.no"]
	rollcall.ids = output.details.df[,"rollcall.id"]
	
	for(c in 1:as.integer(k.val)){
		regrouped.indv.rollcall.folder = file.path(sil.folder.path,paste0("clu",c),"regrouped-indv-rollcalls")
		dir.create(regrouped.indv.rollcall.folder, recursive=TRUE, showWarnings=FALSE)
		
		indx = which(clu.mbrshp == c)
		curr.rollcall.ids = rollcall.ids[indx]
		
		# since the size of a jpg file is large, we prioritize jpg files for copying 
		curr.in.jpg.files = file.path(rollcall.part.folder, curr.rollcall.ids, paste0(cons.vote.types.desc,"-all_links.jpg"))
		curr.out.jpg.files = file.path(regrouped.indv.rollcall.folder, paste0(cons.vote.types.desc,"-all_links",curr.rollcall.ids,".jpg"))
		curr.in.svg.files = file.path(rollcall.part.folder, curr.rollcall.ids, paste0(cons.vote.types.desc,"-all_links.svg"))
		curr.out.svg.files = file.path(regrouped.indv.rollcall.folder, paste0(cons.vote.types.desc,"-all_links",curr.rollcall.ids,".svg"))
		if(length(curr.in.jpg.files)>0) {
			file.copy(from=curr.in.jpg.files, to=curr.out.jpg.files, overwrite=TRUE)
		}
		else if(length(curr.in.svg.files)>0) { # otherwise, we choose svg files for copying 
			file.copy(from=curr.in.svg.files, to=curr.out.svg.files, overwrite=TRUE)
		}

	}
}



#############################################################################################
# Retreives roll-call titles and some other details by cluster.
# This method does not order the titles by cluster.no, this is the same order of membership
#
# rollcall.details: description of each voted roll-call.
# filtered.rollcall.ids: roll-call ids associated with selected domain and time period
# mbrshp: membership vector associated to k-medoids clustering result
#
# returns a filtered data frame containing titles and some other details
#############################################################################################
retreive.rollcall.titles.by.cluster = function(rollcall.details, filtered.rollcall.ids, mbrshp){
	nb.clu = length(unique(mbrshp))

	res = c()
	for(k in 1:nb.clu){
		indx = which(mbrshp == k)
		curr.rollcall.ids = filtered.rollcall.ids[indx]
		
		titles = rollcall.details[curr.rollcall.ids, COL.TITLE]
		full.titles = rollcall.details[curr.rollcall.ids, COL.FULL.TITLE]
		#theme.ids = rollcall.details[curr.rollcall.ids, COL.THEME.ID]
		dates = rollcall.details[curr.rollcall.ids, COL.DATE]
		#is.amendment.list = rollcall.details[curr.rollcall.ids, COL.IS.AMEND]
		ep.references = rollcall.details[curr.rollcall.ids, COL.EP.REF]
		#eurovoc.list = rollcall.details[curr.rollcall.ids, COL.EUROVOC]
		#eurlex.dircode.list = rollcall.details[curr.rollcall.ids, "EurLexDirCode"]
		curr = cbind(clu.no=k, rollcall.id=curr.rollcall.ids, title=titles, 
				# is.amendment=is.amendment.list, theme.id=theme.ids, eurovoc=eurovoc.list,
				date=dates, ep.reference=ep.references, full.title=full.titles)
		res = rbind(res, curr)
	}
	
	return(res)
}




#############################################################################################
# It computes the purity score, for each 'k' value (e.g. 2-medoids, 3-medoids, etc.),
#  in order to assess the quality of clustering results in terms of pure themes. 
#
# partitions: partitions associated with all 'k' clustering results (e.g. partitions[1] corresponds to 2-medoids result)
# ground.truth: the reference clustering. In this context, it consists of theme ids associated with roll-calls
# measure: measure used for similarity matrix
# descs: a description string which combines measure, k value and silhouette score (e.g. 'F.purity-k=2-sil=0.52106')
#
#############################################################################################
process.purity = function(partitions, ground.truth, descs){

	res = matrix(NA,length(partitions),1)
	rownames(res) = descs
	colnames(res) = "purity"
	
	for(i in 1:length(partitions)){
		membrshp = partitions[[i]]
		val = compute.purity.for.partition.vs.multi.labeled.ground.truth (membrshp, ground.truth, purity.level="partition")
		res[i,1]=val
	}
	
	return(res)
}




#############################################################################################
# It computes the inverse purity score, for each 'k' value (e.g. 2-medoids, 3-medoids, etc.),
#  in order to assess the quality of clustering results in terms of pure themes. 
#
# partitions: partitions associated with all 'k' clustering results (e.g. partitions[1] corresponds to 2-medoids result)
# ground.truth: the reference clustering. In this context, it consists of theme ids associated with roll-calls
# measure: measure used for similarity matrix
# descs: a description string which combines measure, k value and silhouette score (e.g. 'F.purity-k=2-sil=0.52106')
#
#############################################################################################
process.purity.inv = function(partitions, ground.truth, descs){
	
	res = matrix(NA,length(partitions),1)
	rownames(res) = descs
	colnames(res) = "purity.inv"
	
	for(i in 1:length(partitions)){
		membrshp = partitions[[i]]
		val = compute.purity.for.multi.labeled.ground.truth.vs.partition (ground.truth, membrshp, purity.level="partition")
		res[i,1]=val
	}
	
	return(res)
}








#############################################################################################
# It finds the max silhouette score and retreive the corresponding partition 
#
# avg.sil.list: silhouette scores associated with all 'k' values 
# partitions: partitions with all 'k' values 
# mem.file: file path into which the best clustering result is written
#############################################################################################
retreive.and.record.best.clustering.result = function(avg.sil.list, partitions, mem.file){

	best.k = which.max(avg.sil.list)
	mbrshp = partitions[[best.k]]
	write.table(x=mbrshp, file=paste(mem.file,".txt",sep=""), row.names=FALSE, col.names=FALSE)
	
	return(mbrshp)
}


#############################################################################################
# It filters and then records the considered clustering results based on an epsilon value or a range of k values.
#
#
# clu.algo.name: the method to be used for clsutering process. KMEDOIDS is the only option.
# rollcall.clu.result.per.k: a matrix where columns correspond to average silhouette score and k value.
#								Each line corresponds to a single (k, silhouette score) pair.
# all.partitions: all partitions
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# measure: similarity measure in order to calcualte similarity matrix
# cons.vote.types.desc: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# domain: political domain to consider when processing the data.
# date: time period to consider when processing the data.
#
# returns filtered results containing clustering partitions, associated silhouette scores and k values
#############################################################################################
filter.and.record.clustering.results.by.epsilon.and.k = function(clu.algo.name, rollcall.clu.result.per.k, all.partitions, epsilon, k.limits,
		measure, cons.vote.types.desc, score.file, country, group, dom, date){

	res = list()
	all.sil.vals = as.numeric(rollcall.clu.result.per.k[,COL.AVG.SILH])
	all.k.vals = as.numeric(rollcall.clu.result.per.k[,COL.K])
	

	indx = filter.rollcall.clu.result.by.epsilon.and.k(rollcall.clu.result.per.k, epsilon, k.limits)
	if(length(indx)>0){
		sil.vals = all.sil.vals[indx]
		k.vals = all.k.vals[indx]
		partitions = all.partitions[indx]
		
	
		for(i in 1:length(k.vals)){
			k.val = k.vals[i]
			sil.val = sil.vals[i]
			mbrshp = partitions[[i]]
			
			folder.path = get.rollcall.clustering.vote.type.silhouette.val.path(
					score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, date)
			dir.create(folder.path, recursive=TRUE, showWarnings=FALSE)
			
			write.table(x=mbrshp, file=file.path(folder.path,paste0(clu.algo.name,"-membership.txt")), row.names=FALSE, col.names=FALSE)
		}
		
		res$partitions = partitions
		res$sil.vals = sil.vals
		res$k.vals = k.vals
	} else {
		res$partitions = c()
		res$sil.vals = c()
		res$k.vals = c()
	}
	
	return(res)
}




#############################################################################################
# It records and plots the calculated silhouette scores.
#
#
# silhouette.scores.file: the file path into wich silhouette scores is written
# avg.sil.list: average silhouette scores
#
# returns a matrix where columns correspond to average silhouette score and k value.
#			Each line corresponds to a single (k, silhouette score) pair.
#############################################################################################
record.and.plot.silhouette.scores = function(silhouette.scores.file, avg.sil.list, partitions){
	
	nb.k = length(avg.sil.list)
	partitions = partitions[-1]
	s.partitions = sapply(partitions, function(part) paste0(part, collapse=","))
	all.sil.vals = avg.sil.list[-1] # 1st index is for k=1 but we did not run silhouette for k=1, so NA was put
	all.k.vals = 2:nb.k
	
	rollcall.clu.result.per.k = cbind(all.k.vals, all.sil.vals, s.partitions)
	colnames(rollcall.clu.result.per.k) = c(COL.K, COL.AVG.SILH, COL.PARTITION)
	write.csv(file=paste0(silhouette.scores.file,".csv"), x=rollcall.clu.result.per.k, row.names=FALSE)

	pdf(file=paste(silhouette.scores.file,".pdf",sep=""),bg="white",compress=COMPRESS)
	plot(x=all.k.vals, y=all.sil.vals, xlab="k (number of clusters)", ylab="silhouette averages scores",
			main="Silhouette averages scores per k")
	dev.off()
	
	return(rollcall.clu.result.per.k)
}






#############################################################################################
# It converts the given similarity matrix into dissimilarity matrix, then apply the clustering method.
#  As clustering method, only k-medoids is used. At the end, the average silhouete score
#  is computed for each clustering result (silhouette score is computed for each point, take the average of them). 
#  Note that 'k' must be in {1,2, .., n-1} for pam().
#  Actually, when k=1, all roll-class are placed in the same cluster.
#  and when k='n' (where 'n' is nbr roll-call), each roll-call constitutes its own cluster.
#
#
# sim.mtrx: similarity matrix
# clu.algo.name: clustering method
#
#
# returns a list containing average silhouette scores associated with clustering result (i.e. partition)
#############################################################################################
perform.clustering = function(sim.mtrx, clu.algo.name=KMEDOIDS){
	
	res = list()
	mtrx.size = nrow(sim.mtrx)
	diss.mtrx = 1-sim.mtrx
	avg.sil.list = c()
	partitions = list()
	
	# when we start to iterate from 1, we get the following error: Error in sil[, COL.SILH.WIDTH] : nombre de dimensions incorrect
	avg.sil.list[1]=NA
	for(k in 2:(mtrx.size-1)){ # 'k' must be in {1,2, .., n-1} for pam()
		res=NA
		if(clu.algo.name == KMEDOIDS)
			res = pam(diss.mtrx,k, diss=TRUE)
#		else
#			res = ANOTHER.METHOD(diss.mtrx, k)
		
		sil = silhouette(res$clustering,diss.mtrx)
		
		partitions[[k]] = res$clustering
		# 'silhouette' score is computed for each point, take the average of them
		avg.sil.list[k] = mean(sil[,COL.SILH.WIDTH])
	}
	# if 'silhouette' score is 1 for point1, that means the point is well placed in its cluster (by means of avg. dissim.)
	# if 'silhouette' score is -1 for point1, that is the opposite meaning
	
	names(avg.sil.list) = 1:(mtrx.size-1)
	res[["avg.sil.list"]] = avg.sil.list
	names(partitions) = 1:(mtrx.size-1)
	res[["partitions"]] = partitions
	
	return(res)
}




#############################################################################################
# It clusters corresponding roll-calls based on a dissimilarity matrix that will be created through 'measures'.
# As clustering method, only k-medoids is used. Since k-medoids relies on the 'k' parameter, 
# one needs to specify it. In the current version of the code, 'k' is set to the number of roll-calls.
# This is needed in order to calculate silhouette scores for each 'k' value for quality assessment purposes.
# Then, one might decide on a range of 'k' values to focus on for further investigation. This range can be specified 
# thanks to the parameter 'k.limits'. The networks associated to detected clusters are created
# only for those 'k' values.
#
#
# rollcall.details: description of each voted roll-call.
# filtered.rollcall.ids: the considered roll-call ids regardin the selected domain and period
# partitions: partitions for the considered roll-calls.
#				Note that each roll-call is partitioned according to considered vote types (e.g. FAA)# measure: similarity measure in order to calcualte similarity matrix
# out.folder: the roll-call clustering folder path up to period value (e.g. out/rollcall-clustering/m3/bycountry/France/AGRI/2012-13/)
# clu.algo.name: the method to be used for clsutering process. KMEDOIDS is the only option.
# cons.vote.types.desc: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.focus.limits: A range of 'k' values for further investigation.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# domain: political domain to consider when processing the data.
# date: time period to consider when processing the data.
# 
#############################################################################################
perform.rollcall.clustering = function(rollcall.details, filtered.rollcall.ids, rollcall.part.folder, rollcall.partitions,
		measure, out.folder, clu.algo.name, cons.vote.types.desc, epsilon, k.focus.limits, score.file,
		country, group, dom, date)
{
	folder.path = get.rollcall.clustering.vote.type.path(score.file, cons.vote.types.desc, country, group, dom, date)
	dir.create(folder.path, recursive=TRUE, showWarnings=FALSE)
	silhouette.scores.file = file.path(folder.path,paste0(SILH.SCORES.FILE.PREFIX,"_measure=",measure))
	
	
	clu.partitions = NA
	rollcall.clu.result.per.k = NA
	if(!file.exists(paste0(silhouette.scores.file,".csv"))){
	    # =========================================================
	    # Build similarity matrix
	    # =========================================================
	    sim.mtrx.date.dom = create.intersection.based.similarity.matrix(rollcall.partitions, measure)
	    # record the matrix into file
	    colnames(sim.mtrx.date.dom) <- names(rollcall.partitions)
	    rownames(sim.mtrx.date.dom) <- names(rollcall.partitions)
	    # knowing vote types is enough about partitioning process (no need to specify 'ExCC')
	    table.file <- file.path(out.folder,paste0(SIM.MATRIX.FILE.PREFIX,"-for-rollcall-partitions",
					    "_mesure=",measure,"_votes=",cons.vote.types.desc,".csv"))
	    write.csv2(sim.mtrx.date.dom,file=table.file, row.names=TRUE)

		# =========================================================
		# Perform clustering for each possible 'k' value
		# =========================================================
		# similarity matrix will be converted into dissimilarity matrix
		res = perform.clustering(sim.mtrx.date.dom, clu.algo.name)
		clu.partitions = res$partitions[-1] # we did not run kmedoids or silhouette for k=1

		# ======================================================
		# record and plot silhouette scores (associated with k values)
		# ======================================================
		rollcall.clu.result.per.k = record.and.plot.silhouette.scores(silhouette.scores.file, res$avg.sil.list, res$partitions)
	} else {
		rollcall.clu.result.per.k = read.csv(file=paste0(silhouette.scores.file,".csv"))
		s.partitions = rollcall.clu.result.per.k[,COL.PARTITION]
		clu.partitions = lapply(s.partitions, function(part) as.integer(unlist(strsplit(part,","))))
	}
	
	
	# ======================================================
	# retreive the best clustering results based on epsilon or k.limits
	# ======================================================
	result = filter.and.record.clustering.results.by.epsilon.and.k(clu.algo.name, rollcall.clu.result.per.k, clu.partitions,
			epsilon, k.focus.limits, measure, cons.vote.types.desc, score.file, country, group, dom, date)

	clu.partitions = result$partitions
	sil.vals = as.numeric(result$sil.vals)
	k.vals = as.numeric(result$k.vals)
	
	# if filtered data is not empty. It is possible if epsilon and k.limits are not chosen appropriately at the same time 
	if(length(k.vals) > 0){ # we could also write: if(length(sil.vals) > 0)

	
		# ======================================================
		# Perform 2 things, for each clustering result of a given value of 'k' (e.g. 3-medoids):
		# - Prepare detailed output (roll-call ids, themes, etc., by cluster, if 3-medoids, then 3 clusters)
		# - regroup network images by cluster
		# ======================================================
		
		for(i in 1:length(k.vals)){
			sil.val = sil.vals[i]
			
			partition = clu.partitions[[i]]
			k.val = k.vals[i]
			desc = prepare.descs.for.silhouette.partitions(measure, cons.vote.types.desc, sil.val, k.val)
			sil.folder.path = get.rollcall.clustering.vote.type.silhouette.val.path(
					score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, date)
				
			# ----
			output.details.df = retreive.rollcall.titles.by.cluster(rollcall.details, filtered.rollcall.ids, partition)
			# write titles into file
			output.details.file <- file.path(sil.folder.path, paste0(clu.algo.name,"-rollcall-clustering-detailed-output.csv"))
			write.csv(x=output.details.df, file=output.details.file, row.names=FALSE)
			
			# ----
			# copy the images of the roll-call networks (with partition information) in order to regroup them by cluster
			# the advantage od doing this is to see if roll-calls in the same cluster are really similar in terms of partition.
			copy.rollcall.networks.by.cluster(rollcall.part.folder, sil.folder.path, output.details.df, k.val, cons.vote.types.desc)

		}
	
	} else {
		tlog(12,"WARNING: Problem while choosing a subset of the data. Empty subet.",
					" It is possible if epsilon and k.limits are not chosen appropriately at the same time ")
	}
	
	
}



#############################################################################################
# It performs clustering for all time periods and domains, for the specified clustering method
# and similarity measure(s). 'epsilon' and 'k.limits' are the 
# additional parameters to restrain the clustering process 
# for only some 'k' values (so, not all possible 'k' values).
#
#
# rollcall.details: description of each voted roll-call.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# measures: similarity measures in order to calcualte similarity matrix
# clu.algo.name: the method to be used for clsutering process. KMEDOIDS is the only option.
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.limits: A range of 'k' values. The networks associated to detected clusters are created
# 			only for those 'k' values.
# k.focus.limits: A range of 'k' values for further investigation. Specifically, in this script, is is only used 
# 					to regroup roll-call network images by cluster.
#
#############################################################################################
cluster.rollcalls <- function(rollcall.details, score.file, domains, dates, country, group, measures, clu.algo.name,
		cons.vote.types, epsilon, k.focus.limits)
{	
	
	for(dom in domains)
	{	#source("src/define-imports.R")
		
		# init
		partitions.term = list()
		out.domain.folder <- get.rollcall.clustering.path(score=score.file,country,group,domain=dom)
		dir.create(out.domain.folder, recursive=TRUE, showWarnings=FALSE)
		
		# take the first letter of vote types (e.g. "F" for FOR, "A" for "*AGAINST")
		list.first.letters = sapply(cons.vote.types, function(x) unlist(strsplit(x, split=""))[1])
		# build the description string for the considered vote types
		cons.vote.types.desc = paste(list.first.letters, collapse="")
		
		
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog("....roll-call clustering for domain ",dom," and period ",DATE.STR.T7[date])
			
			par.folder <- get.rollcall.partitions.path(score=score.file,country,group,domain=dom)
			par.folder <- paste0(par.folder,"/",DATE.STR.T7[date])
			out.date.folder <- paste0(out.domain.folder,"/",DATE.STR.T7[date])
			dir.create(out.date.folder, recursive=TRUE, showWarnings=FALSE)
			
			# retain only the roll-calls related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.rollcall.ids <- filter.rollcalls.by.date.and.domain(rollcall.details, 
					start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
					domains=domval)
			filtered.rollcall.ids = as.integer(filtered.rollcall.ids)
						
		
			# init
			partitions.date.dom = list()
			nb.rollcall = length(filtered.rollcall.ids)
			
			# check if there's enough data remaining
			if(nb.rollcall>1)
			{	
				# process each rollcall
				for(i in 1:nb.rollcall)
				{	tlog("..........Processing rollcall",i,"/",nb.rollcall)
					
					rollcall.id = as.character(filtered.rollcall.ids[i])
					in.rollcall.folder <- paste0(par.folder,"/",rollcall.id)
					
					
					#network.path.graphml = paste0(folder.net.rollcall.id,"/",SIGNED.FILE,".graphml")
					
					tlog("........Loading partition files")
					f.name = paste0("membership-votetype",cons.vote.types.desc)
					partition.file <- file.path(in.rollcall.folder, paste0(f.name, ".txt"))
					partition = NA
					if(!file.exists(partition.file))
						tlog("........Partition file ",partition.file," not found")
					else {
						partition <- as.matrix(read.table(partition.file))
						partitions.date.dom[[rollcall.id]] = partition
						partitions.term[[rollcall.id]] = partition
					}
					
				} # end for rollcalls	
			} # end if
			
			# ==================================================================================
			# process for each measure
			for(measure in measures){ # in general, there is only 1 measure
				perform.rollcall.clustering(rollcall.details, filtered.rollcall.ids, par.folder, partitions.date.dom, measure, out.date.folder,
						clu.algo.name, cons.vote.types.desc, epsilon, k.focus.limits, score.file, country, group, dom, DATE.STR.T7[date])
			}
			# ==================================================================================			
			
		} # end for Dates
		
		
	} # end for Domains
}




#############################################################################################
# It performs clustering for the whole dataset, by country and by political group, for the 
# specified clustering method and similarity measure(s). 'epsilon' and 'k.limits' are the 
# additional parameters to restrain the clustering process 
# for only some 'k' values (so, not all possible 'k' values).
#
#
# rollcall.details: description of each voted roll-call.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# measures: similarity measures in order to calcualte similarity matrix
# clu.algo.name: the method to be used for clustering process. KMEDOIDS is the only option.
# plot.formats: formats of the plot files.
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
# epsilon: Epsilon value in order to keep only best Silhouette scores with a margin defined by epsilon.
# k.focus.limits: A range of 'k' values for further investigation.
#					
#
#############################################################################################
cluster.all.rollcalls <- function(rollcall.details, score.file, domains, dates, everything, countries, groups, measures, clu.algo.name,
		cons.vote.types, epsilon, k.focus.limits)
{	tlog("***************************************************")
	tlog("****** PROCESSING ROLL-CALL CLUSTERING")
	tlog("***************************************************")
	
	# process network extraction by political group
	tlog("..Process rollcall clustering by group")
	for(group in groups)
	{	tlog("....Process roll-call clustering for group ",group)
		
		cluster.rollcalls(rollcall.details, score.file, domains, dates, country=NA, group, measures, clu.algo.name,
				cons.vote.types, epsilon, k.focus.limits)
	}
	
	# process agreement by home country
	tlog("..Process roll-call clustering by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process roll-call clustering for country ",country)
		
		cluster.rollcalls(rollcall.details, score.file, domains, dates, country, group=NA, measures, clu.algo.name,
				cons.vote.types, epsilon, k.focus.limits)
	}
	
	# process for all data
	if(everything)
	{	tlog("..Process roll-call clustering for all data")
		cluster.rollcalls(rollcall.details, score.file, domains, dates, country=NA, group=NA, measures, clu.algo.name,
				cons.vote.types, epsilon, k.focus.limits)
	}
}