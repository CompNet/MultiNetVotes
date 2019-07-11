
# source("src/define-constants.R")
source("src/partition-networks/compare-clusters.R")

library(cluster)


#############################################################################################
# It prepares a statistic about the theme frequency over the clusters (for each theme).
#
#
# partition: a given clustering partition (e.g. k-medoid)
# roll.call.theme.ids: theme ids associates with roll-calls
# themes: existing theme names for a considered domain (e.g. AGRI)
# descs: a description string which combines measure, k value and silhouette score (e.g. 'F.purity-k=2-sil=0.52106')
#
# returns a matrix where rows is associated to cluster ids (cluster 1, cluster 2) and columns coressponds to themes.
#############################################################################################
process.theme.dist = function(partition, roll.call.theme.ids, themes, desc){	
	
	# ---------
	# since roll.call.theme.ids may have multiple labels for roll-calls, turn it into mono-labelled
	n = length(partition)
	new.partition = c()
	new.theme.ids = c()
	for(i in 1:n){
		labels = as.integer(unlist(strsplit(roll.call.theme.ids[[i]],",")))
		nb.label = length(labels)
		new.partition = c(new.partition, rep(partition[i],nb.label))
		new.theme.ids = c(new.theme.ids, labels)
	}
	# ---------
	
	nb.clu = length(unique(new.partition))
	res = matrix(0,nb.clu,length(themes))
	rownames(res) = paste(desc, " clu=", seq(1:nb.clu), sep="")
	colnames(res) = themes
	
	for(c in 1:nb.clu){
		part = new.theme.ids[which(new.partition==c)] # in list format
		vectorized.part = unlist(part)
		freq = table(vectorized.part)
		indx=as.integer(names(freq))
		res[c,indx]=freq
	}
	
	return(res)
}



#############################################################################################
#
#
#############################################################################################
make.post.rollcall.clu.analysis.by.theme <- function(rollcall.details, score.file, domains, dates, 
		country, group, measures, clu.algo.name, cons.vote.types, epsilon, k.limits)
{	
	
	for(dom in domains)
	{	#source("src/define-imports.R")
		
		# init
		partitions.term = list()
		out.domain.folder <- get.rollcall.clustering.path(score=score.file,country,group,domain=dom)
#		dir.create(out.domain.folder, recursive=TRUE, showWarnings=FALSE)
		
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog("....post analysis for domain ",dom," and period ",DATE.STR.T7[date])
			
			par.folder <- get.rollcall.partitions.path(score=score.file,country,group,domain=dom)
			par.folder <- paste0(par.folder,"/",DATE.STR.T7[date])
			out.date.folder <- paste0(out.domain.folder,"/",DATE.STR.T7[date])
			
			# retain only the roll-calls related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.rollcall.ids <- filter.rollcalls.by.date.and.domain(rollcall.details, 
					start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
					domains=domval)
			rollcall.theme.ids = rollcall.details[as.integer(filtered.rollcall.ids),COL.THEME.ID]
			
			
			for(measure in measures){
								
				# take the first letter of vote types => "F" for FOR, "A" for "*AGAINST"
				list.first.letters = sapply(cons.vote.types, function(x) unlist(strsplit(x, split=""))[1])
				cons.vote.types.desc = paste(list.first.letters, collapse="")
				
				folder.path = get.rollcall.clustering.vote.type.path(
						score.file, cons.vote.types.desc, country, group, dom, DATE.STR.T7[date])
				silhouette.scores.file = file.path(folder.path,paste0(SILH.SCORES.FILE.PREFIX,"_measure=",measure,".csv"))
				rollcall.clu.result.per.k = read.csv(file=silhouette.scores.file)
				
				all.sil.vals = rollcall.clu.result.per.k[,COL.AVG.SILH]
				all.k.vals = rollcall.clu.result.per.k[,COL.K]
				s.partitions = rollcall.clu.result.per.k[,COL.PARTITION]
				all.clu.partitions = lapply(s.partitions, function(part) as.integer(unlist(strsplit(part,","))))
		
				indx = filter.rollcall.clu.result.by.epsilon.and.k(rollcall.clu.result.per.k, epsilon=NA, k.limits)
				sil.vals = all.sil.vals[indx]
				k.vals = as.character(all.k.vals[indx])
				clu.partitions = all.clu.partitions[indx]
				
				descs = prepare.descs.for.silhouette.partitions(measure, cons.vote.types.desc, sil.vals, k.vals)
				out.folder = get.rollcall.clustering.vote.type.path(score.file, cons.vote.types.desc, country, group, dom, DATE.STR.T7[date])

				
				# -----
				# ======================================================
				# Assess the quality of clustering results in terms of purity
				#   of themes associated with roll-calls
				# Note that, when k increases, purity increases as well
				# ======================================================
				# compute purity at partition level
				descs = prepare.descs.for.silhouette.partitions(measure, cons.vote.types.desc, sil.vals, k.vals)
				purity.res = process.purity(clu.partitions, rollcall.theme.ids, descs)
				file.name = paste(folder.path,"/",measure,"-purity.csv",sep="")
				write.csv(file=file.name, x=purity.res)
			
				# compute inverse purity at partition level
				descs = prepare.descs.for.silhouette.partitions(measure, cons.vote.types.desc, sil.vals, k.vals)
				purity.inv.res = process.purity.inv(clu.partitions, rollcall.theme.ids, descs)
				file.name = paste(folder.path,"/",measure,"-purity-inv.csv",sep="")
				write.csv(file=file.name, x=purity.inv.res)
			
				# plot partition-level purity vs. silhouette scores
				file.name = paste(folder.path,"/",measure,"-purity-vs-silhouette.pdf",sep="")
				pdf(file=file.name,bg="white",compress=COMPRESS)
				plot(x=purity.res[,"purity"], y=sil.vals,
						xlab="purity", ylab="silhouette scores", col= "blue", pch = 19, cex = 1, lty = "solid")
				text(x=purity.res[,"purity"], y=sil.vals, labels=k.vals, cex= 0.6, pos=3)
				dev.off()
				# -----
				
		# - Prepare a statistic about the theme frequency over the clusters (for each theme)
		# - Assess the quality of clustering result in terms of purity of themes associated with roll-calls
		#   (i.e. are the clusters pure in terms of theme, or heterogenous?)
				
				themes = retreive.theme.names.by.domain(dom)
				for(i in 1:length(k.vals)){
					sil.val = sil.vals[i]
					partition = clu.partitions[[i]]
					k.val = k.vals[i]
					desc = prepare.descs.for.silhouette.partitions(measure, cons.vote.types.desc, sil.val, k.val)
					sil.folder.path = get.rollcall.clustering.vote.type.silhouette.val.path(
							score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, DATE.STR.T7[date])
					
					# ----
					dist = process.theme.dist(partition, rollcall.theme.ids, themes, desc)
					file.name = file.path(sil.folder.path, paste0("theme-freq-dist","-over-",clu.algo.name,"-membership",".csv",sep=""))
					write.csv(file=file.name, x=dist, row.names=FALSE)
					
					# ----
					# compute purity at cluster level: partition vs. ground.truth
					clu.purity = compute.purity.for.partition.vs.multi.labeled.ground.truth(partition, rollcall.theme.ids, purity.level="cluster")
					file.name = file.path(sil.folder.path, paste0("purity-",clu.algo.name,"-membership","_vs_themes.csv"))
					write.csv(file=file.name, x=clu.purity, row.names=FALSE)
			
					# compute purity at cluster level: ground.truth vs. partition
					clu.purity = compute.purity.for.multi.labeled.ground.truth.vs.partition(rollcall.theme.ids, partition, purity.level="cluster")
					file.name = file.path(sil.folder.path, paste0("purity-themes_vs_",clu.algo.name,"-membership",".csv"))
					write.csv(file=file.name, x=clu.purity, row.names=FALSE)
				
					
				}
			
			}		
			
		} # end for Dates
		
		
	} # end for Domains
}




#############################################################################################
#
#
#############################################################################################
make.all.post.rollcall.clu.analysis.by.theme <- function(rollcall.details, score.file, domains, dates, everything, countries, groups, measures, clu.algo.name,
		cons.vote.types, epsilon, k.limits)
{	tlog("***************************************************")
	tlog("****** MAKE ALL POST ROLL-CALL CLUSTER ANALYSIS BY THEME ******")
	tlog("***************************************************")
	
	# process network extraction by political group
	tlog("..Process post analysis by group")
	for(group in groups)
	{	tlog("....Process post analysis for group ",group)
		
		make.post.rollcall.clu.analysis.by.theme(rollcall.details, score.file, domains, dates, country=NA, group, measures, clu.algo.name,
				cons.vote.types, epsilon, k.limits)
	}
	
	# process agreement by home country
	tlog("..Process post analysis by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process post analysis for country ",country)
		
		make.post.rollcall.clu.analysis.by.theme(rollcall.details, score.file, domains, dates, country, group=NA, measures, clu.algo.name,
				cons.vote.types, epsilon, k.limits)
	}
	
	# process for all data
	if(everything)
	{	tlog("..Process post analysis for all data")
		make.post.rollcall.clu.analysis.by.theme(rollcall.details, score.file, domains, dates, country=NA, group=NA, measures, clu.algo.name,
				cons.vote.types, epsilon, k.limits)
	}
}

