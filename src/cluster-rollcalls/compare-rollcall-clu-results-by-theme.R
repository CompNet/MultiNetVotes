
# source("src/define-constants.R")
source("src/partition-networks/compare-clusters.R")

library(cluster)


#############################################################################################
#
#
#############################################################################################
compare.rollcall.clu.results.by.theme <- function(rollcall.details, score.file, domains, dates, country, group, measures, clu.algo.name,
		cons.vote.types, epsilon, k.limits)
{	
	
	for(dom in domains)
	{	#source("src/define-imports.R")
		
		# init
		partitions.term = list()
		out.domain.folder <- get.rollcall.clustering.path(score=score.file,country,group,domain=dom)
#		dir.create(out.domain.folder, recursive=TRUE, showWarnings=FALSE)
		
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog("....roll-call clustering for domain ",dom," and period ",DATE.STR.T7[date])
			
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
			theme.id.labels = rollcall.details[as.integer(filtered.rollcall.ids),COL.THEME.ID]
			
			
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

				indx = filter.rollcall.clu.result.by.epsilon.and.k(rollcall.clu.result.per.k, epsilon=NA, k.limits)
				sil.vals = all.sil.vals[indx]
				k.vals = as.character(all.k.vals[indx])
				
				descs = prepare.descs.for.silhouette.partitions(measure, cons.vote.types.desc, sil.vals, k.vals)

				out.folder = get.rollcall.clustering.vote.type.path(score.file, cons.vote.types.desc, country, group, dom, DATE.STR.T7[date])

				nb.vert.layer = length(k.vals)
				partitions.mtrx = c()
				for(k in 1:nb.vert.layer){ # iterate over k values (Approach 2)
					sil.val = sil.vals[k]
					k.val = k.vals[k]
					
					sil.folder.path = get.rollcall.clustering.vote.type.silhouette.val.path(
							score.file, cons.vote.types.desc, sil.val, k.val, measure, country, group, dom, DATE.STR.T7[date])
					mbrshp = read.table(file=file.path(sil.folder.path,paste0(clu.algo.name,"-membership.txt")))$V1
					partitions.mtrx = cbind(partitions.mtrx, mbrshp)
				}
				partitions.df = as.data.frame(partitions.mtrx)
				
				# plot
				plot.alluvial.diagram(out.folder, nb.vert.layer, theme.id.labels, partitions.df, x.labels=descs, dom, measure, FALSE, FALSE)
				
				# show strip borders (might be more meaningfull when there is not too many horizantal layers, i.e. roll-calls)
				#plot.alluvial.diagram(out.folder, nb.vert.layer, theme.id.labels, partitions.df, x.labels=descs, dom, measure, FALSE, TRUE)
				# order by theme in each cluster ==> I am not sure if it works TODO
				#plot.alluvial.diagram(out.folder, nb.vert.layer, theme.id.labels, partitions.df, x.labels=descs, dom, measure, TRUE, FALSE)

			}		
			
		} # end for Dates
		
		
	} # end for Domains
}




#############################################################################################
#
#
#############################################################################################
compare.all.rollcall.clu.results.by.theme <- function(rollcall.details, score.file, domains, dates, everything, countries, groups, measures, clu.algo.name,
		cons.vote.types, epsilon, k.limits)
{	tlog("***************************************************")
	tlog("****** COMPARING ALL ROLL-CALL CLUSTER RESULTS ******")
	tlog("***************************************************")
	
	# process network extraction by political group
	tlog("..Process roll-call clustering by group")
	for(group in groups)
	{	tlog("....Process roll-call clustering for group ",group)
		
		compare.rollcall.clu.results.by.theme(rollcall.details, score.file, domains, dates, country=NA, group, measures, clu.algo.name,
				cons.vote.types, epsilon, k.limits)
	}
	
	# process agreement by home country
	tlog("..Process roll-call clustering by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process roll-call clustering for country ",country)
		
		compare.rollcall.clu.results.by.theme(rollcall.details, score.file, domains, dates, country, group=NA, measures, clu.algo.name,
				cons.vote.types, epsilon, k.limits)
	}
	
	# process for all data
	if(everything)
	{	tlog("..Process roll-call clustering for all data")
		compare.rollcall.clu.results.by.theme(rollcall.details, score.file, domains, dates, country=NA, group=NA, measures, clu.algo.name,
				cons.vote.types, epsilon, k.limits)
	}
}