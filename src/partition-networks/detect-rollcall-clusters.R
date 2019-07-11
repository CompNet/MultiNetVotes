
source("src/partition-networks/detect-clusters.R")


#############################################################################################
# Remove MEPs which has not joined the EP during the spefic period (i.e. 2009-10)
# For example, a MEP may begin working starting from 01/01/2010 although the 2009-10 period starts on 01/07/2009
# and finishes on 01/07/2010
# For doing so: get rollcall id, and identify MEPS which has not joined the EP
#
# Example of periods:
# 12/05/2005:30/06/2014
# 14/07/2009:30/06/2014
# 14/07/2009:NA
# 20/07/2004:NA
# 14/07/2009:25/04/2011
# 25/07/1989:11/05/1994::20/07/1999:30/06/2014
#############################################################################################
find.unavailable.meps.by.rollcall.id = function(rollcall.id, rollcall.details, mep.details){
	rollcall.date = as.Date(rollcall.details[rollcall.id,COL.DATE],"%d/%m/%Y")
	# tlog("rollcall.date:", as.character(rollcall.date))
	mep.indx.list = c()
	for(i in seq(1, length(mep.details[, COL.PERIODS]))){
		# "01/05/2017" is chosen arbitrary, which is more recent than 2014 (last period)
		date.list = gsub("NA", "01/05/2017", as.character(mep.details[i, COL.PERIODS])) 
		s.date.interval.list = unlist(strsplit(x=date.list, split="::"))
		
		mep.availability.check.list = sapply(s.date.interval.list,
				function(s.date.interval){
					s.date.list = unlist(strsplit(x=s.date.interval, split=":"))
					date.list = as.Date(s.date.list, format="%d/%m/%Y")
					beg.period.mep = date.list[1]
					end.period.mep = date.list[2]
					
					available = beg.period.mep <= rollcall.date && rollcall.date <= end.period.mep
					return(available)
				})
		
		if(all(mep.availability.check.list == FALSE)) # if MEP rollcall date
			mep.indx.list = c(mep.indx.list, i)
	}
	
	return(mep.indx.list)
}



#############################################################################################
# This method is used instead of the ExCC partitioning method. Since the network to be partitioned
#   is a single roll-call network, resulting MEP votes would already yield bi-section or tri-section
#	depending on the considered vote types ('FA' or 'FAA'). So, iy ouputs a membership vector at the end.
#
#############################################################################################
fill.membership.by.vote.type = function(vote, cons.vote.types, nb.mep, indx.unavailable.meps){
	# init
	mbrshp = rep(NA, nb.mep)
	
	counter = 0 
	for(vote.type in cons.vote.types){
		indx = which(vote == vote.type)
		
		if(length(indx)>0){
			counter = counter + 1
			mbrshp[indx] = counter
		}
	}
	
	return(mbrshp)
}



#############################################################################################
# Partitions all networks for all domains and time periods, for the specified raw votes.
#
# all.votes: individual vote data, i.e. how each MEP voted.
# rollcall.details: description of each voted roll-call.
# mep.details: description of each MEP.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# plot.formats: formats of the plot files.
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
#############################################################################################
partition.partitioning.rollcall.networks <- function(all.votes, rollcall.details, mep.details, score.file, domains, dates,
		country, group, plot.formats, cons.vote.types)
{	
	for(dom in domains)
	{	source("src/define-imports.R")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog("....Extracting network for domain ",dom," and period ",DATE.STR.T7[date])
			
			par.folder <- get.rollcall.partitions.path(score=score.file,country,group,domain=dom)
			par.folder <- paste0(par.folder,"/",DATE.STR.T7[date])
			dir.create(par.folder, recursive=TRUE, showWarnings=FALSE)
			
			rollcall.network.folder = get.rollcall.networks.path(score=score.file, country, group, domain=dom)
			rollcall.network.folder <- paste0(rollcall.network.folder,"/",DATE.STR.T7[date])
			print(rollcall.network.folder)
			
			
			# retain only the roll-calls related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.rollcall.ids <- filter.rollcalls.by.date.and.domain(rollcall.details, 
					start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
					domains=domval)
						
			# check if there's enough data remaining
			if(length(filtered.rollcall.ids)>1)
			{	# format data
				cols <- match(filtered.rollcall.ids, colnames(all.votes))
#				active.idx <- which(apply(all.votes[,cols],1,function(v) !all(is.na(v))))
				
	
				# mep.ids <- as.integer(all.votes[active.idx,COL.MEPID])
				votes <- all.votes[,cols]
				rollcall.ids = colnames(votes)
				
				# process each roll-call in the vote matrix
				for(i in 1:ncol(votes))
				{	tlog("..........Processing rollcall",i,"/",ncol(votes))
		
					# create a folder for each single vote
					vote = votes[,i]
					rollcall.id = as.integer(rollcall.ids[i])
					folder.net.rollcall.id <- paste0(rollcall.network.folder,"/",rollcall.id)
					folder.par.rollcall.id <- paste0(par.folder,"/",rollcall.id)
					dir.create(folder.par.rollcall.id, recursive=TRUE, showWarnings=FALSE)

					
					# ==================================================================================================
					nb.mep = nrow(votes)
					# enumerate the membership vector in this order: FOR, AGAINST, ABSTENTION, UNAVAILABLE, ABSENT
		
					# distinguish VOTE.ABSENT WITH UNAVAILABLE.MEP.VOTE.ABSENT => update "vote" vector
					indx.unavailable.meps = find.unavailable.meps.by.rollcall.id(rollcall.id, rollcall.details, mep.details)
					if(length(indx.unavailable.meps)>0)
						vote[indx.unavailable.meps] = UNAVAILABLE.MEP.VOTE.ABSENT
					

					# take the first letter of vote types => "F" for FOR, "A" for "*AGAINST"
					list.first.letters = sapply(cons.vote.types, function(x) unlist(strsplit(x, split=""))[1])
					cons.vote.types.desc = paste(list.first.letters, collapse="")
					
					mbrshp = fill.membership.by.vote.type(vote, cons.vote.types, nb.mep, indx.unavailable.meps)
					f.name = paste0("membership-votetype",cons.vote.types.desc)
					table.file <- file.path(folder.par.rollcall.id, paste0(f.name, ".txt"))
					write.table(x=mbrshp, file=table.file, row.names=FALSE, col.names=FALSE)
					# ==================================================================================================
					
					
					# ==================================================================================================
					#network.path.G = paste0(folder.net.rollcall.id,"/",SIGNED.FILE,".G")
					network.path.graphml = paste0(folder.net.rollcall.id,"/",SIGNED.FILE,".graphml")
					g <- suppressWarnings(read.graph(file=network.path.graphml, format="graphml"))
					#plot.file <- file.path(folder.par.rollcall.id,f.name)
					#plot.network(g, membership=mbrshp, plot.file, format=plot.formats)
					
	
					# ==================================================================================================

					if(("svg" %in% plot.formats) || ("jpg" %in% plot.formats)){
						#  -------------- CIRCOS --------------
						source("src/plot-tools/circos/circos.R")
		
		
						## load the absence data for CIRCOS plotting
						absences = get.meps.absences(as.matrix(vote)) # as there is 1 vote, we need to convert into matrix format
						abstentions = get.meps.abstentions(as.matrix(vote)) # as there is 1 vote, we need to convert into matrix format
											
						if(!is.na(group)){
							# plot circos - all links
							produce.circos.plot.by.countries(g, partition=mbrshp, part.algo.name=cons.vote.types.desc, absences, abstentions, 
									show.imbalance.participation=FALSE, show.names=TRUE, show.clusters=TRUE, show.restricted.countries=FALSE,
									show.restricted.groups=FALSE, show.restricted.clusters=FALSE, out.folder=folder.par.rollcall.id, clean.files=TRUE)
						} else { # if(!is.na(country)){ ==> by default
							# plot circos - all links
							produce.circos.plot.by.pol.groups(g, partition=mbrshp, part.algo.name=cons.vote.types.desc, absences, abstentions, 
									show.imbalance.participation=FALSE, show.names=TRUE, show.clusters=TRUE, show.restricted.countries=FALSE,
									show.restricted.groups=FALSE, show.restricted.clusters=FALSE, out.folder=folder.par.rollcall.id, clean.files=TRUE)
						}
						
						
						# ====================================
						if("jpg" %in% plot.formats){
							all.svg.files = list.files(path=folder.par.rollcall.id,pattern=paste0(".*\\.svg$"),full.names=FALSE)
							for(svg.file in all.svg.files){
								filename = gsub(".svg","",svg.file)
								convert.cmd <- paste0("convert -quality 10% '",folder.par.rollcall.id,"/",svg.file,"' '",folder.par.rollcall.id,"/",filename,".jpg'")
								print(convert.cmd)
								system(command=convert.cmd)
							}
						}
						if(!("svg" %in% plot.formats)){
							# if svg is not a desired format, delete svg files created by circos
							unlink(list.files(path=folder.par.rollcall.id,pattern=paste0(".*\\.svg$"),full.names=TRUE))
						}
						# ====================================
	
					}
				
					
				}
					
				
				
			}
		
		}
	}
}




#############################################################################################
# Partitions all networks for the whole dataset, by country and by political group, for the 
# 	agreement scores and considered vote types. 
#
# all.votes: individual vote data, i.e. how each MEP voted.
# rollcall.details: description of each voted roll-call.
# mep.details: description of each MEP.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# plot.formats: formats of the plot files.
# cons.vote.types: considered vote types when partitioning roll-call networks. For ex: 'FAA'
#############################################################################################
partition.all.rollcall.networks <- function(all.votes, rollcall.details, mep.details, score.file, domains, dates, 
		everything, countries, groups, plot.formats = c("svg","jpg"), cons.vote.types)
{	tlog("***************************************************")
	tlog("****** PROCESSING ExCC")
	tlog("***************************************************")
	
	# process network extraction by political group
	tlog("..Process ExCC by group")
	for(group in groups)
	{	tlog("....Process ExCC for group ",group)
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,all.votes[,COL.MEPID])
		group.votes <- all.votes[idx,]
		mep.details2 <- mep.details[idx,]
		
		# process
		partition.partitioning.rollcall.networks(group.votes, rollcall.details, mep.details2, score.file, domains, dates,
				country=NA, group, plot.formats, cons.vote.types)
	}
	
	# process agreement by home country
	tlog("..Process ExCC by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process ExCC for country ",country)
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,all.votes[,COL.MEPID])
		country.votes <- all.votes[idx,]
		mep.details2 <- mep.details[idx,]
		
		# process
		partition.partitioning.rollcall.networks(country.votes, rollcall.details, mep.details2, score.file, domains, dates, 
				country, group=NA, plot.formats, cons.vote.types)
	}
	
	# process for all data
	if(everything)
	{	tlog("..Process ExCC for all data")
		partition.partitioning.rollcall.networks(all.votes, rollcall.details, mep.details, score.file, domains, dates, 
				country=NA, group=NA, plot.formats, cons.vote.types)
	}
}

