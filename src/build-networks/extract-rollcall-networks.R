# TODO: Add comment
# 
# Author: nejat
###############################################################################
source("src/define-constants.R")
source("src/plot-tools/plot-histos.R")
source("src/plot-tools/plot-networks.R")
source("src/prepare-data/filter-data.R")
source("src/build-networks/process-agreement.R") # get load.score.table() and process.agreement.scores()


#############################################################################################
# Generates the graph files for the specified data and parameters. The fonction record the
# graph using the Graphml format, as well as a format compatible with the pILS algorithm.
# 
# agreement: agreement indices (based on considered roll-call) for the considered MEPs.
# mep.details: MEP details, only for the considered MEPs.
# thresh: vector containing a inf and a sup thresholds. All agreement values between them are
#		  set to zero (i.e. ignored). A value NA for one threshold means it is estimated automatically.
#		  Value (0,0) means no thresholding at all.
# folder: folder (and beginning of the filename) for the produced graph files.
# graph.name: name of the graph in the graphml file.
# plot.formats: formats of the plot files.
#############################################################################################
extract.rollcall.network <- function(agreement, mep.details, folder, graph.name, plot.formats)
{	tlog("......Building network folder='",folder,"'")
	
	# replace NAs by zeros
	agreement[is.na(agreement)] <- 0
	
	mep.ids = as.integer(mep.details[,1])
    rownames(agreement) = mep.ids
    colnames(agreement) = mep.ids

	# build network using igraph
	result <- graph.adjacency(adjmatrix=agreement, 		# use the agreement as the adjacency matrix
			mode="undirected", weighted=TRUE, diag=FALSE,	# ignore the diagonal of the agreement matrix
			add.colnames=NA, add.rownames="MEPid")			# use the id as node names
	result$name <- graph.name
	
	# check if network is empty: It is possible that have an empty network (e.g. all MEPS abstain)
	# even though the network is empty, it is ok, record it
	if(ecount(result)==0)
		tlog("........WARNING: the signed graph contains no links >> it is still recorded")
	
	# add MEP attributes
	#V(result)$Firstname <- mep.details[,COL.FIRSTNAME]
	firstnames <- iconv(mep.details[,COL.FIRSTNAME], to='ASCII//TRANSLIT')
	V(result)$Firstname <- firstnames		
	#V(result)$Lastname <- mep.details[,COL.LASTNAME]
	lastnames <- iconv(mep.details[,COL.LASTNAME], to='ASCII//TRANSLIT')
	V(result)$Lastname <- lastnames
	V(result)$Country <- mep.details[,COL.STATE]
	V(result)$Group <- mep.details[,COL.GROUP]
	V(result)$Region <- mep.details[,COL.EU.REGION]
	V(result)$MepGlobalId <- mep.details[,COL.MEPID]
	
	# plot graph and get spatial positions as nodal attributes
	# tlog("........Plotting network...")
	graph.base <- file.path(folder,SIGNED.FILE)
	# result <- plot.network(g=result, plot.file=graph.base, format=plot.formats)
	
	# export the graph under the graphml format
	graph.file <- paste0(graph.base,".graphml")
	tlog(graph.file)
	write.graph(graph=result, file=graph.file, format="graphml")
	
	# export using a format compatible with pILS
	t <- get.edgelist(graph=result) - 1	# start numbering nodes at zero
	t <- cbind(t,E(result)$weight)		# add weights as the third column
	graph.file <- paste0(graph.base,".G")
	tlog(graph.file)
	write.table(data.frame(nrow(mep.details),nrow(t)), file=graph.file, append=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	# write header
	write.table(t, file=graph.file, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)										# write proper graph
}






#############################################################################################
# Processes the agreement for all domains and time periods, for the specified raw votes.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# matrix is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained gets a score of zero when compared to himself.
#
# all.votes: raw vote data, including how each MEP voted.
# rollcall.details: description of each voted roll-call.
# score.file: files describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# plot.formats: formats used for the plot files.
#############################################################################################
extract.rollcall.networks <- function(all.votes, rollcall.details, mep.details, score.file, domains, dates, country, group, plot.formats)
{	
	# load the agreement scores
	score.table <- load.score.table(score.file)
	
	# setup graph title
	if(is.na(country))
		if(is.na(group))
			mode.str <- ""
		else
			mode.str <- paste0(" - group=",group)
	else
		mode.str <- paste0(" - country=",country)
	base.graph.name <- paste0("MEP agreement - score=",score.file,mode.str)
	
	
	# consider each domain individually (including all domains at once if domains=DOMAIN.ALL)
	for(dom in domains){
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		#foreach(date=dates[1:length(dates)-1]) %dopar% # no need for the 'Term' period
		#date <- DATE.T7.TERM
		{	source("src/define-imports.R")
			tlog("......Processing agreement data for domain ",dom," and period ",DATE.STR.T7[date])
			
			# setup graph title
			graph.name <- paste0(base.graph.name," - domain=",dom," - period=",DATE.STR.T7[date])
			# setup folder
			folder <- get.rollcall.networks.path(score=score.file, country, group, domain=dom)
			dir.create(folder, recursive=TRUE, showWarnings=FALSE)
			folder <- paste0(folder,"/",DATE.STR.T7[date])
			dir.create(folder, recursive=TRUE, showWarnings=FALSE)
			
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
				
				
				# mep.ids <- as.integer(all.votes[active.idx,COL.MEPID])
				votes <- all.votes[,cols]
				rollcall.ids = colnames(votes)
				
				# process each roll-call in the vote matrix
				for(i in 1:ncol(votes))
				{	tlog("..........Processing rollcall",i,"/",ncol(votes))
					vote = votes[,i]
					
					# create a folder for each single roll-call
					rollcall.id = as.integer(rollcall.ids[i])
					folder.rollcall.id <- paste0(folder,"/",rollcall.id)
					dir.create(folder.rollcall.id, recursive=TRUE, showWarnings=FALSE)
					
					
					# get scores. 'agreement' is a square matrix whose size is equivalent to nb MEPs
					agreement <- process.agreement.scores(vote, score.table)
					extract.rollcall.network(agreement, mep.details, folder.rollcall.id, graph.name, plot.formats)
				}
				
			}
			else
				tlog("........WARNING: Only ",length(filtered.rollcall.ids)," roll-calls remaining after filtering >> not processing these data")
		}
	}
}



############################################
# Main function of this script, generating all agreement-related tables and plots.
#
# Note: the agreement between a MEP and himself might not be 1, depending on how the score
# matrix is defined. For instance, if Absention vs. Abstention gets a score of 0, then a
# MEP who abstained gets a score of zero when compared to himself.
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
# plot.formats: formats used for the plot files.
#############################################################################################
extract.all.rollcall.networks <- function(all.votes, rollcall.details, mep.details, score.file, domains, dates, everything, countries, groups, plot.formats)
{	tlog("***************************************************")
	tlog("****** PROCESSING ROLLCALL-WISE NETWORKS")
	tlog("***************************************************")
	
	# process network extraction by political group
	tlog("..Process roll-call networks by group")
	for(group in groups)
	{	tlog("....Process roll-call networks for group ",group)
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,all.votes[,COL.MEPID])
		group.votes <- all.votes[idx,]
		mep.details2 <- mep.details[idx,]
		
		# process network extraction
		extract.rollcall.networks(group.votes, rollcall.details, mep.details2, score.file, domains, dates, country=NA, group, plot.formats)
	}
	
	# process agreement by home country
	tlog("..Process roll-call networks by country")
	for(country in countries)
	#country <- COUNTRY.HR
	{	tlog("....Process roll-call networks for country ",country)
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,all.votes[,COL.MEPID])
		country.votes <- all.votes[idx,]
		mep.details2 <- mep.details[idx,]
		
		# process network extraction
		extract.rollcall.networks(country.votes, rollcall.details, mep.details2, score.file, domains, dates, country, group=NA, plot.formats)
	}
	
	# process network extraction for all data
	if(everything)
	{	tlog("..Process rollcall networks for all data")
		extract.rollcall.networks(all.votes, rollcall.details, mep.details, score.file, domains, dates, country=NA, group=NA, plot.formats)
	}
}


