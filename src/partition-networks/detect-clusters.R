#############################################################################################
# Functions to apply various community detection methods on the positive and complementary 
# negative networks, and various correlation clustering methods to the signed networks.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")
source("src/partition-networks/networks-common.R")
source("src/partition-networks/load-membership.R")


#############################################################################################
# Partitions the specified network, using the specified algorithm, and record the result
# as a table.  
#
# g: graph to process.
# algo.name: (normalized) name of the community detection algorithm.
# part.folder: folder in which to write the result (partition) files.
# graph.folder: folder of the processed network (for external tools).
# plot.formats: formats of the plot files.
# force: indicates whether existing result files should be loaded and used vs. replaced by new ones.
#
# returns: membership vector, i.e. cluster/community number for each node.
#############################################################################################
apply.partitioning.algorithm <- function(g, algo.name, absences, abstentions, part.folder, graph.folder, country, group, plot.formats, force=TRUE)
{	#tlog("n=",vcount(g), " m=",ecount(g), " d=",graph.density(g))
	#tlog(" connected=", is.connected(g,mode="weak"))
	
	mbrshp = NA
	
	# init file names
	dir.create(part.folder, recursive=TRUE, showWarnings=FALSE)
	
	table.file <- file.path(part.folder,paste0(algo.name,"-membership.txt"))
	if(!is.null(g$type))
		table.file <- file.path(part.folder,paste0(algo.name,"-",g$type,"-membership.txt"))
	
	idx <- regexpr(" -",g$name)[1]
	g$name <- paste0(get.algo.names(algo.name),substring(g$name,idx,nchar(g$name)))
	plot.file <- file.path(part.folder,paste0(algo.name,"-membership"))
	
	# check if all the files already exist
	process <- !file.exists(table.file)
	i <- 1
	while(!process && i<length(plot.formats))
	{	if(!is.na(plot.formats[i]))
		{	filename <- paste0(plot.file,".",plot.formats[i])
			process <- !file.exists(filename)
		}
		i <- i + 1
	}
	
	# if that is the case, we just need to load the membership values AND plot again
	if(!force && !process)
	{	tlog(12,"All the files are already present for algorithm ",algo.name," on folder ",part.folder," so we just load the existing results")
		#print(table.file)
		mbrshp <- as.numeric(as.matrix(read.table(file=table.file, header=FALSE)))
				
#		# add imbalane info into plot title
#		cc.imb.val = compute.imbalance.from.membership(g, mbrshp, output.type = "value")
#		cc.imb.per = compute.imbalance.from.membership(g, mbrshp, output.type = "percentage")
#		rcc.imb.val = compute.relaxed.imbalance.from.membership(g, mbrshp, output.type = "value")
#		rcc.imb.per = compute.relaxed.imbalance.from.membership(g, mbrshp, output.type = "percentage")
#		g$name = paste0(g$name,
#				"CC imbalance (value): ",cc.imb.val,", CC imbalance (percentage)",cc.imb.per, " --- ",
#				"RCC imbalance (value): ",rcc.imb.val,", RCC imbalance (percentage): ",rcc.imb.per)
		
				
		# =============================================================================================================
		# record a graphical representation of the detected partition
		## PLOT 1
		# plot.network(g, membership=mbrshp, plot.file, format=plot.formats)
		## PLOT 2
		# lyt = layout_in_circle(g, order=order(V(g)$Group))
		# V(g)$x <- lyt[,1]
		# V(g)$y <- lyt[,2]
		# plot.file = paste0(plot.file,"-circ_layout")
		# plot.network(g, membership=mbrshp, plot.file, format=plot.formats)


	}
		
	# otherwise, we must apply the community detection algorithm
	else
	{			
		tlog(12,"Applying algorithm ",algo.name," on folder ",part.folder)

		if(algo.name==COMDET.ALGO.EDGEBETW | algo.name==comdet.algo.ncg.value(COMDET.ALGO.EDGEBETW))
		{	# this implementation will use the weights and directions, if present
			res <- edge.betweenness.community(
						graph=g, edge.betweenness=FALSE, merges=FALSE,
						bridges=FALSE, modularity=TRUE, membership=TRUE) # modularity needed (bug in igraph v0.7)
			mbrshp = as.vector(membership(res))
			print(mbrshp)
		}
		else if(algo.name==COMDET.ALGO.INFOMAP | algo.name==comdet.algo.ncg.value(COMDET.ALGO.INFOMAP))
		{	# this implementation will use the weights and directions, if present
			print(vcount(g))
			res <- infomap.community(
						graph=g, modularity=FALSE)
			mbrshp = as.vector(membership(res))
			print(mbrshp)
		}
		else if(algo.name==COMDET.ALGO.LABELPROP | algo.name==comdet.algo.ncg.value(COMDET.ALGO.LABELPROP))
		{	# this implementation will use the weights and directions, if present
			res <- label.propagation.community(
						graph=g, 
						initial=NULL, fixed=NULL)
			mbrshp = as.vector(membership(res))
			print(mbrshp)
		}
		else if(algo.name==COMDET.ALGO.LOUVAIN | algo.name==comdet.algo.ncg.value(COMDET.ALGO.LOUVAIN))
		{	# this implementation will use the weights, if present, but cannot use directions
			print(vcount(g))
			#g2 <- as.undirected(g, mode="collapse")
			res <- multilevel.community(
						graph=g, weights=NULL)
			mbrshp = as.vector(membership(res))
			print(mbrshp)
		}
		else if(algo.name==COMDET.ALGO.WALKTRAP | algo.name==comdet.algo.ncg.value(COMDET.ALGO.WALKTRAP))
		{	# this implementation will use the weights, if present, and ignores directions
			res <- walktrap.community(
						graph=g, steps=4, 
						merges=TRUE, modularity=TRUE, membership=TRUE)
			mbrshp = as.vector(membership(res))
			print(mbrshp)
		}
		
		# apply the correlation clustering algorithm (which are external programs)
		else {
			# set the external command and invoke it
			cmd <- get.algo.commands(algo.names=algo.name, input.folder=graph.folder, out.folder=part.folder)
			tlog(14,"Command: ",cmd)
			start=Sys.time()
			system(command=cmd)
			end=Sys.time()
			exec.time = as.numeric(end) - as.numeric(start)
			# save exec.time: write into file
			write(x=exec.time, file=paste0(part.folder,"/",paste0(algo.name,"-",EXEC.TIME.FILENAME)))
			
			# load the resulting partition file

			mbrshp <- load.external.partition(part.folder, algo.name, keep.tmp=FALSE)
		}
		
		# record the result
		if(all(is.na(mbrshp)))
			tlog(12,"WARNING: Problem while applying partitioning algorithm ",algo.name," on folder ",part.folder)
		else
		{	
#			# OPTION 1: record the membership vector where each isolated node is placed into different cluster
#			# mbrshp1 = post.proc.membership.for.isolated.nodes(g, mbrshp, option.no=1)
#			# OPTION 2: record the membership vector where every isolated node is placed into the same cluster
#			mbrshp = post.proc.membership.for.isolated.nodes(g, mbrshp, option.no=2)
#			write.table(x=mbrshp, file=table.file, row.names=FALSE, col.names=FALSE)
			# TODO: what to do ? how to update membership file regarding isolated nodes?
			write.table(x=mbrshp, file=table.file, row.names=FALSE, col.names=FALSE)
			
#			if(all(is.na(mbrshp)))
#				mbrshp <- as.vector(membership(coms))
#			while(min(mbrshp)==0)
#				mbrshp <- mbrshp + 1
	
			
#			

#			
#			
#			# add imbalane info into plot title
#			cc.imb.val = compute.imbalance.from.membership(g, mbrshp, output.type = "value")
#			cc.imb.per = compute.imbalance.from.membership(g, mbrshp, output.type = "percentage")
#			rcc.imb.val = compute.relaxed.imbalance.from.membership(g, mbrshp, output.type = "value")
#			rcc.imb.per = compute.relaxed.imbalance.from.membership(g, mbrshp, output.type = "percentage")
#			g$name = paste0(g$name,
#					"CC imbalance (value): ",cc.imb.val,", CC imbalance (percentage): ",cc.imb.per, " --- ",
#					"RCC imbalance (value): ",rcc.imb.val,", RCC imbalance (percentage): ",rcc.imb.per)
			
			
			
#			# =============================================================================================================
#			# record a graphical representation of the detected partition
#			# PLOT 1
#			plot.network(g, membership=mbrshp, plot.file, format=plot.formats)
#			# PLOT 2
#			lyt = layout_in_circle(g, order=order(V(g)$Group))
#			V(g)$x <- lyt[,1]
#			V(g)$y <- lyt[,2]
#			plot.file = paste0(plot.file,"-circ_layout")
#			plot.network(g, membership=mbrshp, plot.file, format=plot.formats)

		}
	}
	

	if(("svg" %in% plot.formats) || ("jpg" %in% plot.formats)){
		#  -------------- CIRCOS --------------
		source("src/plot-tools/circos/circos.R")

		print(g$type)
		# note that algo.name is used for filename
		if(!is.null(g$type))
			algo.name <- paste0(algo.name,"-",g$type)
	
		absences = NA
		abstentions = NA
							
		if(!is.na(group)){
			# plot circos - all links
			produce.circos.plot.by.countries(g, partition=mbrshp, part.algo.name=algo.name, absences=NA, abstentions=NA, 
					show.imbalance.participation=FALSE, show.names=TRUE, show.clusters=TRUE, show.restricted.countries=FALSE,
					show.restricted.groups=FALSE, show.restricted.clusters=FALSE, out.folder=part.folder, clean.files=TRUE)
		} else { # if(!is.na(country)){ ==> by default
			# plot circos - all links
			produce.circos.plot.by.pol.groups(g, partition=mbrshp, part.algo.name=algo.name, absences=NA, abstentions=NA, 
					show.imbalance.participation=FALSE, show.names=TRUE, show.clusters=TRUE, show.restricted.countries=FALSE,
					show.restricted.groups=FALSE, show.restricted.clusters=FALSE, out.folder=part.folder, clean.files=TRUE)
		}
									
						
		# ====================================
		if("jpg" %in% plot.formats){
			all.svg.files = list.files(path=part.folder,pattern=paste0(".*\\.svg$"),full.names=FALSE)
			for(svg.file in all.svg.files){
				filename = gsub(".svg","",svg.file)
				convert.cmd <- paste0("convert -quality 10% ",part.folder,"/",svg.file," ",part.folder,"/",filename,".jpg")
				print(convert.cmd)
				system(command=convert.cmd)
			}
		}
		if(!("svg" %in% plot.formats)){
			# if svg is not a desired format, delete svg files created by circos
			unlink(list.files(path=part.folder,pattern=paste0(".*\\.svg$"),full.names=TRUE))
		}
		# ====================================
		
	}
	# ============================================================================================================
	
#	return(mbrshp)
}



#############################################################################################
# Loads all three graphs (signed, positive and complementary negative) and applies all 
# community detection and correlation clustering algorithms. Then, records the results as text 
# files and record a new graph file (Graphml format only) containing nodal attributes corresponding 
# to the detected communities, for each considered algorithm.
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domain: political domain currently processed.
# date: time period currently processed.
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# repetitions: number of times each algorithm must be applied.
# plot.formats: formats of the plot files.
# force: indicates whether existing result files should be loaded and used vs. replaced by new ones.
#############################################################################################
perform.partitioning <- function(thresh, score.file, domain, date, country, group, comdet.algos, corclu.algos, repetitions, plot.formats, force=TRUE)
{	
	# init graph files
	graph.folder <- get.networks.path(score=score.file, thresh, country, group, domain, period=date)
	part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=,date, repetition=NA)
	graph.file.neg <- file.path(part.folder,paste0(COMP.NEGATIVE.FILE,".graphml"))
	graph.file.pos <- file.path(part.folder,paste0(POSITIVE.FILE,".graphml"))
	graph.file <- file.path(part.folder,paste0(SIGNED.FILE,".graphml"))
	
	# if the new graph files were already created, load them
	if(!force && file.exists(graph.file.neg) && file.exists(graph.file.pos) && file.exists(graph.file))
	{	tlog(8,"Enhanced graph files already exist: loading them")
		g <- suppressWarnings(read.graph(file=graph.file, format="graphml"))
		g.pos <- suppressWarnings(read.graph(file=graph.file.pos, format="graphml"))
		g.neg <- suppressWarnings(read.graph(file=graph.file.neg, format="graphml"))
		graphs <- list(neg=g.neg, pos=g.pos, signed=g)
	}
	# otherwise, load the existing ones
	else
	{	tlog(8,"No enhanced graph files (or forced processing): loading the raw graph files")
		graphs <- retrieve.graphs(score=score.file, thresh, country, group, domain, period=date, comp=TRUE)
	}
	
	# the process might be repeated several times
	for(r in 1:repetitions)
	{	tlog(8,"Processing iteration ",r,"/",repetitions)
		# setup iteration folder
		#folder <- paste0(PARTITIONS.FOLDER,"/",subfolder)
		#r.folder <- paste0(folder,r,"/")
		#dir.create(r.folder, recursive=TRUE, showWarnings=FALSE)
		
		if(repetitions>=1) # TODO it was: if(repetitions>1) 
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=,date, repetition=r)
		else
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=,date, repetition=NA)
		dir.create(part.folder, recursive=TRUE, showWarnings=FALSE)
		
		# apply all community detection algorithms
		for(algo.name in comdet.algos)
		{	neg.algo.name <- comdet.algo.ncg.value(algo.name)
			
			# setup attribute name
			if(repetitions>1)
			{	pos.att.name <- paste0(algo.name,'-',r)
				neg.att.name <- paste0(neg.algo.name,'-',r)
			}
			else
			{	pos.att.name <- algo.name
				neg.att.name <- neg.algo.name
			}
			
			# complementary negative graph
			if(!all(is.na(graphs$neg)))
			{	tlog(10,"Applying ",get.algo.names(algo.name)," to the complementary negative graph")
				memb <- apply.partitioning.algorithm(graphs$neg, neg.algo.name, absences=NA, part.folder, graph.folder, country, group, plot.formats, force)
				graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=neg.att.name, value=memb)
				graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=neg.att.name, value=memb)
				graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=neg.att.name, value=memb)
			}
			
			# positive graph
			if(!all(is.na(graphs$pos)))
			{	tlog(10,"Applying ",get.algo.names(algo.name)," to the positive graph")
				memb <- apply.partitioning.algorithm(graphs$pos, algo.name, absences=NA, part.folder, graph.folder, country, group, plot.formats, force)
				graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=pos.att.name, value=memb)
				graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=pos.att.name, value=memb)
				graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=pos.att.name, value=memb)
			}
		}
		
		# apply all correlation clustering algorithms
		for(algo.name in corclu.algos)
		{	# setup attribute name
			if(repetitions>1)
				att.name <- paste0(algo.name,'-',r)
			else
				att.name <- algo.name
			
			if(!all(is.na(graphs$signed)))
			{	tlog(10,"Applying ",get.algo.names(algo.name)," to the signed graph")
				memb <- apply.partitioning.algorithm(graphs$signed, algo.name, absences=NA, part.folder, graph.folder, country, group, plot.formats, force)
				graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=att.name, value=memb)
				graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=att.name, value=memb)
				graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=att.name, value=memb)
			}
		}
	}

	# record graphs (Graphml only) with detected communities, in the partition folder (not the network one)
	if(!all(is.na(graphs$neg)))
		write.graph(graph=graphs$neg, file=graph.file.neg, format="graphml")
	if(!all(is.na(graphs$pos)))
		write.graph(graph=graphs$pos, file=graph.file.pos, format="graphml")
	if(!all(is.na(graphs$signed)))
		write.graph(graph=graphs$signed, file=graph.file, format="graphml")
}



#############################################################################################
# Applies the selected partitioning algorithms for all time periods and domains, for the specified 
# thresholds and agreement scores. 
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# repetitions: number of times each algorithm must be applied.
# plot.formats: formats of the plot files.
# force: indicates whether existing result files should be loaded and used vs. replaced by new ones.
#############################################################################################
partition.graphs <- function(thresh=NA, score.file, domains, dates, country, group, comdet.algos, corclu.algos, repetitions, plot.formats, force=TRUE)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
#	foreach(dom=domains) %dopar%
	{	source("src/define-imports.R")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog(6,"Detect communities for domain ",dom," and period ",DATE.STR.T7[date])
			
			# setup graph subfolder
			#folder <- paste0(subfolder,"/",score.file,
			#		"/","negtr=",thresh[1],"-postr=",thresh[2],
			#		"/",dom,"/",DATE.STR.T7[date],"/")
			
			# perform community detection
			perform.partitioning(thresh, score.file, dom, date, country, group, comdet.algos, corclu.algos, repetitions, plot.formats, force)
		}
	}
}


#############################################################################################
# Applies all selected partitioning algorithms, for the whole dataset, by country and by political 
# group, for the specified thresholds and agreement scores. 
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
# repetitions: number of times each algorithm must be applied (to assess the stability of the results).
# plot.formats: formats of the plot files.
# force: indicates whether existing result files should be loaded and used vs. replaced by new ones.
#############################################################################################
partition.all.graphs <- function(mep.details, thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, repetitions, plot.formats, force=TRUE)
{	tlog("***************************************************")
	tlog("****** PARTITIONING NETWORKS")
	tlog("***************************************************")
	
	# networks by political group
	tlog(2,"Detect communities by group")
	for(group in groups)
	{	tlog("....Detect communities for group ",group)
		
#		# select data
#		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
#		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
#		grp.meps <- mep.details[idx,]
		
		# extract networks
		partition.graphs(thresh, score.file, domains, dates, country=NA, group, comdet.algos, corclu.algos, repetitions, plot.formats, force)
	}
	
	# networks by home country
	tlog(2,"Detect communities by country")
	for(country in countries)
	{	tlog(4,"Detect communities for country ",country)
		
#		# select data
#		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
#		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
#		cntr.meps <- mep.details[idx,]
		
		# extract networks
		partition.graphs(thresh, score.file, domains, dates, country, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats, force)
	}

	# extract networks for all data
	if(everything)
	{	tlog(2,"Detect communities for all data")
		partition.graphs(thresh, score.file, domains, dates, country=NA, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats, force)
	}
}
