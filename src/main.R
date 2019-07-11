#############################################################################################
# Main script, launches the whole process:
# - Load the raw data
# - Preprocess/filter the resulting tables
# - Extract the collection of single roll-call networks
# - Partition single roll-call networks, and generate network images with this info
# - Apply k-medoids in order to cluster roll-calls based on how they are voted by MEPs
# - Construct aggregated roll-call networks for each cluster detected in the k-medoids result
#		=> this network can be signed or unsigned
# - Apply a partitioning algorithm (community detection or ExCC, depending on the network) to partition
#		aggregated networks. The resulting partitions are called 'characteristic voting patterns'.
# - (optional) Compare partitions of the aggregated roll-call networks (to see how similar they are)
# 
# The parameters located at the beginning of the script (section "Init parameters", right below)
# allow to control it, and to restrict the focus to certain topics/years, or control certain 
# points of the network extraction.
# 
#
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################
# libraries for parallel processing
#library(foreach)
#library(doParallel)

source("src/define-imports.R")



#############################################################################################
# Init parameters
#############################################################################################
##################### raw data
#dataset.name <- "VW"		# VoteWatch
dataset.name <- "IYP"		# It's your Parliament
#dataset.name <- "PT"		# Parltrack


##################### domains
#domains <- c(DOMAIN.VALUES, DOMAIN.ALL)			# which domains to process individually
#domains <- DOMAIN.ALL
#domains <- c(DOMAIN.AGRI, DOMAIN.FEMM, DOMAIN.ECON)
#domains <- c(DOMAIN.FEMM)
domains <- c(DOMAIN.AGRI)
#domains <- c(DOMAIN.VW2SYMB[TEST.DOMAINS],DOMAIN.ALL)
##################### dates
#dates <- c(DATE.T7.YEARS, DATE.T7.TERM)			# which time periods to process individually
#DATE.T7.YEARS <- c(DATE.T7.Y1, DATE.T7.Y2, DATE.T7.Y3, DATE.T7.Y4, DATE.T7.Y5)
#dates <- c(DATE.T7.TERM)
dates <- c(DATE.T7.Y4)
#DATE.T7.YEARS <- c(DATE.T7.Y2, DATE.T7.Y3, DATE.T7.Y5)
#dates = c(DATE.T7.YEARS)
#dates <- C(
#		DATE.T7.Y1
#		DATE.T7.Y2
#		DATE.T7.Y3,
#		DATE.T7.Y4,
#		DATE.T7.Y5,
#		DATE.T7.TERM
#)
#dates <- TEST.YEARS
##################### everything at once
#everything <- TRUE								# whether or not to process all data without distinction of country or date
everything <- FALSE
##################### countries
#countries <- COUNTRY.VALUES						# which country to process individually
countries <- c(COUNTRY.FR)
#countries <- c()
#countries <- c(COUNTRY.FR, COUNTRY.IT, COUNTRY.UK)
#countries <- TEST.COUNTRIES

##################### groups
#groups <- GROUP.VALUES
groups <- c()
# which group to process individually
#groups <- c(GROUP.EPP)
#groups <- GROUP.VW2SYMB[TEST.GROUPS]
#groups <- c(
#	GROUP.ALDE,GROUP.ECR,GROUP.EFD,GROUP.EPP
#	GROUP.GREENS,GROUP.GUENGL,GROUP.NI,GROUP.SD
#)

##################### score matrix used to process agreement
score.file <- "m3"					# see folder in/score
#thresh <- c(0,0)					# no thresholding at all
#thresh <- c(-0.34,+0.34)			# thresholds applied to agreement index values during network extraction (use c(0,0) for no filtering)
#thresh <- NA						# both thresholds automatically estimated (through k-means)


##################### formats of the generated plot (NA for screen -- mainly for debug)
#plot.formats <- c(
#		"svg" 
#	"jpg"
#	NA
#)

##################### configure parallel processing
#cn <- detectCores(all.tests=TRUE)
#if(!is.na(cn))
#	cl <- makeCluster(cn)		# automatically use all the available processors
#else

#cl <- makeCluster(5)		# manually set the number of processors to use
#registerDoParallel(cl)


#############################################################################################
# Load raw data
#############################################################################################
if(dataset.name=="VW")
{	data <- load.votewatch.data()
}else if(dataset.name=="IYP")
{	data <- load.itsyourparliament.data()
}else if(dataset.name=="PT")
{	data <- load.parltrack.data()
}


# ==========================================================================================================
# We will need this for node shapes when the mode is GROUP
regions = unlist(EU.REGION.FOR.STATE[data$mep.details[,COL.STATE]])
nb.col = length(colnames(data$mep.details))
data$mep.details = cbind(data$mep.details, regions)
colnames(data$mep.details)[nb.col+1]=COL.EU.REGION
# ==========================================================================================================



# considered vote types list
#FA = c(VOTE.FOR, VOTE.AGST)
FAA = c(VOTE.FOR, VOTE.AGST, VOTE.ABST)
#FAAAU = c(VOTE.FOR, VOTE.AGST, VOTE.ABST, VOTE.ABSENT, UNAVAILABLE.MEP.VOTE.ABSENT)
#cons.vote.types.list = list(FA=FA, FAA=FAA, FAAA=FAAA, FAAAU=FAAAU)
cons.vote.types.list = list(FAA=FAA)



# ========================================================================================
# a range of 'k' (i.e. nb cluster in roll-call clustering) values for further investigation.
# this does not mean that roll-call clustering is applied with these values. On the contrary,
# It is applied for all possible values of 'k'. One drawback for this approach is that we use fixed thresholds.
# When  Italy and France are treated at the same time, the same fixed threshold may not fit both countries.
# On the other hand, small values of k should be enough for the EP data, since there is not many EP political groups.
K.FOCUS.LIMITS = c(4,4)

# another way of choosing a subset of k values is to specify an epsilon value.
# Note that K.FOCUS.LIMITS and EPSILON are not mutual exclusive, the intersected k values are used.
EPSILON = NA # take all silhouette scores
#EPSILON = 0.10
#EPSILON = 0 # retreive only the best silhouette score and the associated partition
# ========================================================================================



# ==========================================================================================
# graph type when obtaining aggregated roll-call networks (i.e. before obtaining characteristic voting patterns)
aggrega.graph.type = "signed" # get signed graph
#aggrega.graph.type = "unsigned" # get unsigned graph
								 # when it is "unsigned", note that you may consider filtering aggregated graph, 
								 #    because it is dense, but, this is not parametrized, you need to change it in the code
								 # However, signed version should normally perform better (i.e. with ExCC), so no need to change it.

# These methods are only applied onto aggregated roll-call networks in order to identify
#   characteristic voting patterns.
# (see define-algos.R for defined correlation clustering algos)
# IMPORTANT: Note that if there is any algo in the vector 'comdet.algos' while the aggrega.graph.type = "signed",
#			 they it will be used. So, it is the user who should know if the given community detection method(s) can be applied
#			 onto signed graph.
corclst.algos=c(
		CORCLST.ALGO.ExCC
)

comdet.algos=c(
#		COMDET.ALGO.EDGEBETW
#		COMDET.ALGO.INFOMAP
#		COMDET.ALGO.LABELPROP
#		COMDET.ALGO.LOUVAIN
#		COMDET.ALGO.WALKTRAP
)
# ==========================================================================================


# "0.5" means removing MEPs which are absent at least for half of the considered roll-calls in each network
# "1" means not removing absent MEPS at all ==> the name 'absence.threshold' may be misleading here
#absence.thresholds = c(0.5, 1)
absence.thresholds = c(0.5)
#absence.thresholds = c(0)

##################### measures used to compare partitions
comp.measures <- c(
#		"nmi"
#		"rand"
#		"adjusted.rand", # change slightly the default index: normalize as (res+1)/2
#		"adjusted.rand2" # change slightly the default index: normalize as setting 0 to neg values in default adjusted.rand
		"F.purity"
)

# End of the section 'Init parameters'
# =============================================








##############################################################################################
## Extract all the rollcall-wise networks (for each country/group and each period)
##############################################################################################
extract.all.rollcall.networks(data$all.votes, data$rollcall.details, data$mep.details, score.file,
		domains, dates, everything, countries, groups, plot.formats)


for(i in 1:length(cons.vote.types.list)){
	cons.vote.types = cons.vote.types.list[[i]]
	vote.desc = names(cons.vote.types.list)[i]
	
	##############################################################################################
	## Partitioning all the rollcall-wise networks (for each country/group and each period)
	##############################################################################################
	partition.all.rollcall.networks(data$all.votes, data$rollcall.details, data$mep.details, score.file,
			domains, dates, everything, countries, groups, plot.formats = c(""), cons.vote.types)

	
	
	#############################################################################################
	## Clustering roll-calls
	##############################################################################################
	cluster.all.rollcalls(data$rollcall.details, score.file, domains, dates, everything, countries, groups,
			measures=comp.measures, clu.algo.name=KMEDOIDS, cons.vote.types, EPSILON, K.FOCUS.LIMITS)


#	###########################################################################################
#	# Make some statistics about themes after clustering roll-calls
#	# ==> You can use these methods when themes associated to the roll-calls are extracted in the input files.
#	##########################################################################################
#	# Make Post roll-call Cluster Analysis by theme
#	make.all.post.rollcall.clu.analysis.by.theme(data$rollcall.details, score.file, domains, dates, everything,
#	 	countries, groups, measures=comp.measures, clu.algo.name=KMEDOIDS, cons.vote.types, EPSILON, K.FOCUS.LIMITS)
#	# Compare All roll-call Cluster Results by theme
#	compare.all.rollcall.clu.results.by.theme(data$rollcall.details, score.file, domains, dates, everything,
#	 	countries, groups, measures=comp.measures, clu.algo.name=KMEDOIDS, cons.vote.types, EPSILON, K.FOCUS.LIMITS)
	

	
	#############################################################################################
	# Aggregating roll-call networks by cluster
	############################################################################################
	aggregate.all.rollcall.networks.by.cluster(data$all.votes, data$rollcall.details, data$mep.details,
			score.file, clu.algo.name=KMEDOIDS, domains, dates, everything,
			countries, groups, measures=comp.measures, cons.vote.types, EPSILON, K.FOCUS.LIMITS,
			aggrega.graph.type, plot.formats, absence.thresholds)
	
	
	
	##############################################################################################
	## Partition aggregated roll-call networks, i.e. identifying characteristic voting patterns
	###############################################################################################	
	partition.all.aggregated.rollcall.networks(score.file, corclst.algos, comdet.algos, domains, dates, 
			everything, countries, groups, plot.formats=c("svg"), measures=comp.measures, cons.vote.types, 
			EPSILON, K.FOCUS.LIMITS, aggrega.graph.type, absence.thresholds)

	
	##############################################################################################
	## (optional) Compare the partitions obtained for the aggregated roll-call networks
	##############################################################################################
	compare.all.aggregated.rollcall.network.clusters(score.file, corclst.algos, comdet.algos, domains, dates, everything, countries, groups,
			measures=comp.measures, cons.vote.types, EPSILON, K.FOCUS.LIMITS, aggrega.graph.type, absence.thresholds)

}


##################### stop parallel processing
#stopCluster(cl)


tlog("Done!")
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
# Notes
# =======
# - The program is tested under Linux/ubuntu. And some bash functionalities are used.
# - This code is the continuity of the following repository: https://github.com/CompNet/NetVotes
# - The current code is in the following repository: https://github.com/CompNet/MultiNetVotes
# - user should set corrrectly 'CIRCOS_CMD' in "circos.R"
# - user should set corrrectly  'CPLEX.BIN.PATH' in "define-algos.R", e.g. "/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/"
# - Do not test it with 'everything'. 1) it would make less sense. 2) it will be computationally expensive
# - Use 'plot.formats=c()' to go faster in processing
# - Depeding on the size of the graph, the signed graph partitioning method which solves the Correlation Clustering
# 		problem optimally, called 'ExCC', may take time, and it may require large memory (e.g. 32 Gb)
#	ExCC requires a .G graph format in input. 
#	User should download the ExCC code from its github repostiroty, and then compile it in order to get a executable file.
#		This executable should be placed under 'lib/ExCC/'.
# - The code should not be run in parallel mode while performing some partitioning task
#		(so, only in the method 'partition.all.aggregated.rollcall.networks()'). Because, ExCC is already run in parallel mode.
# - The input files can be downloaded from: https://figshare.com/articles/NetVotes_2017_-_iKnow_17/5785833.
#		The only needed data is the folder named 'itsyourparliament.zip'. Unzip it, then place it under 'in/'.
#		These input files are processed ,before everything, in order to generate files having more compact form.
#		Those files will be generated under 'in/_overall' 
#

# Warnings
# =======
# - the code can become proper by increasing the usage of  constant variables (e.g. using IMAGE.SVG instead of 'svg', file paths)
# I would not find trh gender data relted to MEPs, so we do not into account during the XML file processing to generate
#	files lated in the folder '_overall'
# - in clustering process: we use the term "faction" in the paper for the k-medoids results.
#		However, in the code, we did not distinguish between clustering of a single rollcall netork 
#			and roll-call clustering (based on all considered roll-calls) with the associated dissimilarty matrix.
# - A post-processing may be re quired for the membership vector, depending on the used partitioning method.
#		For instance, ExCC may not handle zero degreed-nodes, and may put inside a cluster.
#		So far, we do no handle/adjust any membership vector in the code. However, we handle it for Circos image files

# Problems
# =========
# - The parameter FORCE is used to force processing when the required data already exists (to regenerate). 
#	 	But, currently it is not handled systematically in the code, so it is simetimes in use, and sometimes it will not work.

# TODOs
# ======
# - One may notice that the time periods we propose may not be appropriate in the multiplex network analysis.
#		Maybe, it would be better if we analyze the roll-calls within a context (e.g. CAP-related, Brexit-related, etc.)
#		instead of 2009-10, 2010-11, etc.
# - In the paper, we have identified the themes only for AGRI roll-calls and for 2012-13 period. For others, we de not have any.
#		We can identify for all the roll-calls. However, the problem is that we realized that identifying themes
#		associated to the roll-calls was not a good idea. Instead, we need to do text analysis to understand the context of the considered roll-calls.
# 		=> Currently, I did not include the identified themes in 'rollcall-details.csv', since I wanted that input files
#			are generated from XML input files.
#		The current code takes the themes into consideration with the following configuration:
#			* (optional) The  column name 'isAmendment' will be inserted into 'in/_overall/rollcall-details.csv',
#				which indicates if the considered roll-call is an amendment (1) or not (0). A binary value.
#			* 'Theme Id' column name will be inserted into 'in/_overall/rollcall-details.csv',
#				which indicates the theme id of the considered roll-call. An integer value. This id should be defined in 
#			'in/_overall/rollcall-theme-details.csv'. An example of this file is included in 'in/_overall/'.
#			* The structure of 'in/_overall/rollcall-theme-details.csv' is as follows:
#				- The column name 'Id': the id of the theme among all the themes (across all domains)
#				- The theme 'Id': the theme id in the current domain (e.g. AGRI)
#				- The column name 'Theme Name'
#				- The column name 'Domain Id' (e.g. AGRI)
# - In the current implementation, we use only k-medoids as roll-call clustering method. 
#		So this is a 'k'-parameter dependent. You may try out other 'k'-dependent methods, or 'k'-free methods.
#


# nohup R --vanilla < src/main.R > terminal.output.txt &
