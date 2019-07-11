#############################################################################################
# These functions build the normalized names of the algorithms handled in MultiNetVotes.
# 
# 03/2017 Vincent Labatut
# 04/2019 Nejat Arinik
#############################################################################################


#############################################################################################
# Unsigned networks partitioning algorithms info
#############################################################################################
COMDET.ALGO.VALUES <- c()
COMDET.ALGO.NAMES <- c()
COMDET.ALGO.EDGEBETW <- "EB"
	# this implementation uses the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.EDGEBETW)
	COMDET.ALGO.NAMES[COMDET.ALGO.EDGEBETW] <- "EdgeBetweenness"
COMDET.ALGO.INFOMAP <- "IM"
	# this implementation uses the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.INFOMAP)
	COMDET.ALGO.NAMES[COMDET.ALGO.INFOMAP] <- "InfoMap"
COMDET.ALGO.LABELPROP <- "LP"
	# this implementation uses the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.LABELPROP)
	COMDET.ALGO.NAMES[COMDET.ALGO.LABELPROP] <- "LabelPropagation"
COMDET.ALGO.LOUVAIN <- "LV"
	# this implementation uses the weights, if present, but cannot use directions
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.LOUVAIN)
	COMDET.ALGO.NAMES[COMDET.ALGO.LOUVAIN] <- "Louvain"
COMDET.ALGO.WALKTRAP <- "WT"
	# this implementation uses the weights, if present, and simply ignores directions
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.WALKTRAP)
	COMDET.ALGO.NAMES[COMDET.ALGO.WALKTRAP] <- "WalkTrap"
comdet.algo.ncg.value <- function(value) # returns the negative complementary value (i.e. short code) associated to the specified (positive) value
	{	if(length(value)==0)
			res <- c()
		else
			res <- paste("NCG",value,sep="-")
		return(res)
	}
for(value in COMDET.ALGO.VALUES) COMDET.ALGO.NAMES[comdet.algo.ncg.value(value)] <- paste("NCG",COMDET.ALGO.NAMES[value])
#TODO maybe we should allow parameters there too? (ie for regular community detection methods)



# =================================
EXE.DIR = "lib/ExCC"
CORCLST.ALGO.ExCC <- "ExCC"
ExCC.RESULT.FILENAME = "ExCC-result.txt" # Dont change that
ExCC.JAR.PATH = paste(EXE.DIR,"cplex-partition.jar",sep="/") # gaia cluster - CERI
#CPLEX.BIN.PATH = "/users/narinik/Cplex/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/"
CPLEX.BIN.PATH = "/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/"
CLU.NO.FOR.ALL.ISOLATED.NODES = 0
# =================================









#############################################################################################
# It builds the ExCC command to be executed in order to obtain graph partition.
#
# input.folder: input folder path where input grpah file is located
# out.folder: output folder path where the ExCC result will be recorded.
#
# return: ExCC command
#############################################################################################
get.ExCC.command <- function(input.folder, out.folder)
{
	
	input.file = paste("'", input.folder, "/", SIGNED.FILE, ".G", "'", sep="")
#	output.file = paste("'", out.folder, "/", ExCC.RESULT.FILENAME, "'", sep="")
	
	# An example:
	# java -Djava.library.path=/users/narinik/Cplex/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/
	# 	-DinFile=data/test.G -DoutDir=. -jar exe/cplex-partition.jar
	
	cmd = 
		paste(
				"java",		
				paste("-Djava.library.path=", CPLEX.BIN.PATH, sep=""),
				paste0("-DinFile=", input.file),
				paste0("-DoutDir='", out.folder, "'"),
#				"-Dcp=false",
#				"-DenumAll=false",
#				"-Dtilim=-1",
#				"-DlazyInBB=false",
#				"-DuserCutInBB=false",
#				"-DMaxTimeForRelaxationImprovement=-1",
				"-jar",
				ExCC.JAR.PATH,
				sep=" "
		)
	
	print(cmd)

	return(cmd)
}






#############################################################################################
# Returns the full name based on the normalized (short) name. Note that for parameterized 
# algorithms, this will just return a clean version of the short name, since it contains 
# the parameter values.
#
# algo.names: short names of the considered algorithms.
#
# returns: the corresponding full names, to be used in plots for instance.
#############################################################################################
get.algo.names <- function(algo.names)
{	result <- c()
	
	for(algo.name in algo.names)
	{	# no parameters
		if(algo.name %in% names(COMDET.ALGO.NAMES))
			result <- c(result, COMDET.ALGO.NAMES[algo.name])
		# parameters
		else
			result <- c(result, gsub(pattern="_", replacement=" ", x=algo.name, fixed=TRUE))
	}
	
	return(result)
}



#############################################################################################
# Returns the inline command for the specified algorithm. The "..." parameters are fetched
# to the algorithm-specific function.
#
# algo.name: short code associated to the algorithm, including its parameter values.
#
# returns: the command allowing to invoke the program externally.
#############################################################################################
get.algo.commands <- function(algo.names, ...)
{	result <- c()
		
	for(algo.name in algo.names)
	{	
#		if(startsWith(algo.name,COMDET.ALGO.ILS))
#			result <- c(result, get.ils.command(algo.name, ...))
#		else if(startsWith(algo.name,COMDET.ALGO.GRASP))
#			result <- c(result, get.grasp.command(algo.name, ...))
#		else if(startsWith(algo.name,COMDET.ALGO.KMBS))
#			result <- c(result, get.kmbs.command(algo.name, ...))
		if(algo.name == CORCLST.ALGO.ExCC)
			result <- c(result, get.ExCC.command(...))
	}
	
	return(result)
}
