#############################################################################################
# These functions build file paths and names based on a series of parameters. They allow a 
# uniform access to the resources created by the rest of the scripts.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
# 04/2019 Nejat Arinik
#############################################################################################




#############################################################################################
# Builds a path for a file located in the "docnetworks" folder.
#
# score: name of the score table used to process the agreement index.
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
#
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.rollcall.networks.path <- function(score, country=NA, group=NA, domain)
{	result <- ROLLCALL.NETWORKS.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# country, group or everything (mutually exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	return(result)
}




#############################################################################################
# Builds a path for a file located in the "rollcall-partitions" folder.
#
# score: name of the score table used to process the agreement index. Ex: 'm3'
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
#
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.rollcall.partitions.path <- function(score, country=NA, group=NA, domain)
{	result <- ROLLCALL.PARTITIONS.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# country, group or everything (mutually exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	return(result)
}



#############################################################################################
# Builds a path for a file located in the "rollcall-clustering" folder.
# Ex: 'out/docclustering/m3/bycountry/France/AGRI'
# Ex: 'out/docclustering/m3/bycountry/France/AGRI/2012-13'
#
# score: name of the score table used to process the agreement index. Ex: 'm3'
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period.
#
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.rollcall.clustering.path <- function(score, country=NA, group=NA, domain, period=NA)
{	result <- ROLLCALL.CLUSTERING.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# country, group or everything (mutually exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	if(!(is.na(period)))
		result <- file.path(result,period)
	
	return(result)
}

#############################################################################################
# Builds a path for a file located in the "rollcall-clustering" folder where the description of vote types is specified.
# Ex: 'out/docclustering/m3/bycountry/France/AGRI/2012-13/votetypes=FAA'
#
# score: name of the score table used to process the agreement index. Ex: 'm3'
# vote.types.desc: Description of vote types used in the partition process of each document. Ex: 'FAA'
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period.
#
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.rollcall.clustering.vote.type.path <- function(score, vote.types.desc, country=NA, group=NA, domain, period)
{	
	rollcall.clu.path = get.rollcall.clustering.path(score, country, group, domain, period)
	result <- file.path(rollcall.clu.path,paste0("votetypes=",vote.types.desc))
	return(result)
}


#############################################################################################
# Builds a path for a file located in the "rollcall-clustering" folder where the description of vote types and
# the parameters of cluster analysis (i.e. k-medoids) are specified.
# Ex: 'out/docclustering/m3/bycountry/France/AGRI/2012-13/votetypes=FAA/F.purity-k=3-sil=0.65115'
#
# score: name of the score table used to process the agreement index. Ex: 'm3'
# vote.types.desc: Description of vote types used in the partition process of each document. Ex: 'FAA'
# silhouette.val: The silhouette value obtained in cluster analysis (i.e. k-medoids) for a given 'k' value
# k.val: The number of cluster to be detected in cluster analysis (i.e. k-medoids)
# measure: The (external) similarity measure used in dissimilarity matrix before cluster analysis (i.e. k-medoids).
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period (compulsory).
#
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.rollcall.clustering.vote.type.silhouette.val.path <- function(score, vote.types.desc, silhouette.val, k.val, measure,
		country=NA, group=NA, domain, period)
{	
	silhouette.val = round(silhouette.val, digits = 5)
	rollcall.clu.vote.type.path = get.rollcall.clustering.vote.type.path(score, vote.types.desc, country, group, domain, period)
	result <- file.path(rollcall.clu.vote.type.path, paste0(measure,"-k=",k.val,"-sil=",silhouette.val))
	
	return(result)
}









#############################################################################################
# Builds a path for a file located in the "agreement" folder.
#
# score: name of the score table used to process the agreement index. Ex: 'm3'
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
#
# returns: the appropriate path for a files in the "votes" folder.
#############################################################################################
get.agreement.path <- function(score, country=NA, group=NA, domain)
{	result <- AGREEMENT.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# country, group or everything (mutually exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	return(result)
}

#############################################################################################
# Builds a path for a file located in the "networks" folder.
#
# score: name of the score table used to process the agreement index. Ex: 'm3'
# thresh: thresholds used for network extraction (vector of two values).
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period.
#
# returns: the appropriate path for a files in the "networks" folder.
#############################################################################################
get.networks.path <- function(score, thresh=NA, country=NA, group=NA, domain, period)
{	result <- NETWORKS.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# positive and negative thresholds (can be NA if no threhsold)
	result <- file.path(result,paste0("negtr=",thresh[1],"_postr=",thresh[2]))
	
	# country, group or everything (mutually exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	# time period (should not be NA)
#	if(!is.na(period))
		result <- file.path(result,DATE.STR.T7[period])
	
	return(result)
}

#############################################################################################
# Builds a path for a file located in the "partitions" folder.
#
# score: name of the score table used to process the agreement index (compulsory). Ex: 'm3'
# thresh: thresholds used for network extraction (vector of two values).
# country: considered member state (optional).
# group: considered political group (optional).
# domain: considered domain of activity (compulsory).
# period: considered time period (optional).
# repetition: repetition number for the partitioning algorithm (NA for no repetition at all).
#
# returns: the appropriate path for a file in the "networks" folder.
#############################################################################################
get.partitions.path <- function(score, thresh=NA, country=NA, group=NA, domain, period=NA, repetition=NA)
{	result <- PARTITIONS.FOLDER
	
	# score table (should not be NA)
#	if(!is.na(score))
	result <- file.path(result,score)
	
	# positive and negative thresholds (can be NA if no threhsold)
	result <- file.path(result,paste0("negtr=",thresh[1],"_postr=",thresh[2]))
	
	# country, group or everything (mutually exclusive)
	if(!is.na(country))
		result <- file.path(result,"bycountry",country)
	else if(!is.na(group))
		result <- file.path(result,"bygroup",group)
	else
		result <- file.path(result,"everything")
	
	# domain (should not be NA)
	if(!(is.na(domain)))
		result <- file.path(result,domain)
	
	# time period
	if(!is.na(period))
		result <- file.path(result,DATE.STR.T7[period])
	
	# repetition number (partitioning algos can be applied several times to the same data)
	if(!is.na(repetition))
		result <- file.path(result,repetition)
	
	return(result)
}
