#############################################################################################
# Takes the list of roll-calls and filters it to extract only those corresponding to
# certain criteria:
# - Temporal: focus on a given period.
# - Topical: focus on certain policy domains.
#
# Also filters the list of MEPs depending on various criteria:
# - Geographic: their home state.
# - Political: their european political group.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
# 04/2019 Nejat Arinik
#############################################################################################
source("src/define-constants.R")



#############################################################################################
# Takes the table containing the roll-call details, as well as two dates, and returns the ids
# of the roll-calls matching the period between the dates (included). If one of the dates (or
# both of them) is NA, it is replaced by an extreme date supposed to include all data.
#
# rollcall.details: table containing the details of the voted roll-calls.
# start.date: starting date.
# end.date: ending date.
#
# returns: a vector of roll-call ids, corresponding to the roll-call matching the dates.
#############################################################################################
filter.rollcalls.by.date <- function(rollcall.details, start.date=NA, end.date=NA)
{	# possibly set extreme dates
	if(is.na(start.date))
		start.date <- "01/01/1900"
	if(is.na(end.date))
		end.date <- "31/12/3000"
	
	# possibly convert the start/end dates
	if(class(start.date)!="Date")
		start.date <- as.Date(start.date,"%d/%m/%Y")
	if(class(end.date)!="Date")
		end.date <- as.Date(end.date,"%d/%m/%Y")
	
	# retrieve and convert the date strings
	dates <- as.Date(rollcall.details[,COL.DATE],"%d/%m/%Y")
	
	# retain only the dates located between start and end (included)
	idx <- which(dates>=start.date & dates<=end.date)
	result <- rollcall.details[idx,COL.ROLLCALL.ID]
	
	return(result)
}


#############################################################################################
# Takes the table containing the roll-call details, as well as a vector of policy domains, 
# and returns the ids of the roll-calls matching one of the domains. If the domain vector is
# NA, or empty, then all domains are considered.
#
# rollcall.details: table containing the details of the voted roll-calls.
# domains: a vector of domains, or NA to use all keep domains.
#
# returns: a vector of roll-call ids, corresponding to the roll-call matching the domains.
#############################################################################################
filter.rollcalls.by.domain <- function(rollcall.details, domains=c())
{	# possibly add all domains
	if(length(domains)==1 && is.na(domains))
		domains <- c()
	if(length(domains)==0)
		domains <- DOMAIN.VALUES
	
	# retrieve the roll-call domains
	doms <- rollcall.details[,COL.DOMID]
	
	# retain only the domains matching one of those specified in the parameter vector
	idx <- match(doms,domains)
	idx <- which(!is.na(idx))
	result <- rollcall.details[idx,COL.ROLLCALL.ID]
		
	return(result)
}


#############################################################################################
# Takes the table containing the roll-call details, as well as a vector of policy domains, 
# a starting and an ending dates, and returns the ids of the roll-calls matching at the same
# time the period between the dates (included) and one of the domains. 
# 
# If one of the dates (or both of them) is NA, it is replaced by an extreme date supposed to 
# include all data. Same thing for the domain vector, if it is NA or empty, then all domains 
# are considered.
#
# rollcall.details: table containing the details of the voted roll-calls.
# start.date: starting date.
# end.date: ending date.
# domains: a vector of domains, or NA to use all keep domains.
#
# returns: a vector of roll-call ids, corresponding to the roll-call matching the criteria.
#############################################################################################
filter.rollcalls.by.date.and.domain <- function(rollcall.details, start.date, end.date, domains)
{	# filter by date
	ids1 <- filter.rollcalls.by.date(rollcall.details, start.date, end.date)
	# filter by domain
	ids2 <- filter.rollcalls.by.domain(rollcall.details, domains)
	
	# keep only the roll-calls appearing in both vectors
	result <- intersect(ids1,ids2)
	return(result)
}


#############################################################################################
# Takes the table containing the MEPs details, as well as a vector of coutries, 
# and returns the ids of the MEPs matching one of the countries. If the country vector is
# NA, or empty, then all countries are considered.
#
# mep.details: table containing the details of the MEPs.
# countries: a vector of countries, or NA to use all keep countries.
#
# returns: a vector of MEP ids, corresponding to those matching the countries.
#############################################################################################
filter.meps.by.country <- function(mep.details, countries)
{	# possibly add all countries
	if(length(countries)==1 && is.na(countries))
		countries <- c()
	if(length(countries)==0)
		countries <- COUNTRIES.VALUES
	
	# retrieve the MEPs countries
	cntrs <- mep.details[,COL.STATE]
	
	# retain only the countries matching one of those specified in the parameter vector
	idx <- match(cntrs,countries)
	idx <- which(!is.na(idx))
	result <- mep.details[idx,COL.MEPID]
	
	return(result)
}


#############################################################################################
# Takes the table containing the MEPs details, as well as a vector of political groups, 
# and returns the ids of the MEPs matching one of the groups. If the group vector is
# NA, or empty, then all groups are considered.
#
# mep.details: table containing the details of the MEPs.
# groups: a vector of groups, or NA to use all keep groups.
#
# returns: a vector of MEP ids, corresponding to the those matching the groups.
#############################################################################################
filter.meps.by.group <- function(mep.details, groups)
{	# possibly add all groups
	if(length(groups)==1 && is.na(groups))
		groups <- c()
	if(length(groups)==0)
		groups <- GROUP.VALUES
	
	# retrieve the MEPs groups
	cntrs <- mep.details[,COL.GROUP]
	
	# retain only the groups matching one of those specified in the parameter vector
	idx <- match(cntrs,groups)
	idx <- which(!is.na(idx))
	result <- mep.details[idx,COL.MEPID]
	
	return(result)
}


#############################################################################################
# Takes the table containing the MEP details, as well as a vector of countries and a vector
# of political groups, and returns the ids of the MEPs matching at the same time one of these
# countries and one of these groups. 
# 
# If one of the vector is NA or empty, all possible values are considered.
#
# mep.details: table containing the details of the MEPs.
# countries: a vector of countries, or NA to use all keep countries.
# groups: a vector of groups, or NA to use all keep groups.
#
# returns: a vector of MEP ids, corresponding to those matching the criteria.
#############################################################################################
filter.meps.by.country.and.group <- function(mep.details, countries, groups)
{	# filter by country
	ids1 <- filter.meps.by.country(mep.details, countries)
	# filter by group
	ids2 <- filter.meps.by.group(mep.details, groups)
	
	# keep only the MEPs appearing in both vectors
	result <- intersect(ids1,ids2)
	return(result)
}


#############################################################################################
# Test
#############################################################################################
#rollcall.ids <- filter.rollcalls.by.date(rollcall.details, start.date="07/10/2010", end.date="19/10/2010")
#print(rollcall.ids)
#rollcall.ids <- filter.rollcalls.by.domain(rollcall.details, domains=c("BUDG"))
#print(rollcall.ids)
#rollcall.ids <- filter.rollcalls.by.date.and.domain(rollcall.details, start.date="07/10/2010", end.date="19/10/2010", domains=c("BUDG"))
#print(rollcall.ids)
#rollcall.ids <- filter.rollcalls.by.date.and.domain(rollcall.details, start.date="07/10/2010", end.date="19/10/2010", domains=c("BUDG","CONT"))
#print(rollcall.ids)
