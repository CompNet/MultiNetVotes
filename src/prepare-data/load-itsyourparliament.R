#############################################################################################
# This script processes the raw data exported from the Itsyourparliament website, in order to 
# extract some tables the main script can then use.
# 
# 11/2015 Vincent Labatut
# 04/2019 Nejat Arinik => I replaced 'doc.details' to 'rollcall.details' everywhere.
#				  			 Especially, the output should have the following attribute:
#								result$rollcall.details (not result$doc.details)
#					   => there was a bug in the method 'iyp.extract.meps.details()':
# 							Retreiving periods of MEP presence (from '_mep-periods.csv') was handled in a cleanr way.
#							Because, in IYP, EP Id is not always present.
#							Also, I disabled GENDER information, because I could not find where to extract this info.
#############################################################################################
library("XML")
library("stringr")


source("src/prepare-data/define-consts-itsyourparliament.R")




#############################################################################################
# Retrieves the full title of a document (it is sometimes missing or imprecise in the IYP
# data) thanks to the Europarl website (official website of the European Parliament).
#
# title: IYP title of the document (which includes its Europarl ID).
#
# returns: the title retrieved from the Europarl website, or NA if none could be found.
#############################################################################################
ep.retrieve.fulltitle <- function(title)
{	fulltitle <- NA
	
	# extract the document Europarl ID from the title
	tlog("..title='",title,"'")
	# possibly clean the beginning of the title
	if(!startsWith(title,"A7") && !startsWith(title,"B7") && !startsWith(title,"RC"))
	{	idx <- str_locate(title,fixed("A7"))[1]
		if(is.na(idx))
			idx <- str_locate(title,fixed("B7"))[1]
		if(is.na(idx))
			idx <- str_locate(title,fixed("RC"))[1]
		if(!is.na(idx))
			title <- substr(title,idx,nchar(title))
	}
	prefix <- substr(title,1,2)
	tlog("....prefix='",prefix,"'")
	# known Europarl ID
	if(prefix %in% c("A7","B7","RC"))
	{	title <- gsub(" - ", "-", title)
		# build the appropriate request URL for A7- or B7-type ids
		if(prefix=="A7" | prefix=="B7") # A7-0144/2014
		{	while(substr(title,9,9)=="/")	# sometimes, too many digits in the doc number
				title <- paste0(substr(title,1,7),substr(title,9,nchar(title)))
			number <- substr(title,4,7)
			tlog("....number='",number,"'")
			year <- substr(title,9,12)
			tlog("....year='",year,"'")
			reference <- paste0(prefix,"-",year,"-",number,sep="")
			tlog("..reference='",reference,"'")
			if(prefix=="A7")
				ep.url <- paste0(IYP.URL.REPORTS,reference,IYP.URL.LANG.SUFFIX)
			else if(prefix=="B7")
				ep.url <- paste0(IYP.URL.MOTIONS,reference,IYP.URL.LANG.SUFFIX)
			tlog("..url='",ep.url,"'")
		}
		# build the appropriate request URL for RC-type ids
		else #if(prefix=="RC") # RC-B7-0693/2011
		{	if(startsWith(title,"RC7"))
				title <- gsub("RC7-", "RC-", title)
			prefix2 <- substr(title,4,5)
			tlog("....prefix2='",prefix2,"'")
			number <- substr(title,7,10)
			tlog("....number='",number,"'")
			year <- substr(title,12,15)
			tlog("....year='",year,"'")
			reference <- paste0("P7","-",prefix,"-",year,"-",number)
			tlog("..reference='",reference,"'")
			ep.url <- paste0(IYP.URL.MOTIONS,reference,IYP.URL.LANG.SUFFIX)
			tlog("..url='",ep.url,"'")
		}
		# request the Europarl server
		ep.page <- readLines(ep.url)
		# identify the full title
		
		idx <- which(startsWith(ep.page,"<title>"))
		fulltitle <- substr(ep.page[idx],8,nchar(ep.page[idx])-8)
		
#		idx <- str_locate(ep.page, fixed("doc_subtitle_level1"))[1]
#print(ep.page[1:100])		
#		idx2 <- which(apply(idx,1,function(v) !all(is.na(v))))
#print(ep.page[idx2:(idx2+10)])		
#		# a title could be found
#		if(length(idx2)>0)
#		{	# retrieve the title
#			start.idx <- idx2 + 1
#			fulltitle <- ep.page[start.idx]
#			if(startsWith(fulltitle,"<"))
#			{	d <- str_locate(fulltitle,fixed(">"))[1] + 1
#				fulltitle <- substr(fulltitle,start=d,stop=nchar(fulltitle))
#				f <- str_locate(fulltitle,fixed("</"))[1] - 1
#				fulltitle <- substr(fulltitle,start=1,stop=f)
#			}
#			temp.idx <- start.idx + 1
#			temp <- ep.page[temp.idx]
#			while(!startsWith(temp,"</td>"))
#			{	if(startsWith(temp,"<"))
#				{	d <- str_locate(temp,fixed(">"))[1] + 1
#					temp <- substr(temp,start=d,stop=nchar(temp))
#					f <- str_locate(temp,fixed("</"))[1] - 1
#					temp <- substr(temp,start=1,stop=f)
#				}
#				if(!is.na(temp) && nchar(temp)>0)
#					fulltitle <- paste(fulltitle,temp)
#				temp.idx <- temp.idx + 1
#				temp <- ep.page[temp.idx]
#			}
#			tlog("..title'",fulltitle,"'")
#		}
#		# no title found
#		else
#			tlog("..WARNING: could not find the full title")
		tlog("..title=",fulltitle)
	}
	# unknown Europarl ID
	else
	{	tlog("..WARNING: prefix not recognized, skipping this one")
	}
	
	return(fulltitle)
}













#############################################################################################
# Checks if the specified date belongs to one of the periods represented in the specified
# string. The format of this string is xx/xx/xxxx:xx/xx/xxxx::yy/yy/yyyy:yy/yy/yyyy. A NA for
# the end date represents no limit.
#
# periods: string representing a list of temporal period, using the above format.
# date: date of interest (a date object, not a string)
#
# returns: TRUE iff the date belongs to at list one period.
#############################################################################################
iyp.check.date <- function(periods, date)
{	# possible convert the date parameter to an actual date object
	if(is.character(date))
		date <- as.Date(date,"%d/%m/%Y")
	#print(date)
	
	# init variables
	result <- FALSE
	rest <- gsub("NA", "00/00/0000", periods)
	
	# process each period
	while(nchar(rest)>0 && !result)
	{	# get the period
		period <- substr(rest,1,21)
		if(nchar(rest)>=23)
			rest <- substr(rest,24,nchar(rest))
		else
			rest <- ""
		#tlog("Period: ",period," rest=",rest)
		
		# get the dates
		start.date.str <- substr(period,1,10)
		end.date.str <- substr(period,12,21)
		#print(start.date.str);print(end.date.str)
		start.date <- as.Date(start.date.str,"%d/%m/%Y")
		if(end.date.str=="00/00/0000")
			result <- date>=start.date
		else
		{	end.date <- as.Date(end.date.str,"%d/%m/%Y")
			# check if the considered date belongs to the period
			result <- date>=start.date && date<=end.date
		}
	}
	
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified MEP id, and returns the corresponding
# vector of strings.
#
# mep.id: ID of the MEP (in IYP).
#
# returns: string vector representing the MEP details.
#############################################################################################
iyp.extract.mep.details <- function(mep.id)
{	tlog("....Processing MEP ",mep.id)
	result <- c()
	
	# retrieve XML document
	file <- file.path(IYP.MEPS.FOLDER,paste0(mep.id,".xml"))
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# MEP id in NetVotes
	result[IYP.ELT.MEPID] <- mep.id
	
	# names
	fullname <- str_trim(xml[[IYP.ELT.FULLNAME]])
	lastname <- str_trim(xml[[IYP.ELT.MEPNAME]])
#	idx <- str_locate(fullname,fixed(lastname,ignore_case=TRUE))[1]-2
	#idx <- str_locate(fullname,ignore.case(lastname))[1]-2
	#firstname <- substr(fullname,1,idx)
	firstname = gsub(paste0(" ",lastname),"",fullname)
	#firstname = gsub(" ","",firstname) # remove whitespaces  ========>
	if(!is.na(lastname) & lastname=="")
		lastname <- NA
	result[COL.LASTNAME] <- lastname
	if(!is.na(firstname) & firstname=="")
		firstname <- NA
	result[COL.FIRSTNAME] <- firstname
	if(!is.na(fullname) & fullname=="")
		fullname <- NA
	result[COL.FULLNAME] <- fullname
	
	# state
	state <- str_trim(xml[[IYP.ELT.COUNTRY]])
	if(!is.na(state) & state=="")
		state <- NA
	result[COL.STATE] <- state
	
	# european political group
	group <- GROUP.IYP2SYMB[str_trim(xml[[IYP.ELT.GROUP]])]
	if(!is.na(group) & group=="")
		group <- NA
	result[COL.GROUP] <- group
	
	# MEP title
	title <- str_trim(xml[[IYP.ELT.TITLE]])
	if(!is.na(title) & title=="")
		title <- NA
	result[COL.TITLE] <- title
	
	# european party
	party <- str_trim(xml[[IYP.ELT.PARTY]])
	if(!is.na(party) & party=="")
		party <- NA
	result[COL.PARTY] <- party
	
	# MEP date of birth
	birthdate <- str_trim(xml[[IYP.ELT.BIRTHDATE]])
	if(!is.na(birthdate) & birthdate=="")
		birthdate <- NA
	result[COL.BIRTHDATE] <- birthdate
	
	# MEP place of birth
	birthplace <- str_trim(xml[[IYP.ELT.BIRTHPLACE]])
	if(!is.na(birthplace) & birthplace=="")
		birthplace <- NA
	result[COL.BIRTHPLACE] <- birthplace
	
	# official MEP ID in the european parliament
	ep.id <- str_trim(xml[[IYP.ELT.EP_ID]])
	if(!is.na(ep.id) & ep.id=="")
		ep.id <- NA
	result[COL.EP.ID] <- ep.id
	
	return(result)
}



#############################################################################################
# Reads the XML files corresponding to all the MEP ids, and returns the corresponding
# string table. Also adds some information retrieved from Europarl, the official European
# Parliament website.
#
# duplicate.meps: matrix describing the duplicate MEPs (id of the original on the 1st column
#				  id of the copy on the 2nd one).
#
# returns: string array representing the MEP details.
#############################################################################################
iyp.extract.meps.details <- function(duplicate.meps)
{	tlog("..Retrieving the MEPs details")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(MEP.DETAILS.FILE))
	{	result <- as.matrix(read.csv2(MEP.DETAILS.FILE,check.names=FALSE))
		result[,COL.MEPID] <- as.integer(result[,COL.MEPID])
		result[,IYP.ELT.MEPID] <- as.integer(result[,IYP.ELT.MEPID])
		result[,COL.EP.ID] <- as.integer(result[,COL.EP.ID])
	}
	
	# otherwise, build the table and record it
	else
	{	# retrieve the list of MEP ids
		files <- list.files(path=IYP.MEPS.FOLDER, full.names=FALSE, no..=TRUE)
		mep.ids <- c()
		for(file in files)
			mep.ids <- c(mep.ids,substr(file,1,str_locate(file,".xml")-1))
		mep.ids <- sort(as.integer(mep.ids))
		
		duplicate.meps <- cbind(duplicate.meps, rep(NA,nrow(duplicate.meps)))
		
		# build the matrix
		cols <- c(COL.MEPID, COL.LASTNAME, COL.FIRSTNAME,
			COL.FULLNAME, COL.STATE, COL.GROUP, COL.TITLE,
			COL.PARTY, COL.BIRTHDATE, COL.BIRTHPLACE, COL.EP.ID,
			IYP.ELT.MEPID, COL.PERIODS)
		result <- matrix(NA,nrow=length(mep.ids)-nrow(duplicate.meps),ncol=length(cols))
		colnames(result) <- cols
		idx <- 1
		tlog("!!!!!!!!!!! Extracting MEP details from XML files !!!!!!!!!")
		for(i in 1:length(mep.ids))
		{	# get the MEP data
			data <- iyp.extract.mep.details(mep.ids[i])
			# if second occurrence of a duplicate
			if(mep.ids[i] %in% duplicate.meps[,2])
			{	r <- which(duplicate.meps[,2]==mep.ids[i])
				tlog("Duplicate detected: ",mep.ids[i]," vs. ",duplicate.meps[r,1])
				for(c in colnames(result))
				{	if(is.na(result[duplicate.meps[r,3],c]))
					{	if(is.na(data[c]))
							tlog(c,": both fields are NA >> no change")
						else
						{	result[duplicate.meps[r,3],c] <- data[c]
							tlog(c,": NA vs. ",data[c]," >> ",data[c])
						}
					}
					else
					{	if(is.na(data[c]))
							tlog(c,": ",result[duplicate.meps[r,3],c]," vs. NA >> no change")
						else
							tlog(c,": ",result[duplicate.meps[r,3],c]," vs. ",data[c]," >> no change")
					}
				}
			}
			# otherwise
			else
			{	# if first occurrence of a duplicate
				if(mep.ids[i] %in% duplicate.meps[,1])
				{	r <- which(duplicate.meps[,1]==mep.ids[i])
					duplicate.meps[r,3] <- idx
				}
				# and in any case
				data[COL.MEPID] <- idx
				result[idx,cols] <- data[cols]
				idx <- idx + 1
			}
		}

		# retrieve the official list of MEPs activity periods
		ep.table <- as.matrix(read.csv2(IYP.MEP.PERIODS.FILE,check.names=FALSE))
		ep.table[,COL.EP.ID] <- as.integer(ep.table[,COL.EP.ID])
		Encoding(ep.table[,COL.FULLNAME]) <- "UTF-8"
		ep.table[,COL.FULLNAME] <- toupper(ep.table[,COL.FULLNAME])
		# for each MEP in the IYP table, add the official info from the second table
		#fullnames <- toupper(paste(result[,COL.FIRSTNAME],result[,COL.LASTNAME]))
		fullnames <- toupper(result[,COL.FULLNAME])
		
		# ==================================
		# first: match by EP id
		idx  <- match(result[,COL.EP.ID],ep.table[,COL.EP.ID])	
		result[,COL.PERIODS] <- ep.table[idx,COL.PERIODS]

		# second: match by fullname (in IYP, EP Id is not always present)
		na.indx = which(is.na(result[,COL.EP.ID]))
		idx  <- match(fullnames,ep.table[,COL.FULLNAME])
		result[na.indx,COL.EP.ID] <- ep.table[idx,COL.EP.ID][na.indx]
		result[na.indx,COL.PERIODS] <- ep.table[idx,COL.PERIODS][na.indx]
		# ==================================

		#print(cbind(result[,COL.EP.ID],ep.table[idx,COL.EP.ID],result[,COL.EP.ID]==ep.table[idx,COL.EP.ID]))

		
#		# retrieve the MEPs gender (manually added, based on firstnames and pictures available on the Europarl website)
#		#TODO not tested (directly added manually in the current out/_overall file)
#		ep.table <- as.matrix(read.csv2(IYP.MEP.GENDERS.FILE,check.names=FALSE))
#		ep.table[,COL.EP.ID] <- as.integer(ep.table[,COL.EP.ID])
#		Encoding(ep.table[,COL.LASTNAME]) <- "UTF-8"
#		# for each MEP in the IYP table, add the gender from the second table
#		idx  <- match(result[,COL.EP.ID],ep.table[,COL.EP.ID])
#		result[,COL.GENDER] <- ep.table[idx,COL.GENDER]
		
		# record matrix
		write.csv2(result,file=MEP.DETAILS.FILE,row.names=FALSE)
	}
	
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified domain id, and returns the corresponding
# vector of vote ids.
#
# domain.id: ID of the domain (in IYP).
#
# returns: vector of vote ids.
#############################################################################################
iyp.extract.domain <- function(domain.id)
{	tlog("....Processing domain ",domain.id)
	
	# retrieve XML document
	file <- file.path(IYP.DOMAINS.FOLDER,paste0(domain.id,".xml"))
	doc <- readLines(file, warn = FALSE)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
	
	# process each listed vote
	result <- rep(NA,length(xml))
	for(i in 1:length(xml))
	{	vote <- xml[[i]]
		vote.id <- str_trim(vote[[IYP.ELT.VOTEID]])
		result[i] <- vote.id
	}
	
	return(result)
}



#############################################################################################
# Read the XML file listing the domains, then process each domain, and returns a vector representing
# the policy domain associated to each voted rollcall.
#
# returns: a vector associating a vote id to the corresponding policy domain.
#############################################################################################
iyp.extract.domains <- function()
{	tlog("..Retrieving the domain details")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# if the file already exists, just load it
	if(file.exists(ROLLCALL.DOMAINS.FILE))
	{	result <- as.matrix(read.csv2(ROLLCALL.DOMAINS.FILE,check.names=FALSE))
		result[,IYP.ELT.VOTEID] <- as.integer(result[,IYP.ELT.VOTEID])
	}
	
	# otherwise, build the table and record it
	else
	{	# get the ids of the effective domains
		files <- list.files(path=IYP.DOMAINS.FOLDER, full.names=FALSE, no..=TRUE)
		dom.ids <- c()
		for(file in files)
			dom.ids <- c(dom.ids,substr(file,1,str_locate(file,".xml")-1))
		dom.ids <- suppressWarnings(as.integer(dom.ids))
		dom.ids <- dom.ids[!is.na(dom.ids)]
		dom.ids <- sort(dom.ids)
		
		# retrieve the main XML document
		doc <- readLines(IYP.DOMAIN.LIST.FILE)
		xml.data <- xmlParse(doc)
		xml <- xmlToList(xml.data)
	
		# init table and vote list
		cols <- c(IYP.ELT.ID, IYP.ELT.COMMITTEE, IYP.ELT.POLICY.NAME)
		domains <- matrix(NA,nrow=length(xml),ncol=length(cols))
		colnames(domains) <- cols
		result <- NULL
		
		# process each domain
		for(i in 1:length(xml))
		{	# update table
			domain <- xml[[i]]
			dom.id <- str_trim(domain[[IYP.ELT.ID]])
			
			# update details table
			domains[i,IYP.ELT.ID] <- dom.id
			domains[i,IYP.ELT.COMMITTEE] <- str_trim(domain[[IYP.ELT.COMMITTEE]])
			domains[i,IYP.ELT.POLICY.NAME] <- str_trim(domain[[IYP.ELT.POLICY.NAME]])
			
			# update vote list
			if(dom.id %in% dom.ids)
			{	vote.ids <- iyp.extract.domain(domains[i,IYP.ELT.ID])
				domain.code <- DOMAIN.IYP2SYMB[domains[i,IYP.ELT.POLICY.NAME]]
				result <- rbind(result,
					cbind(vote.ids, rep(domain.code,length(vote.ids))))
			}
		}
		
		# finalize and record result vector
		colnames(result) <- c(IYP.ELT.VOTEID,COL.DOMID)
		result <- result[order(as.integer(result[,IYP.ELT.VOTEID])),]
		write.csv2(result,file=ROLLCALL.DOMAINS.FILE,row.names=FALSE)
		
		# record domain details table (not needed, just for information)
		write.csv2(domains,file=DOMAIN.DETAILS.FILE,row.names=FALSE)
	}
		
	return(result)
}



#############################################################################################
# Read the XML file corresponding to the specified vote id, and returns the corresponding
# details and MEP vote values.
#
# vote.id: ID of the vote (in IYP).
# duplicate.meps: matrix describing the duplicate MEPs (id of the original on the 1st column
#				  id of the copy on the 2nd one).
#
# returns: a list containing the vote information (details) and the MEP vote values (votes).
#############################################################################################
iyp.extract.vote <- function(vote.id, duplicate.meps)
{	# retrieve XML document
	file <- file.path(IYP.VOTES.FOLDER,paste0(vote.id,".xml"))
	doc <- readLines(file)
	xml.data <- xmlParse(doc)
	xml <- xmlToList(xml.data)
#	print(xml)	
	
	# extract vote information
	details <- c()
	details[IYP.ELT.VOTEID] <- vote.id
	
	# document title
	title <- str_trim(xml[[IYP.ELT.VOTE.TITLE]])
	if(!is.na(title) & title=="")
		title <- NA
	details[COL.TITLE] <- title
	
	# different title (not always present)
	full.title <- str_trim(xml[[IYP.ELT.FULL.TITLE]])
	if(!is.na(full.title) & full.title=="")
		full.title <- NA
	details[COL.FULL.TITLE] <- full.title
	
	# =================================================
	# if title is not recognizable in terms of doc reference, it puts 'NA'
	# sometimes it throws an unexpected exception: it can not connect to a URL
	# Also, in order to go faster, you can comment this line
	details[COL.RET.TITLE] = tryCatch(ep.retrieve.fulltitle(title), error=function(e) NA)
	# =================================================

	
	# id of the policy domain
	dom.id <- str_trim(xml[[IYP.ELT.POLICY.AREA]])
#	if(!is.na(dom.id) & dom.id=="")
#		dom.id <- DOMAIN.AUTR
#	else
		dom.id <- DOMAIN.IYP2SYMB[dom.id]
	details[COL.DOMID] <- dom.id
tlog("'",dom.id,"' >> ",dom.id)
	
	# document reference
	doc.ref <- str_trim(xml[[IYP.ELT.DOC.REF]])
	if(!is.na(doc.ref) & doc.ref=="")
		doc.ref <- NA
	details[COL.DOC.REF] <- doc.ref
	
	# european parliament official reference
	ep.ref <- str_trim(xml[[IYP.ELT.EP.REF]])
	if(!is.na(ep.ref) & ep.ref=="")
		ep.ref <- NA
	details[COL.EP.REF] <- ep.ref
	
	# id of the reporter of the document
	reporter.id <- str_trim(xml[[IYP.ELT.REPORTER.ID]])
	if(!is.na(reporter.id) & reporter.id=="")
		reporter.id <- NA
	details[COL.REPORTER.ID] <- reporter.id
	
	# date of the vote
	date <- str_trim(xml[[IYP.ELT.VOTE.DATE]])	
	if(!is.na(date) & date=="")
		date <- NA
	details[COL.DATE] <- format(as.Date(date,"%Y-%m-%d"),"%d/%m/%Y")
	
	# extract vote values
	votes <- c()
	for(i in 1:length(xml[[IYP.ELT.VOTES]]))
	{	v <- xml[[IYP.ELT.VOTES]][[i]]
		mep.id <- str_trim(v[[IYP.ELT.MEPID]])
		vote.value <- VOTE.IYP2SYMB[str_trim(v[[IYP.ELT.MEP.VOTE]])]
		r <- which(duplicate.meps[,2]==as.integer(mep.id))
		# not a duplicate MEP, or first occurrence of a duplicate
		if(length(r)==0)
			votes[as.character(mep.id)] <- vote.value
		# second occurrence of a duplicate MEP
		else
		{	if(!is.na(vote.value))
			{	fst <- as.character(duplicate.meps[r,1])
				if(length(votes[fst])==0)
					votes[fst] <- vote.value
				else if(is.na(votes[fst]))
					votes[fst] <- vote.value
				else
				{	tlog("Problem: conflicting votes when merging duplicate MEPs (",fst,"vs. ",mep.id,"): ",votes[fst]," vs. ",vote.value)
					votes[fst] <- vote.value
				}
			}
			
		}
	}
	
	# process vote total result
	for.count <- length(votes==VOTE.FOR)
	against.count <- length(votes==VOTE.AGST)
	if(against.count>for.count)
		details[COL.RESULT] <- VOTE.FOR
	else
		details[COL.RESULT] <- VOTE.AGST
	
	result <- list(details=details, votes=votes)
#print(result)	
	return(result)
}



#############################################################################################
# Read the XML files corresponding to all the vote ids, and returns the corresponding
# vote data, as two tables: rollcall details and vote values.
#
# doc.domains: table containing the domains of the voted documents.
# mep.details: table with the information descriving the MEPs.
# duplicate.meps: matrix describing the duplicate MEPs (id of the original on the 1st column
#				  id of the copy on the 2nd one).
#
# returns: a list containing the document details (rollcall.details) annd the vote values (all.votes).
#############################################################################################
iyp.extract.votes <- function(rollcall.domains, mep.details, duplicate.meps)
{	tlog("..Extract vote-related data")
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	result <- list()
	
	# check if the files already exist, load everything
	if(file.exists(ALL.VOTES.FILE) & file.exists(ROLCALL.DETAILS.FILE))
	{	# vote values
		temp <- as.matrix(read.csv2(ALL.VOTES.FILE,check.names=FALSE))
		temp[,COL.MEPID] <- as.integer(temp[,COL.MEPID])
		result$all.votes <- temp
		# document details
		temp <- as.matrix(read.csv2(ROLCALL.DETAILS.FILE,check.names=FALSE))
		temp[,COL.ROLLCALL.ID] <- as.integer(temp[,COL.ROLLCALL.ID])
		result$rollcall.details <- temp
	}
	
	# otherwise, process everything
	else
	{	# retrieve the list of vote ids
		files <- list.files(path=IYP.VOTES.FOLDER, full.names=FALSE, no..=TRUE)
		vote.ids <- c()
		for(file in files)
			vote.ids <- c(vote.ids,substr(file,1,str_locate(file,".xml")-1))
		vote.ids <- sort(as.integer(vote.ids))
		
		# complete the list of rollcall domains (some docs were missing)
		rollcall.domains0 <- rollcall.domains
		rollcall.domains <- cbind(vote.ids,DOMAIN.AUTR)
		colnames(rollcall.domains) <- c(IYP.ELT.VOTEID, COL.DOMID)
		idx <- match(rollcall.domains0[,IYP.ELT.VOTEID],vote.ids)
		rollcall.domains[idx,] <- rollcall.domains0
#		print(rollcall.domains)
		
		# build details matrix
		details.cols <- c(COL.ROLLCALL.ID, IYP.ELT.VOTEID,
			COL.TITLE, COL.FULL.TITLE, COL.DOMID, COL.DOC.REF, COL.RET.TITLE,
			COL.EP.REF, COL.REPORTER.ID, COL.DATE)
		details.mat <- matrix(NA,nrow=length(vote.ids),ncol=length(details.cols))
		colnames(details.mat) <- details.cols
		
		# build vote values matrix
		votes.mat <- matrix(NA,nrow=nrow(mep.details),ncol=length(vote.ids))
		colnames(votes.mat) <- 1:length(vote.ids)
		
		# fill both matrices
#vote.ids <- vote.ids[vote.ids>=7065]
#i <- 4633
		for(i in 1:length(vote.ids))
		{	tlog("....Processing vote ",vote.ids[i]," (",i,"/",length(vote.ids),")")
			
			temp <- iyp.extract.vote(vote.ids[i], duplicate.meps)
#			print(temp)			
			# update details matrix
			temp$details[COL.ROLLCALL.ID] <- i
#			print(temp$details)			
#			print(temp$details[COL.DOMID])
#			print(rollcall.domains[i,COL.DOMID])
			#if(is.na(temp$details[COL.DOMID]))
			#	temp$details[COL.DOMID] <- rollcall.domains[i,COL.DOMID]
			#else if(temp$details[COL.DOMID]!=rollcall.domains[i,COL.DOMID])
			#	tlog("WARNING: domain is different in vote (",temp$details[COL.DOMID],") and domain (",rollcall.domains[i,COL.DOMID],") files")
			if(!is.na(rollcall.domains[i,COL.DOMID]))
			{	if(is.na(temp$details[COL.DOMID]))
					temp$details[COL.DOMID] <- rollcall.domains[i,COL.DOMID]
				else if(temp$details[COL.DOMID]!=rollcall.domains[i,COL.DOMID])
					tlog("....WARNING: domain is different in vote (",temp$details[COL.DOMID],") and domain (",rollcall.domains[i,COL.DOMID],") files")
			}
			else if(is.na(temp$details[COL.DOMID]))
				tlog("....WARNING: both domains in vote and domain table are missing")
			details.mat[i,details.cols] <- temp$details[details.cols]
			
			# update vote matrix
			idx <- match(as.integer(names(temp$votes)), mep.details[,IYP.ELT.MEPID])
			votes.mat[idx,i] <- temp$votes
			# differentiate absent MEPs and inactive ones (i.e. persons not holding a MEP position at the time of the vote)
			vote.date <- temp$details[COL.DATE]
			active <- sapply(mep.details[,COL.PERIODS],function(periods) iyp.check.date(periods,vote.date))
			nas <- is.na(votes.mat[,i])
			votes.mat[active & nas,i] <- VOTE.ABSENT # we consider active MEP who didn't vote as absent
		}

		# record details matrix
		write.csv2(details.mat,file=ROLCALL.DETAILS.FILE,row.names=FALSE)
		result$rollcall.details <- details.mat
		
		# record vote values
		votes.mat <- cbind(1:nrow(mep.details),votes.mat)
		colnames(votes.mat)[1] <- COL.MEPID
		write.csv2(votes.mat,file=ALL.VOTES.FILE,row.names=FALSE)
		result$all.votes <- votes.mat
	}
	
	#print(sort(unique(c(result$all.votes))))	
	return(result)
}



#############################################################################################
# Load all the tables and returns them as a list.
#
# returns: a list containing all the loaded tables.
#############################################################################################
load.itsyourparliament.data <- function()
{	tlog("***************************************************")
	tlog("****** LOAD IYP DATA")
	tlog("***************************************************")
	
	dir.create(OVERALL.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	result <- list()
	
	duplicate.meps <- as.matrix(read.csv2(IYP.MEP.DUPLICATES.FILE,check.names=FALSE))
	rollcall.domains <- iyp.extract.domains()
	result$mep.details <- iyp.extract.meps.details(duplicate.meps)
	temp <- iyp.extract.votes(rollcall.domains, result$mep.details, duplicate.meps)
	result$rollcall.details <- temp$rollcall.details
	result$all.votes <- temp$all.votes
	#result$group.lines <- extract.group.lines(result$all.votes, result$mep.details)
	#result$behavior.values <- process.behavior.values(result$all.votes, result$mep.details, result$group.lines)
	
	return(result)
}
