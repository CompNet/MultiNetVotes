

source("src/prepare-data/load-common.R")
source("src/define-constants.R")


#############################################################################################
# Folder names
#############################################################################################
# "It's your parliament" data
IYP.FOLDER <- file.path(IN.FOLDER,"itsyourparliament")
# MEP data
IYP.MEPS.FOLDER <- file.path(IYP.FOLDER,"meps")
# Vote data
IYP.VOTES.FOLDER <- file.path(IYP.FOLDER,"votes")
# Policy areas data
IYP.DOMAINS.FOLDER <- file.path(IYP.FOLDER,"domains")


#############################################################################################
# File names
#############################################################################################
# XML list of domains
IYP.DOMAIN.LIST.FILE <- file.path(IYP.DOMAINS.FOLDER,"_domains.xml")
# Activity periods associated to the MEPs
IYP.MEP.PERIODS.FILE <- file.path(IYP.MEPS.FOLDER,"_mep-periods.csv")
# Genders  of the MEPs
IYP.MEP.GENDERS.FILE <- file.path(IYP.MEPS.FOLDER,"_mep-genders.csv")
# Duplicate MEPs (IYT contains certain MEPs twice, we need to merge them)
IYP.MEP.DUPLICATES.FILE <- file.path(IYP.MEPS.FOLDER,"_duplicates.csv")


#############################################################################################
# XML elements and attributes
#############################################################################################
# mep details
IYP.ELT.MEPID		<- "mepid"		# IYP MEP id (internal to the DB, different from both the official ID and that of NetVotes)
IYP.ELT.MEPNAME		<- "mepname"
IYP.ELT.FULLNAME	<- "fullname"
IYP.ELT.COUNTRY		<- "country"
IYP.ELT.TITLE		<- "title"
IYP.ELT.PARTY		<- "party"
IYP.ELT.BIRTHDATE	<- "birth"
IYP.ELT.BIRTHPLACE	<- "birthplace"
IYP.ELT.EP_ID		<- "europarlid"	# official EP id of the MEP
IYP.ELT.GROUP		<- "group"
# vote details
IYP.ELT.VOTE.TITLE	<- "votetitle"
IYP.ELT.FULL.TITLE	<- "fulltitle"
IYP.ELT.POLICY.AREA	<- "policyarea"
IYP.ELT.DOC.REF		<- "docref"
IYP.ELT.EP.REF		<- "epref"
IYP.ELT.REPORTER.ID	<- "reporterid"
IYP.ELT.VOTE.DATE	<- "date"
IYP.ELT.MEP.VOTE	<- "mepvote"
IYP.ELT.VOTES		<- "votes"
# domain details
IYP.ELT.COMMITTEE	<- "committee"
IYP.ELT.ID			<- "id"
IYP.ELT.POLICY.NAME	<- "policyarea_name"
IYP.ELT.VOTEID		<- "voteid"


#############################################################################################
# Domain mapping
#############################################################################################
# map used to convert official domain names into VoteWatch ones
DOMAIN.IYP2SYMB <- c()
DOMAIN.IYP2SYMB["Constitutional Affairs"] <- DOMAIN.AFCO
DOMAIN.IYP2SYMB["Foreign Affairs"] <- DOMAIN.AFET
DOMAIN.IYP2SYMB["Agriculture and Rural Development"] <- DOMAIN.AGRI
DOMAIN.IYP2SYMB["Budgets"] <- DOMAIN.BUDG
DOMAIN.IYP2SYMB["Budgetary Control"] <- DOMAIN.CONT
DOMAIN.IYP2SYMB["Culture and Education"] <- DOMAIN.CULT
DOMAIN.IYP2SYMB["Development"] <- DOMAIN.DEVE
DOMAIN.IYP2SYMB["Women's Rights and Gender Equality"] <- DOMAIN.FEMM
DOMAIN.IYP2SYMB["Economic and Monetary Affairs"] <- DOMAIN.ECON
DOMAIN.IYP2SYMB["Employment and Social Affairs"] <- DOMAIN.EMPL
DOMAIN.IYP2SYMB["Environment, Public Health and Food Safety"] <- DOMAIN.ENVI
DOMAIN.IYP2SYMB["Internal Market and Consumer Protection"] <- DOMAIN.IMCO
DOMAIN.IYP2SYMB["International Trade"] <- DOMAIN.INTA
DOMAIN.IYP2SYMB["Industry, Research and Energy"] <- DOMAIN.ITRE
DOMAIN.IYP2SYMB["Legal Affairs"] <- DOMAIN.JURI
DOMAIN.IYP2SYMB["Civil Liberties, Justice and Home Affairs"] <- DOMAIN.LIBE
DOMAIN.IYP2SYMB["Fisheries"] <- DOMAIN.PECH
DOMAIN.IYP2SYMB["Petitions"] <- DOMAIN.PETI
DOMAIN.IYP2SYMB["Regional Development"] <- DOMAIN.REGI
DOMAIN.IYP2SYMB["Internal regulations of the EP"] <- DOMAIN.RIPE
DOMAIN.IYP2SYMB["Transport and Tourism"] <- DOMAIN.TRAN


#############################################################################################
# Vote mapping
#############################################################################################
# note: no "Didn't vote", "Absent" or "Documented Absence" like with VoteWatch
VOTE.IYP2SYMB <- c()
VOTE.IYP2SYMB["For"] <- VOTE.FOR
VOTE.IYP2SYMB["Abstention"] <- VOTE.ABST
VOTE.IYP2SYMB["Against"] <- VOTE.AGST


#############################################################################################
# Group mapping
#############################################################################################
GROUP.IYP2SYMB <- c()
GROUP.IYP2SYMB["ALDE"] <- GROUP.ALDE
GROUP.IYP2SYMB["ECR"] <- GROUP.ECR
GROUP.IYP2SYMB["EFD"] <- GROUP.EFD
GROUP.IYP2SYMB["PPE"] <- GROUP.EPP
GROUP.IYP2SYMB["Verts/ALE"] <- GROUP.GREENS
GROUP.IYP2SYMB["GUE/NGL"] <- GROUP.GUENGL
GROUP.IYP2SYMB["NI"] <- GROUP.NI
GROUP.IYP2SYMB["SD"] <- GROUP.SD










#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################








#############################################################################################
# Constants used when downloading the IYP data from the website.
#############################################################################################
# list of the effective MEP ids
IYP.MEP.IDS		<- 1:870#[-c(325,587,627,724,739,746,758,759,760,793,794,795)]
# list of the effective vote ids
IYP.VOTE.IDS	<- 1:7513
# list of the effective domain ids
IYP.DOMAIN.IDS	<- 26:58#[-c(32,45,49,50,52)]
# URLs
# MEP URL
IYP.URL.MEP		<- "http://itsyourparliament.eu/api/mep.php"
# Vote URL
IYP.URL.VOTE	<- "http://www.itsyourparliament.eu/api/vote.php"
# List of domains URL
IYP.URL.DOMAINS	<- "http://itsyourparliament.eu/api/policyareas.php"
# Domain URL
IYP.URL.DOMAIN	<- "http://itsyourparliament.eu/api/policyarea.php"
# ID parameter
IYP.URL.ID		<- "?id="
# offical europarl URL for reports
IYP.URL.REPORTS	<- "http://www.europarl.europa.eu/sides/getDoc.do?type=REPORT&mode=XML&reference="
# offical europarl URL for motions
IYP.URL.MOTIONS	<- "http://www.europarl.europa.eu/sides/getDoc.do?type=MOTION&mode=XML&reference="
# language suffix for the official europarl website
IYP.URL.LANG.SUFFIX	<- "&language=EN"
# offical europarl URL for MEP individual pages
IYP.URL.MEP	<- "http://www.europarl.europa.eu/meps/en/"
# suffix for "history of parliamentary service"
IYP.URL.HIST.SUFFIX	<- "_history.html"



#############################################################################################
# Files used when retrieving information from the WWW
#############################################################################################
# Official Europarl XML list of MEPs (retrieved from Europarl: http://www.europarl.europa.eu/meps/en/xml.html?query=full&filter=all&leg=0 )
IYP.EP.MEP.LIST.FILE <- file.path(IYP.MEPS.FOLDER,"_meps.xml")
