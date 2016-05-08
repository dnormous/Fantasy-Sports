setwd("C:/")
getwd()

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("matrixStats")
library("lpSolve")
library("plyr")
library("reshape")
library("zoo")

#source("NBA_DailySched_2013.R") 

DEN_link <- "http://espn.go.com/nba/team/roster/_/name/den/"
MIN_link <- "http://espn.go.com/nba/team/roster/_/name/min/"
OKC_link <- "http://espn.go.com/nba/team/roster/_/name/okc/"
POR_link <- "http://espn.go.com/nba/team/roster/_/name/por/"
UTA_link <- "http://espn.go.com/nba/team/roster/_/name/utah/"
BOS_link <- "http://espn.go.com/nba/team/roster/_/name/bos/" 
BKN_link <- "http://espn.go.com/nba/team/roster/_/name/bkn/"
NYK_link <- "http://espn.go.com/nba/team/roster/_/name/ny/"
PHI_link <- "http://espn.go.com/nba/team/roster/_/name/phi/"
TOR_link <- "http://espn.go.com/nba/team/roster/_/name/tor/"
GSW_link <- "http://espn.go.com/nba/team/roster/_/name/gs/"
LAC_link <- "http://espn.go.com/nba/team/roster/_/name/lac/"
LAL_link <- "http://espn.go.com/nba/team/roster/_/name/lal/"
PHX_link <- "http://espn.go.com/nba/team/roster/_/name/phx/"
SAC_link <- "http://espn.go.com/nba/team/roster/_/name/sac/"
CHI_link <- "http://espn.go.com/nba/team/roster/_/name/chi/"
CLE_link <- "http://espn.go.com/nba/team/roster/_/name/cle/"
DET_link <- "http://espn.go.com/nba/team/roster/_/name/det/"
IND_link <- "http://espn.go.com/nba/team/roster/_/name/ind/"
MIL_link <- "http://espn.go.com/nba/team/roster/_/name/mil/"
DAL_link <- "http://espn.go.com/nba/team/roster/_/name/dal/"
HOU_link <- "http://espn.go.com/nba/team/roster/_/name/hou/"
MEM_link <- "http://espn.go.com/nba/team/roster/_/name/mem/"
NOP_link <- "http://espn.go.com/nba/team/roster/_/name/no/"
SAS_link <- "http://espn.go.com/nba/team/roster/_/name/sa/"
ATL_link <- "http://espn.go.com/nba/team/roster/_/name/atl/"
CHA_link <- "http://espn.go.com/nba/team/roster/_/name/cha/"
MIA_link <- "http://espn.go.com/nba/team/roster/_/name/mia/"
ORL_link <- "http://espn.go.com/nba/team/roster/_/name/orl/"
WSH_link <- "http://espn.go.com/nba/team/roster/_/name/wsh/"


Team_links <- c(DEN_link,MIN_link,OKC_link,POR_link,UTA_link,BOS_link,BKN_link,NYK_link,PHI_link,TOR_link,GSW_link,LAC_link,LAL_link,PHX_link,SAC_link,CHI_link,CLE_link,DET_link,IND_link,MIL_link,DAL_link,HOU_link,MEM_link,NOP_link,SAS_link,ATL_link,CHA_link,MIA_link,ORL_link,WSH_link)
Teams <- c("DEN","MIN","OKC","POR","UTA","BOS","BKN","NYK","PHI","TOR","GSW","LAC","LAL","PHX","SAC","CHI","CLE","DET","IND","MIL","DAL","HOU","MEM","NOP","SAS","ATL","CHA","MIA","ORL","WSH")

##################
# Adjust by team (might be constant once season starts...)
rosterno <- c(55,55,54,55,55,54,55,55,55,55,54,55,55,54,55,53,55,55,54,55,55,55,53,55,55,55,55,55,55,55)
##################

for (i in 1:length(Team_links))  
{ 
url = Team_links[i]
doc = htmlTreeParse(url, useInternalNodes = T)
links <- getHTMLLinks(doc, externalOnly = TRUE, xpQuery = "//a/@href")
team_links <- NULL
team_links$link = links[41:rosterno[i]]             
list <- strsplit(team_links$link, "/")
df <- ldply(list)
df$V9 <- gsub("-", " ", df$V9)
tbl <- df[,9:8]
colnames(tbl)[1] <- "NAME"
colnames(tbl)[2] <- "ID"
tbl$TEAM <- Teams[i]
#tbl$NAME <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tbl$NAME, perl=TRUE)
#tbl$NAME <- gsub("(j.)","\\U\\1",tbl$NAME,perl=TRUE)
tbl$NAME <- sapply(tbl$NAME, toupper)
assign(paste0("", Teams[i]), tbl)

}

for (i in 1:length(Team_links))  
{ 
  url = Team_links[i]
  html.page <- htmlParse(url)
  tableNodes <- getNodeSet(html.page, "//table")
  tbl = readHTMLTable(tableNodes[[1]], colClasses = c("character"),stringsAsFactors = FALSE)
  colnames(tbl) <- tbl[1,]
  tbl <- tbl[-1,]
  tbl$NAME <- sapply(tbl$NAME, toupper)
  tbl$NAME <- gsub("-", " ", tbl$NAME)
  assign(paste0(Teams[i],"_ROS"), tbl)
}


Teamlist <- list(DEN,MIN,OKC,POR,UTA,BOS,BKN,NYK,PHI,TOR,GSW,LAC,LAL,PHX,SAC,CHI,CLE,DET,IND,MIL,DAL,HOU,MEM,NOP,SAS,ATL,CHA,MIA,ORL,WSH)
ROSlist <- list(DEN_ROS,MIN_ROS,OKC_ROS,POR_ROS,UTA_ROS,BOS_ROS,BKN_ROS,NYK_ROS,PHI_ROS,TOR_ROS,GSW_ROS,LAC_ROS,LAL_ROS,PHX_ROS,SAC_ROS,CHI_ROS,CLE_ROS,DET_ROS,IND_ROS,MIL_ROS,DAL_ROS,HOU_ROS,MEM_ROS,NOP_ROS,SAS_ROS,ATL_ROS,CHA_ROS,MIA_ROS,ORL_ROS,WSH_ROS)


for (i in 1:length(Teams))  
{ assign(paste0(Teams[i],""), (merge(ROSlist[i],Teamlist[i],by = "NAME", all.x=TRUE, all.y=TRUE))) }



Players <- rbind(DEN,MIN,OKC,POR,UTA,BOS,BKN,NYK,PHI,TOR,GSW,LAC,LAL,PHX,SAC,CHI,CLE,DET,IND,MIL,DAL,HOU,MEM,NOP,SAS,ATL,CHA,MIA,ORL,WSH)
Players$ID <- as.numeric(Players$ID)

Players$Nodes=ifelse(Players$ID>9999, "1","2")
Players$Nodes=ifelse(Players$ID==6633, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==6634, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==6639, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==6503, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==6602, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==3442, "2", Players$Nodes)
Players$Nodes=ifelse(Players$ID==4025, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==4218, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==3969, "1", Players$Nodes)
Players$Nodes=ifelse(Players$ID==2968439, "2", Players$Nodes)
Players$Nodes=ifelse(Players$ID==2528779, "2", Players$Nodes)
Players$Nodes=ifelse(Players$ID==2528353, "2", Players$Nodes)
Players$Nodes=ifelse(Players$ID==2488958, "2", Players$Nodes)
Players$Nodes=ifelse(Players$ID==2527963, "2", Players$Nodes)


#shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )

#substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

ALL_DATA <- NULL

for (i in 1:length(Players$NAME))  
{ 

  url = paste0("http://espn.go.com/nba/player/gamelog/_/id/", Players$ID[i])

##############################################################################################################################################
#Change stat year (2014,2013,2012,2011)

url <- paste0(url,"/year/2013/")

##############################################################################################################################################
  
tbl <- NULL
html.page <- htmlParse(url)
tableNodes <- getNodeSet(html.page, "//table")
x <- as.numeric(Players$Nodes[i])
tbl = readHTMLTable(tableNodes[[x]], colClasses = c("character"),stringsAsFactors = FALSE)

ifelse(tbl[1,1] == "DATE", "ok", next)
 
colnames(tbl) <- tbl[1,]
tbl <- tbl[-1,]

tbl$DAY <- str_sub(tbl$DATE, end=str_locate(string=tbl$DATE, '\\/')[,1]-2)
tbl$DATE4 <- str_sub(tbl$DATE, end=str_locate(string=tbl$DATE, '\\/')[,1]+4)
tbl <- tbl[complete.cases(tbl$DATE4),]
list <- strsplit(tbl$DATE4, " ")
tblx <- ldply(list)
colnames(tblx) <- c("DAY", "DATE")
tbl$DATE <- tblx$DATE
tbl$DAY <- tblx$DAY

tbl$DATE2[1:length(tbl$DATE2)] <- tbl$DATE[2:(1+(length(tbl$DATE2)))]
tbl$DATE2[1:length(tbl$DATE2)] <- tbl$DATE[2:(1+(length(tbl$DATE2)))]
tbl$DATE2 <- as.Date(tbl$DATE2,"%m/%d")

tbl$DATE2[is.na(tbl$DATE2)] <- "2013-10-01"

tbl$DATE <- as.Date(tbl$DATE,"%m/%d")
tbl$DATE2 <- as.Date(tbl$DATE2,"%m/%d")
tbl$DATE3 <- tbl$DATE2 + 1
tbl$B2B <- ifelse(tbl$DATE==tbl$DATE3, "B2B","O")
tbl$REST <- abs(tbl$DATE2 - tbl$DATE+1)

#tbl$DATE <- ifelse(tbl$DATE < (as.Date("2013-07-01","%Y-%m-%d")), tbl$DATE, (tbl$DATE-365))
#tbl$DATE <- as.Date(tbl$DATE)                           
                   
tbl$PLYD <- ifelse(tbl$MIN>0, "1","0")

tbl$WL <- str_sub(tbl$SCORE, end=str_locate(string=tbl$SCORE, '\\ ')[,1]-1)
tbl$SCORE <- substring(tbl$SCORE, 3)
listx <- strsplit(tbl$SCORE, "-")
tblx <- ldply(listx)
colnames(tblx) <- c("AWAYSCORE", "HOMESCORE")
tbl$AWAYSCORE <- as.numeric(tblx$AWAYSCORE)
tbl$HOMESCORE <- as.numeric(tblx$HOMESCORE)
tbl$TOTALSCORE <- rowSums(tbl[,c("AWAYSCORE","HOMESCORE")])

colnames(tbl)[5] <- "FG"
colnames(tbl)[7] <- "TFG"
colnames(tbl)[9] <- "FT"

listx <- strsplit(tbl$FG, "-")
tblx <- ldply(listx)
colnames(tblx) <- c("FGM", "FGA")
tbl$FGM <- tblx$FGM
tbl$FGA <- tblx$FGA

listx <- strsplit(tbl$TFG, "-")
tblx <- ldply(listx)
colnames(tblx) <- c("TPM", "TPA")
tbl$TPM <- tblx$TPM
tbl$TPA <- tblx$TPA

listx <- strsplit(tbl$FT, "-")
tblx <- ldply(listx)
colnames(tblx) <- c("FTM", "FTA")
tbl$FTM <- tblx$FTM
tbl$FTA <- tblx$FTA

tbl$COURT <- substring(tbl$OPP, 1, 2)
tbl$COURT <- ifelse(tbl$COURT=="vs", "HOME","AWAY")
tbl$OPP <- substring(tbl$OPP, 3)

tbl$NAME <- Players$NAME[i]
tbl$NAME <- sapply(tbl$NAME, toupper)
tbl$TEAM <- Players$TEAM[i]

tbl$TEAMSCORE <- ifelse(tbl$COURT == "HOME",tbl$HOMESCORE, tbl$AWAYSCORE)
tbl$OPPSCORE <- ifelse(tbl$COURT == "AWAY",tbl$HOMESCORE, tbl$AWAYSCORE)
tbl$PTDIFF <- tbl$TEAMSCORE - tbl$OPPSCORE  

tbl <- merge(tbl, Players,by = "NAME", all.x=TRUE, all.y=FALSE )

#Convert column classes to numeric for fantasy point calculation
tbl[,12] <- as.numeric(tbl[,12])
tbl[,13] <- as.numeric(tbl[,13])
tbl[,14] <- as.numeric(tbl[,14])
tbl[,15] <- as.numeric(tbl[,15])
tbl[,16] <- as.numeric(tbl[,16])
tbl[,17] <- as.numeric(tbl[,17])
tbl[,18] <- as.numeric(tbl[,18])
tbl$FGM <- as.numeric(tbl$FGM)
tbl$FGA <- as.numeric(tbl$FGA)
tbl$FTM <- as.numeric(tbl$FTM)
tbl$FTA <- as.numeric(tbl$FTA)

#FPTS conversion
tbl$F_PTS <- tbl$PTS*1
tbl$F_REB <- tbl$REB*1.25
tbl$F_AST <- tbl$AST*1.5
tbl$F_STL <- tbl$STL*2
tbl$F_BLK <- tbl$BLK*2
tbl$F_TO <- tbl$TO*-1  
tbl$F_MFT <- (tbl$FTA-tbl$FTM)*-0.5
tbl$F_MFG <- (tbl$FGA-tbl$FTM)*-0.5
tbl$FPTS <- tbl$F_PTS + tbl$F_REB + tbl$F_AST + tbl$F_STL + tbl$F_BLK + tbl$F_TO + tbl$F_MFT + tbl$F_MFG

tbl <- tbl[c(1,41,42,36,25,2,3,26,35,37,38,39,5,23,24,7,9,11,12,13,14,15,16,17,18,29,30,31,32,33,34,50,51,52,53,54,55,56,57,58,59,19,27,28,48)] 

  
#ADVANCED STATS OR MODEL COULD GO HERE  
  
  
  
#Select data start date.  Leave "#" if you want all data.  
#tbl <- tbl[tbl$DATE >= "2013-10-18",]  
 
#Put all game stats from all player together in one table
ALL_DATA <- rbind(ALL_DATA,tbl)

#Create data table of stats for each player
assign(paste0("", Players$NAME[i]), tbl)

}



#ggplot(ALL_DATA$NAME, aes(x=DATE, y=FPTS, color=NAME, group=NAME)) + geom_line()+ geom_text(aes(label=NAME),hjust=-0.05, vjust=-0.1)

#write.csv(ALL_DATA, file = "NBA_ALL_DATA2012.csv")

