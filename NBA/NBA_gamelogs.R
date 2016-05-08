#setwd(...)
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

Teams <- c("DEN","MIN","OKC","POR","UTA","BOS","BKN","NYK","PHI","TOR","GSW","LAC","LAL","PHX","SAC","CHI","CLE","DET","IND","MIL","DAL","HOU","MEM","NOP","SAS","ATL","CHA","MIA","ORL","WSH")
Players <- NULL

#LOOP TO PULL IN CURRENT ROSTERS AND ESPN IDs
for (i in 1:length(Teams)) 
  {
    assign("url", paste0("http://espn.go.com/nba/team/roster/_/name/",Teams[1]))
    doc <- htmlTreeParse(url, useInternalNodes = T)
    links <- getHTMLLinks(doc, externalOnly = TRUE, xpQuery = "//a/@href")
    team_links <- links[35:65]
    team_links <- team_links[grepl("player", team_links)]
    roster <- ldply(strsplit(team_links, "/"))
    roster$V9 <- gsub("-", " ", roster$V9)
    tbl <- roster[,9:8]
    colnames(tbl)[1] <- "NAME"
    colnames(tbl)[2] <- "ID"
    tbl$TEAM <- Teams[i]
    tbl$NAME <- sapply(tbl$NAME, toupper)
    Players <- rbind(Players,tbl)
    Sys.sleep(3)
  }


#LOOP TO GRAB GAMELOGS FOR EACH PLAYER
gamelogs <- NULL
for (i in 1:length(Players$NAME))  
  { 
    url = paste0("http://espn.go.com/nba/player/gamelog/_/id/", Players$ID[i])
    #url <- paste0(url,"/year/2013/")  <--Set year
    tbl <- NULL
    page <- htmlParse(url)
    tableNodes <- getNodeSet(page, "//table")
      tbl = readHTMLTable(tableNodes[[2]], colClasses = c("character"),stringsAsFactors = FALSE)
    colnames(tbl) <- tbl[1,]
    tbl <- tbl[grep("/", tbl[,1]), ]
    tbl$NAME <- Players$NAME[i]
    gamelogs <- rbind(gamelogs, tbl)
    Sys.sleep(8)
  }



#### NEEDS PROOFING ###
#FORMATTING FOR GAMELOG DATA

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
