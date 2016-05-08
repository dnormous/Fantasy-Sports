library("XML")
library("stringr")
library("httr")
library("lpSolve")
library("data.table")

loc <- "OFFICE"   #HOME or OFFICE

if(loc == "HOME"){
  H.FPROS <- read.csv(paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Pick Sheets/Fpros/FprosHitters_",Sys.Date(),".csv"))
  ODDS <- read.csv(paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Odds/ODDS_",Sys.Date(),".csv"))
  H.STATS <- read.csv(paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Stats/STATS_",Sys.Date(),".csv"))  
  P.STATS <- read.csv(paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Stats/P_STATS_",Sys.Date(),".csv")) 
  P.FPROS <- read.csv(paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Pick Sheets/Fpros/FprosPitchers_",Sys.Date(),".csv"))
}

if(loc == "OFFICE"){
  H.FPROS <- read.csv(paste0("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/Pick Sheets/Fpros/FprosHitters_",Sys.Date(),".csv"))
  ODDS <- read.csv(paste0("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/Odds/ODDS_",Sys.Date(),".csv"))
  H.STATS <- read.csv(paste0("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/Stats/STATS_",Sys.Date(),".csv")) 
  P.STATS <- read.csv(paste0("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/Stats/P_STATS_",Sys.Date(),".csv"))
  P.FPROS <- read.csv(paste0("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/Pick Sheets/Fpros/FprosPitchers_",Sys.Date(),".csv"))
}

#Start table with data from FPROS
Draft.tbl <- H.FPROS[,c(2,15,16,3:8, 13)]
Draft.tbl$Name <- toupper(Draft.tbl$Name)
Draft.tbl$OppPitcher <- toupper(Draft.tbl$OppPitcher)

#Format team abbrevs and add ODDS to tbl
Draft.tbl$Team <- as.character(Draft.tbl$Team)
Draft.tbl$Team[Draft.tbl$Team =="CWS"] <- "CHW"
Draft.tbl$Opp <- as.character(Draft.tbl$Opp)
Draft.tbl$Opp[Draft.tbl$Opp =="CWS"] <- "CHW"
Draft.tbl <- merge(Draft.tbl, ODDS[,c(2,3,5,7)], by.x="Team", by.y="TEAM", all.x=T, all.y=F)

#Match up differing player names between H.STATS and Draft.tbl
Draft.tbl$Name[Draft.tbl$Name =="JOSE DARIEL ABREU"] <- "JOSE ABREU"
Draft.tbl$Name[Draft.tbl$Name =="NORICHIKA AOKI"] <- "NORI AOKI"

#Add HSTATS to tbl
Draft.tbl <- merge(Draft.tbl, H.STATS, by.x=c("Name", "Team"), by.y=c("NAME", "TEAM"), all.x=T, all.y=F)

#Create columns for wOBA, fdOBA, & ABHR by pitcher arm
Draft.tbl$wOBA <- ifelse(Draft.tbl$Arm == "L", Draft.tbl$wOBA_vL, Draft.tbl$wOBA_vR)
Draft.tbl$fdOBA <- ifelse(Draft.tbl$Arm == "L", Draft.tbl$fdOBA_vL, Draft.tbl$fdOBA_vR)
Draft.tbl$ABHR <- ifelse(Draft.tbl$Arm == "L", Draft.tbl$ABHR_vL, Draft.tbl$ABHR_vR)

#Trim unused Hstats from Draft.tbl
Draft.tbl <- Draft.tbl[,c(1:13, 283:285)]

#Add PSTATS to tbl
Draft.tbl <- merge(Draft.tbl, P.STATS, by.x=c("OppPitcher", "Opp"), by.y=c("NAME", "TEAM"), all.x=T, all.y=F)

#Create columns for wOBA, fdOBA, & ABHR by pitcher arm
Draft.tbl$ERA <- Draft.tbl$ERA_LY
Draft.tbl$WHIP <- Draft.tbl$WHIP_LY
Draft.tbl$WHIP.ARM <-ifelse(Draft.tbl$Arm == "L", Draft.tbl$WHIP_vL, Draft.tbl$WHIP_vR) 
Draft.tbl$K9 <- Draft.tbl$K.9_LY
Draft.tbl$K9.ARM <-ifelse(Draft.tbl$Arm == "L", Draft.tbl$K.9_vL, Draft.tbl$K.9_vR) 

#Trim unused Hstats from Draft.tbl
Draft.tbl <- Draft.tbl[,c(3:16,1,2,348,349,351,350,352)]

### ALSO USE FOR RANKING TEAM STATS ###
#Rank pitcher by K/9, WHIP & ERA, then merge with Draft.tbl
dt <- unique(Draft.tbl[,c(15,11,17,18,19)])
dt$K9.RNK <- ifelse(dt$K9 == "NA",NA,round(ave(-dt$K9, FUN=rank),0))
dt$WHIP.RNK <- ifelse(dt$WHIP == "NA",NA,round(ave(dt$WHIP, FUN=rank),0))
dt$ERA.RNK <- ifelse(dt$ERA == "NA",NA,round(ave(dt$ERA, FUN=rank),0))
dt$pRUNS.RNK <- ifelse(dt$ERA == "NA",NA,round(ave(dt$pRUNS, FUN=rank),0))

Draft.tbl <- merge(Draft.tbl, dt[,c(1,6:9)], by="OppPitcher")
Draft.tbl <- Draft.tbl[,c(2:6,1,7:25)]

######################
#  ADD TEAM HITTING STATS TO TABLE
#######################

#####################
### DRAFT SOLVER  ###
#####################

#Preserve Draft.tbl data by duplicating the dataframe
D.tbl <- Draft.tbl

#STEP ONE: remove players if rain chance is greater than 50%
D.tbl$MaxRain <- ifelse(Draft.tbl$MaxRain == "111", -111, Draft.tbl$MaxRain)
MaxRain <- 50  
D.tbl <- D.tbl[D.tbl$MaxRain < MaxRain,]


#STEP TWO: Identify 3-4 best pitchers
Pitchers <- unique(D.tbl[,c(6,7,16,2,17:25)])
colnames(Pitchers)[3] <- "TEAM"
colnames(Pitchers)[4] <- "Opp"
P.FPROS$Name <- toupper(P.FPROS$Name)
Pitchers <- merge(Pitchers, P.FPROS[,c(2,14,11,12)], by.x = c("OppPitcher", "TEAM"), by.y = c("Name", "Team"))

##################################
### ADD IN OPPOSING TEAM STATS ###
##################################

  #Take top 8 ranked in K per 9 IP 
  Good.Pitchers <- Pitchers[Pitchers$K9.RNK < 9,]

  #Cut down by ERA
  Good.Pitchers <- Good.Pitchers[Good.Pitchers$ERA.RNK < 7,] 

### Add cut using opposing team runs or K% ###

### Add cut using fantasy points from previous starts

  #Calculate value
  Good.Pitchers$Value <- (Good.Pitchers$K9-(Good.Pitchers$WHIP+Good.Pitchers$ERA))/(Good.Pitchers$Salary/10000) 

### Make a final selection ###



#STEP THREE: Identify 3-5 worst pitchers

  #Take eight worst WHIP pitchers
  Bad.Pitchers <- Pitchers[Pitchers$WHIP.RNK > 22,]

  #Cut down by pRUNS
  Bad.Pitchers <- Bad.Pitchers[Bad.Pitchers$pRUNS.RNK > 20,] 

  #Cut down by ERA, if more than four pitchers remain
  if(nrow(Bad.Pitchers) < 4){
    Bad.Pitchers <- Bad.Pitchers[Bad.Pitchers$ERA.RNK > 22,] 
  }

Bats.Teams <- unlist(Bad.Pitchers$Opp)


#STEP FOUR: Pick best hitters from teams with best matchups (Bats.Teams)
  
  #Cut down hitters list to Bats.Teams
  Top.Bats <- D.tbl[D.tbl$Team %in% Bats.Teams, ]

  #Remove if batting slot is NA
  Top.Bats <- Top.Bats[!is.na(Top.Bats$Slot),]
  
  #Find the top 25% of Top.Bats in fdOBA
  Top.fdOBA <- subset(Top.Bats, fdOBA >= quantile(fdOBA, 0.75))

  #Get value based on fdOBA vs Salary
  Top.fdOBA$Value <- Top.fdOBA$fdOBA/(Top.fdOBA$Salary/10000)

  #Choose infielders with best value from Top.fdOBA

  IF <- Top.fdOBA[Top.fdOBA$POS != "OF",]
  Bat.Picks <- NULL
  Bat.Picks <- do.call("rbind", by(IF, IF$POS, function(x) x[which.max(x$Value), ]))


  #Choose outfielders with best value from Top.fdOBA
  OF <- Top.fdOBA[Top.fdOBA$POS == "OF",]
  if(nrow(OF) > 0){
  OF.1 <- do.call("rbind", by(OF, OF$POS, function(x) x[which.max(x$Value), ]))
  Bat.Picks <- rbind(Bat.Picks, OF.1)  
  }
  
  OF <- OF[OF$Name != OF.1[1,1],]
  if(nrow(OF) > 0){
  OF.2 <- do.call("rbind", by(OF, OF$POS, function(x) x[which.max(x$Value), ]))
  Bat.Picks <- rbind(Bat.Picks, OF.2)  
  }

  OF <- OF[OF$Name != OF.2[1,1],]
  if(nrow(OF) > 0){
    OF.3 <- do.call("rbind", by(OF, OF$POS, function(x) x[which.max(x$Value), ]))
    Bat.Picks <- rbind(Bat.Picks, OF.3)  
  }

