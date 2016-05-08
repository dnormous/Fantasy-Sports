library("XML")
library("stringr")
library("lpSolve")
library("ggplot2")

setwd("C:/Users/Derek/Google Drive/Derek/R/NFL/NFL 2015")

###############################################################################################################
#QB projections from FantasyPros

url = "http://www.fantasypros.com/nfl/projections/qb.php"  
html.page <- htmlParse(url)  
tableNodes <- getNodeSet(html.page, "//table")  
tbl = readHTMLTable(tableNodes[[2]],colClasses = c("character"),stringsAsFactors = FALSE) 

QB <- NULL

QB <- as.data.frame(str_split_fixed(tbl[,1], " ", 4))
QB[,1] <- toupper(paste0(QB[,1]," ", QB[,2]))
tbl$Player <- QB[,1]
tbl$Team <- as.character(QB[,3]) 

  tbl$Team <- as.character(ifelse(tbl$Team == "III", "WAS", tbl$Team))

tbl$YDS <- gsub(",","",tbl$YDS)

QB <- tbl[,c(1,12,2:11)]
colnames(QB) <- c("PLAYER","TEAM","PATT","CMP","PYDS","PTDS","INTS","RATT","RYDS","RTDS","FL","FPTS") 
for (i in 3:12) {QB[,i] <- as.numeric(QB[,i])}

QBfpt <- QB[,c(1,2,12)]
QBfpt$POS <- "QB"


###############################################################################################################
#Aucion Values from FantasyPros --Used in 2015?

#From csv
AucVal <- read.csv("AuctionValues.csv")
AucVal$PLAYER <- toupper(AucVal$PLAYER)

#url = "http://www.fantasypros.com/nfl/auction-values/overall.php?teams=10"  
#html.page <- htmlParse(url)  
#tableNodes <- getNodeSet(html.page, "//table")  
#tbl = readHTMLTable(tableNodes[[2]],colClasses = c("character"),stringsAsFactors = FALSE) 

#tbl[,1] <- gsub("Jr. ","",tbl[,1])
#tbl[,1] <- gsub(" III","",tbl[,1])
#tbl[,1] <- gsub("St. Louis","STL",tbl[,1])
#tbl[,1] <- gsub("New York","NY",tbl[,1])
#tbl[,1] <- gsub("New England","NE",tbl[,1])
#tbl[,1] <- gsub("Kansas City","KC",tbl[,1])
#tbl[,1] <- gsub("San Francisco","SF",tbl[,1])
#tbl[,1] <- gsub("Green Bay","GB",tbl[,1])

#Auc <- as.data.frame(str_split_fixed(tbl[,1], " ",5))
#df <- as.data.frame(str_split_fixed(Auc[,5], "\\,",2))
#Auc$TEAM <- df[,1]
#Auc$BYE <- as.numeric(gsub(" ","",df[,2]))
#Auc$NAME <- toupper(paste0(Auc[,1]," ",Auc[,2]))
#Auc$POS <- Auc[,3]
#Auc <- Auc[,c(8,6,9,7)]

#Auc$AVG <- (as.numeric(gsub("\\$","",tbl$Ave)))*2.5
#Auc$MIN <- (as.numeric(gsub("\\$","",tbl$Min)))*2.5
#Auc$MAX <- (as.numeric(gsub("\\$","",tbl$Max)))*2.5


###############################################################################################################
#QB Plotter by FPTS, AUCTION$, VALUE

QBplot <- merge(QB,AucVal, by="PLAYER", all.x=F, all.y=F)
QBplot <- QBplot[with(QBplot, order(-FPTS)), ]
QBplot$PLAYER <- factor(QBplot$PLAYER, levels = QBplot$PLAYER, ordered = TRUE)

QB_ranks12 <- ggplot(QBplot[1:25,], aes(x=PLAYER, y=FPTS)) + geom_bar(aes(fill=FPTS),stat = "identity") + geom_point(aes(x=PLAYER, y=AUC12, size=2)) + geom_point(aes(x=PLAYER, y=FPTS/AUC12, size=2, color="WHITE")) + theme(axis.text.x = element_text(angle = -45, hjust = 0)) + scale_fill_gradientn(colours = rainbow(4))
QB_ranks12

QB_ranks8 <- ggplot(QBplot[1:25,], aes(x=PLAYER, y=FPTS)) + geom_bar(aes(fill=FPTS),stat = "identity") + geom_point(aes(x=PLAYER, y=AUC8, size=2)) + geom_point(aes(x=PLAYER, y=FPTS/AUC8, size=2, color="WHITE")) + theme(axis.text.x = element_text(angle = -45, hjust = 0)) + scale_fill_gradientn(colours = rainbow(4))
QB_ranks8


###############################################################################################################
#RB projections from FantasyPros

url = "http://www.fantasypros.com/nfl/projections/rb.php"  
html.page <- htmlParse(url)  
tableNodes <- getNodeSet(html.page, "//table")  
tbl = readHTMLTable(tableNodes[[2]],colClasses = c("character"),stringsAsFactors = FALSE) 

RB <- NULL

RB <- as.data.frame(str_split_fixed(tbl[,1], " ", 4))
RB[,1] <- toupper(paste0(RB[,1]," ", RB[,2]))
tbl$Player <- RB[,1]
tbl$Team <- as.character(RB[,3]) 

tbl$YDS <- gsub(",","",tbl$YDS)

RB <- tbl[,c(1,10,2:9)]
colnames(RB) <- c("PLAYER","TEAM","RATT","RYDS","RTDS","REC","RCYDS","RCTDS","FL","FPTS") 
for (i in 3:10) {RB[,i] <- as.numeric(RB[,i])}

RBfpt <- RB[,c(1,2,10)]
RBfpt$POS <- "RB"


###############################################################################################################
#WR projections from FantasyPros

url = "http://www.fantasypros.com/nfl/projections/wr.php"  
html.page <- htmlParse(url)  
tableNodes <- getNodeSet(html.page, "//table")  
tbl = readHTMLTable(tableNodes[[2]],colClasses = c("character"),stringsAsFactors = FALSE) 

WR <- NULL

WR <- as.data.frame(str_split_fixed(tbl[,1], " ", 4))
WR[,1] <- toupper(paste0(WR[,1]," ", WR[,2]))
tbl$Player <- WR[,1]
tbl$Team <- as.character(WR[,3])

  tbl$Team <- as.character(ifelse(tbl$Team == "Jr.", "NYG", tbl$Team))

tbl[,6] <- gsub(",","",tbl[,6])

WR <- tbl[,c(1,10,2:9)]
colnames(WR) <- c("PLAYER","TEAM","RATT","RYDS","RTDS","REC","RCYDS","RCTDS","FL","FPTS") 
for (i in 3:10) {WR[,i] <- as.numeric(WR[,i])}

WRfpt <- WR[,c(1,2,10)]
WRfpt$POS <- "WR"

###############################################################################################################
#TE projections from FantasyPros

url = "http://www.fantasypros.com/nfl/projections/te.php"  
html.page <- htmlParse(url)  
tableNodes <- getNodeSet(html.page, "//table")  
tbl = readHTMLTable(tableNodes[[2]],colClasses = c("character"),stringsAsFactors = FALSE) 

TE <- NULL

TE <- as.data.frame(str_split_fixed(tbl[,1], " ", 4))
TE[,1] <- toupper(paste0(TE[,1]," ", TE[,2]))
tbl$Player <- TE[,1]
tbl$Team <- as.character(TE[,3])

tbl$YDS <- gsub(",","",tbl$YDS)

TE <- tbl[,c(1,7,2:6)]
colnames(TE) <- c("PLAYER","TEAM","REC","RCYDS","RCTDS","FL","FPTS") 
for (i in 3:7) {TE[,i] <- as.numeric(TE[,i])}

TEfpt <- TE[,c(1,2,7)]
TEfpt$POS <- "TE"

#############################################################################################################
#Combine, etc

ALLfpt <- rbind(QBfpt,RBfpt,WRfpt,TEfpt)
ALLfpt <- ALLfpt[with(ALLfpt, order(PLAYER)), ]

ALL <- merge(ALLfpt,AucVal[,c(1,4,5)], by="PLAYER", all.x=F, all.y=F)

#############################################################################################################
#FF League keeper list compile

FF <- read.csv("FFRosterR.csv") 
FF$PLAYER <- toupper(FF$PLAYER)
FF <- FF[with(FF, order(PLAYER)), ]

FFp <- merge(FF, ALLfpt, by ="PLAYER", all.x=TRUE)

write.csv(FFp,"FF_projections.csv")
write.csv(ALLauc,"FFsolver_data.csv")
getwd()





