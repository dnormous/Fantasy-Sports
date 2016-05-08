library("XML")
library("stringr")
library("lpSolve")
library("ggplot2")

###############################################################################################

pos.list <- c("qb","rb","wr","te")
pos.label <- c("QB","RB","WR","TE")
NFL.tbl <- NULL

for (i in 1:length(pos.list))
{
  url <- paste0("http://www.fantasypros.com/nfl/projections/",pos.list[i],".php?max-yes=true&min-yes=true")
  html.page <- htmlParse(url)  
  tableNodes <- getNodeSet(html.page, "//table")  
  tbl <- readHTMLTable(tableNodes[[2]],colClasses = c("character"),stringsAsFactors = FALSE) 
  
  df <- NULL
  df <- as.data.frame(toupper(str_split_fixed(tbl[,1], " \\(", 2)))[1]
  df[,2] <- pos.label[i]
  colnames(df) <- c("NAME","POS")
  
  x <- as.data.frame(str_split_fixed(tbl[,length(tbl)], "\\.",3))
  x[,1] <- as.character(x[,1])
  x[,2] <- as.character(x[,2])
  x[,3] <- as.character(x[,3])
  
  
  df$FPTS.A <- as.numeric(paste0(x[,1],".",substring(x[,2], 1, 1)))
  df$FPTS.H <- as.numeric(paste0(substring(x[,2], 2, nchar(x[,2])),".",substring(x[,3], 1, 1)))
  df$FPTS.L <- as.numeric(substring(x[,3], 2, nchar(x[,3])))

  df <- df[!grepl("HIGHLOW",df$NAME),]

  NFL.tbl <- rbind(NFL.tbl,df)
}

###############################################################################################

df <- NULL
tbl <- NULL

url = "http://www.rotowire.com/daily/nfl/value-report.htm?site=DraftKings"  #<--Also have FanDuel, DraftDay, StarStreet 

html.page <- htmlParse(url)  
tableNodes <- getNodeSet(html.page, "//table")  
tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 

Encoding(tbl[,2]) <- iconv("UTF-8")
tbl[,2] <- str_replace_all(tbl[,2], "[^[:alnum:]]", ":")

tbl <- tbl[!grepl("::IR",tbl[,2]),]
tbl <- tbl[!grepl("::Out",tbl[,2]),]
tbl <- tbl[!grepl("::Inac",tbl[,2]),]

tbl[,2] <- gsub("::Prob","",tbl[,2])
tbl[,2] <- gsub("::Ques","",tbl[,2])

df <- str_split_fixed(tbl[,2], "::", 2)
df[,2] <- gsub(":",".",df[,2])

tbl.dux <- as.data.frame(toupper(paste0(df[,2]," ",df[,1])))  

tbl <- cbind(tbl.dux,tbl[,c(1,3:6)])
colnames(tbl) <- c("NAME","POS","TEAM","OPP","FIELD","SALARY")
tbl$SALARY <- as.numeric(gsub("\\$","",(gsub("\\,","",tbl$SALARY))))

tbl$NAME <- as.character(tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "ROBERT GRIFFIN","ROBERT GRIFFIN III",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "T.Y. HILTON","TY HILTON",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "LE.VEON BELL","LE'VEON BELL",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "MAURICE JONES:DREW","MAURICE JONES-DREW",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "EJ MANUEL","E.J. MANUEL",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "KA.DEEM CAREY","KA'DEEM CAREY",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "AUSTIN SEFERIAN:JENKINS","AUSTIN SEFERIAN-JENKINS",tbl$NAME)
tbl$NAME <- ifelse(tbl$NAME == "CHRIS IVORY","CHRISTOPHER IVORY",tbl$NAME)

NFL <- merge(NFL.tbl,tbl[,c(1,3:6)], by=c("NAME"), all.x=FALSE, all.y=FALSE)
#df <- df[which(df$FPTS.A > 2),]

#################################################################################

#Load DEF projections table from CBSSports.com
DEFurl <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/DST/9/avg/standard"
html.page <- htmlParse(DEFurl)
tableNodes <- getNodeSet(html.page, "//table")
tbl = readHTMLTable(tableNodes[[7]], header = c("Player", "Int", "DFR",  "FF", "SACK", "DTD", "STY",   "PA",  "TYdA", "FPTS"),
                    colClasses = c("character"),stringsAsFactors = FALSE)
#Remove the top two rows
tbl <- tbl[-1,]    
tbl <- tbl[-1,]   

df <- as.data.frame(toupper(str_split_fixed(tbl[,1], "\\,", 2)))[1]
df[,2] <- "DEF"
df[,3] <- tbl[,10]
colnames(df) <- c("NAME","POS","FPTS")
DEF <- df

##################################################################################

#Set player to exclude by pasting names in Ex list
Ex <- c("")

#Set teams to avoid by pasting tea abbreviation in TMx list
TMx <- c("NO","CAR","BRANDON MARSHALL","")

#Set players to include by pasting names in Inc list 
Inc <- NULL #c("")

#Solver setup and parameters  
DEF.sal <- 3200
rwtot <- nrow(NFL)    
obj <- NFL$FPTS.A
con <- rbind(t(model.matrix(~ POS + 0, NFL)), rep(1, rwtot), NFL$SALARY, 
             as.numeric(NFL$NAME %in% Ex),
             as.numeric(NFL$TEAM %in% TMx),
             as.numeric(NFL$NAME %in% Inc))

dir <- c("==", ">=", ">=", ">=", "==", "<","=","=","=")
rhs <- c(1, 2, 1, 3, 8, (50000-DEF.sal),
         0,               #NO CHANGE REQD - Excludes players listed in Ex 
         0,               #NO CHANGE REQD - Excludes players from teams listed in TMx 
         length(Inc))     #NO CHANGE REQD - Includes players listed in Inc 

result <- lp("max", obj, con, dir, rhs, binary.vec = 1:rwtot)
Lineup <- NFL[result$solution == 1, ]

Lineup
sum(Lineup$FPTS.A)
sum(Lineup$SALARY)


#Solve for 2nd-best lineup

con2 <- rbind(con, result$solution)
dir2 <- c(dir, "<=")
rhs2 <- c(rhs, 2)
Lineup2 <- lp("max", obj, con2, dir2, rhs2, all.bin = TRUE)

Lineup2 <- NFL[Lineup2$solution == 1, ]

Lineup2
sum(Lineup2$FPTS.A)
sum(Lineup2$SALARY)
