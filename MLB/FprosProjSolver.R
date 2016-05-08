library("XML")
library("httr")
library("lpSolve")

url <- "http://www.fantasypros.com/mlb/fanduel-cheatsheet.php?position=H"
response<- GET(url)
doc <- htmlParse(response, encoding = "UTF-8")
tableNodes <- getNodeSet(doc, "//table")  
tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 

tbl$Team <- sapply(strsplit(sapply(strsplit(as.character(tbl[,1]), " \\("), "[[", 2), " \\- "), "[[", 1)
tbl$POS <- gsub("\\)","",sapply(strsplit(sapply(strsplit(as.character(tbl[,1]), " \\("), "[[", 2), " \\- "), "[[", 2))
tbl[,1] <- sapply(strsplit(as.character(tbl[,1]), " \\("), "[[", 1)

colnames(tbl) <- c("Name","Bats","Slot","Time","Opp","OppPitcher","Arm","MaxRain","ProjRank","SalaryRank",     
                  "Diff","ProjPts","Salary","CostPt","Team","POS")  

tbl$Opp <- gsub("\\@","",tbl$Opp)
tbl$Salary <- gsub(",","", tbl$Salary)
tbl$Salary <- as.numeric(gsub("\\$","", tbl$Salary))
tbl$CostPt <- gsub(",","", tbl$CostPt)
tbl$CostPt <- gsub("\\-","", tbl$CostPt)
tbl$CostPt <- as.numeric(gsub("\\$","", tbl$CostPt))
tbl$ProjPts <- gsub(" pts","", tbl$ProjPts)
tbl$MaxRain <- gsub("\\%","", tbl$MaxRain)
tbl$MaxRain <- gsub("Roof","0", tbl$MaxRain)
tbl$MaxRain <- gsub("Dome","0", tbl$MaxRain)
tbl$MaxRain <- gsub("\\-","-100", tbl$MaxRain)
tbl$Slot <- gsub("X","0", tbl$Slot)
tbl$Slot <- as.numeric(gsub("\\-","0", tbl$Slot))
for (i in 8:14){tbl[,i] <- as.numeric(tbl[,i])}

#Export tbl
write.csv(tbl,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Pick Sheets/Fpros/FprosHitters_",Sys.Date(),".csv"))


# Solver for top daily lineups
# STATUS: FUNCTIONAL as of April 8, 2014.

#Set player to exclude by pasting names in Ex list
Ex <- c("")

#Set teams to avoid by pasting tea abbreviation in TMx list
TMx <- c("")

#Set players to include by pasting names in Inc list 
Inc <- NULL #c("")

#Solver setup and parameters  
rwtot <- nrow(tbl)    
obj <- tbl$ProjPts
con <- rbind(t(model.matrix(~ POS+0, tbl)), rep(1, rwtot),tbl$Salary,
             tbl$MaxRain>50,
             as.numeric(tbl$Name %in% Ex),
             as.numeric(tbl$Team %in% TMx),
             as.numeric(tbl$Name %in% Inc))

dir <- c("==","==","==","==","==","==","==","<","=","=","=","=")
rhs <- c(1, 1, 1, 1, 3, 1, 8,
         (35000-10000),           #Salary limit
         0,               #No players in games over Max Rain % limit
         0,               #NO CHANGE REQD - Excludes players listed in Ex 
         0,               #NO CHANGE REQD - Excludes players from teams listed in TMx 
         length(Inc))     #NO CHANGE REQD - Includes players listed in Inc 

result <- lp("max", obj, con, dir, rhs, binary.vec = 1:rwtot)
Lineup <- tbl[result$solution == 1, ]

Lineup
sum(Lineup$Salary)
sum(Lineup$ProjPts)

Fpros <- tbl
Fpros.Lineup <- Lineup
write.csv(Lineup,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Lineups/FprosSolver/Lineup_",Sys.Date(),".csv"))

#Remove data items that are no longer needed
nada <- ls()
nada <- nada[nada != "FD"] 
nada <- nada[nada != "STATS"]  
nada <- nada[nada != "ODDS"] 
nada <- nada[nada != "PS"]
nada <- nada[nada != "Fpros"] 
nada <- nada[nada != "Fpros.Lineup"] 
nada <- nada[nada != "Rwire"] 
nada <- nada[nada != "Rwire.Lineup"] 
nada <- nada[nada != "P.STATS"] 
rm(list = (nada))
rm(nada)


