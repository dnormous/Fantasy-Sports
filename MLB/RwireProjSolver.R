library("XML")
library("httr")
library("lpSolve")

url <- "http://www.rotowire.com/daily/mlb/optimizer.htm?site=Fanduel"
response<- GET(url)
doc <- htmlParse(response, encoding = "UTF-8")
results <- xpathSApply(doc, "//*/table[@id='PlayersTable']/tbody/tr/td", xmlValue)
tbl <- as.data.frame(matrix(results, ncol =9, byrow = TRUE),stringsAsFactors = F)
tbl <- tbl[,c(2,3,4,6,7,8)]
colnames(tbl) <- c("Name", "Team","POS","Salary","RWPRJ","Value") 
tbl$Salary <- gsub(",","", tbl$Salary)
tbl$Salary <- as.numeric(gsub("\\$","", tbl$Salary))
tbl$RWPRJ <- as.numeric(tbl$RWPRJ)
tbl$Value <- as.numeric(tbl$Value)

#Export tbl
write.csv(tbl,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Pick Sheets/Optimizer Table/Optimizer_",Sys.Date(),".csv"))


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
obj <- tbl$RWPRJ
con <- rbind(t(model.matrix(~ POS+0, tbl)), rep(1, rwtot),tbl$Salary,
             as.numeric(tbl$Name %in% Ex),
             as.numeric(tbl$Team %in% TMx),
             as.numeric(tbl$Name %in% Inc))

dir <- c("==","==","==","==","==","==","==","==","<","=","=","=")
rhs <- c(1, 1, 1, 1, 3, 1, 1, 9,
         35000,           #Salary limit
         0,               #NO CHANGE REQD - Excludes players listed in Ex 
         0,               #NO CHANGE REQD - Excludes players from teams listed in TMx 
         length(Inc))     #NO CHANGE REQD - Includes players listed in Inc 

result <- lp("max", obj, con, dir, rhs, binary.vec = 1:rwtot)
Lineup <- tbl[result$solution == 1, ]

Lineup
sum(Lineup$SALARY)
sum(Lineup$RWPRJ)

Rwire <- tbl
Rwire.Lineup <- Lineup

write.csv(Lineup,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Lineups/RWsolver/Lineup_",Sys.Date(),".csv"))

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


