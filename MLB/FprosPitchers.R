library("XML")
library("httr")
library("lpSolve")

url <- "http://www.fantasypros.com/mlb/fanduel-cheatsheet.php?position=P"
response<- GET(url)
doc <- htmlParse(response, encoding = "UTF-8")
tableNodes <- getNodeSet(doc, "//table")  
tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 

tbl$Team <- sapply(strsplit(sapply(strsplit(as.character(tbl[,1]), " \\("), "[[", 2), " \\- "), "[[", 1)
tbl$POS <- gsub("\\)","",sapply(strsplit(sapply(strsplit(as.character(tbl[,1]), " \\("), "[[", 2), " \\- "), "[[", 2))
tbl[,1] <- sapply(strsplit(as.character(tbl[,1]), " \\("), "[[", 1)

colnames(tbl) <- c("Name","Wrong","Time","Opp","OppPitcher","Wrong2","MaxRain","ProjRank","SalaryRank",     
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
for (i in 7:13){tbl[,i] <- as.numeric(tbl[,i])}

#Export tbl
write.csv(tbl,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Pick Sheets/Fpros/FprosPitchers_",Sys.Date(),".csv"))


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


