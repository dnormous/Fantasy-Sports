library("XML")
library("stringr")
library("httr")

# Grab FanDuel salaries
# Output data: FD 
# STATUS: Fully functional on August 12, 2014

  #Initialize variables 'df' and 'tbl'
  df <- NULL
  tbl <- NULL
  
  #Set url to pull DraftKings salary table from (Thank you rotowire!)
  url = "http://www.rotowire.com/daily/mlb/value-report.htm?site=Fanduel"  #<--Also have FanDuel, DraftDay, StarStreet 
  
  #Grab table from url
  #html.page <- htmlParse(url)  
  #tableNodes <- getNodeSet(html.page, "//table")  
  #tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
  
  #Using httr
  response<- GET(url)
  doc <- htmlParse(response, encoding = "UTF-8")
  tableNodes <- getNodeSet(doc, "//table")  
  tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 


  #Change table encoding and remove unknown/unwanted characters
  Encoding(tbl[,2]) <- iconv("UTF-8")
  tbl[,2] <- str_replace_all(tbl[,2], "[^[:alnum:]]", ":")
  tbl[,2] <- gsub("DTD","",tbl[,2])
  
  #Split tbl player names column by '::' and put pieces in df
  df <- str_split_fixed(tbl[,2], "::", 2)
  
  #Remove '::' from df last names and replace any ':' with '.'
  df[,2] <- gsub("::","",df[,2])
  df[,2] <- gsub(":",".",df[,2])
  #IMPORTANT: Players on the DL will have improper names
  #This is helpful as those players will not be included when merged 
  
  ### WATCH OUT FOR PLAYERS NAMED 'Jr.', might cause code problems ###
  
  #Create FD as join of columns from df and tbl
  FD <- NULL
  FD <- cbind(df,tbl)
  
  #Edit and capitalize FD column names 
  colnames(FD)[1:2] <- c("LASTNAME","FIRSTNAME")
  colnames(FD) <- toupper(colnames(FD))
  
  #FUTURE:  Could cut down FD table columns
  #FD <- FD[,c(1:3,5:11)]
  
  #Remove $ from FD salary column and format as numeric
  FD$SALARY <- gsub("\\$","",FD$SALARY)
  FD$SALARY <- as.numeric(gsub("\\,","",FD$SALARY))
  
  #Change column names 
  colnames(FD)[c(4,9,12:17)] <- c("NAME","VS","RWPRJ","RWVALUE","L5FPG","L5VALUE","SSNFPG","SSNVALUE")
  
  #Build full player names for FD$NAME column
  FD$NAME <- paste0(FD$FIRSTNAME," ",FD$LASTNAME)
  
  #Convert all anme columns to uppercase
  FD$NAME <- toupper(FD$NAME)
  FD$LASTNAME <- toupper(FD$LASTNAME)
  FD$FIRSTNAME <- toupper(FD$FIRSTNAME)
  
  #Add FIELD column to FD
  FD$FIELD <- ifelse(grepl("@",FD$OPP)==TRUE,"AWAY","HOME")
  
  #Remove '@' from FD$OPP column 
  FD$OPP <- gsub("\\@","",FD$OPP)
  
  #Change WAS to WSH in TEAM AND OP columns
  FD$TEAM <- ifelse(FD$TEAM == "WAS", "WSH", FD$TEAM)
  FD$OPP <- ifelse(FD$OPP == "WAS", "WSH", FD$OPP)
  
  #Fix LASTNAME column spacers showing up as ":"
  FD$LASTNAME <- gsub("\\:"," ",FD$LASTNAME)
  
  #Output FD as a .csv
  today <- Sys.Date()
  write.csv(file=paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/FD Salaries/FD_",today,".csv"), x=FD)
  
  #Remove data items that are no longer needed
#Remove data items that are no longer needed
nada <- ls()
nada <- nada[nada != "FD"] 
nada <- nada[nada != "STATS"] 
nada <- nada[nada != "P.STATS"] 
nada <- nada[nada != "ODDS"] 
nada <- nada[nada != "PS"] 
rm(list = (nada))
rm(nada)

