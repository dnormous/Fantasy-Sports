library("XML")
library("stringr")
library("httr")

# STEP 3: Grab daily odds for projecting RUNS & favorites
# Output data: ODDS  
# STATUS: Functional, but pRUNS is slightly inaccurate as of August 14, 2014

  #Set URL for scraping mlb odds
  url = "http://www.donbest.com/mlb/odds/money-lines/" 
  
  #Pull in odds tble from URL
  html.page <- htmlParse(url)  
  tableNodes <- getNodeSet(html.page, "//table")  
  tbl = readHTMLTable(tableNodes[[1]],                                  
                      colClasses = c("character"),                         
                      stringsAsFactors = FALSE) 
  
  #Cut down and format odds tbl  
  tbl <- tbl[,c(3,4,8)]   #<-- Money lines available from other sources in different columns
  colnames(tbl) <- toupper(tbl[1,])
  tbl <- tbl[-1,]
  tbl <- tbl[which(tbl[,1] != "NA"),]
  tbl <- tbl[which(tbl[,1] != "Team"),]
  
  #Split the teams string into useable pieces
  tms <- strsplit(as.character(tbl$TEAM),"-")
  tms <- t(as.data.frame(tms))
  tms <- strsplit(substring(tms[,3], 2),"(?<=[a-z]{2})(?=[A-Z])", perl = TRUE)
  
  #Rebuild odds table
  tbl <- cbind(t(as.data.frame(tms)),tbl[,2:3])
  row.names(tbl) <- NULL
  tbl[,5] <- substring(tbl[,4], 5)
  tbl[,4] <- substring(tbl[,4], 1,4)
  
  #Format column values
  tbl[,1] <- as.character(tbl[,1])    
  tbl[,2] <- as.character(tbl[,2]) 
  tbl[,3] <- as.POSIXct(tbl[,3], format="%I:%M %p", tz="EST")
  tbl[,4] <- gsub("--","0",tbl[,4])
  tbl[,4] <- as.numeric(tbl[,4])
  tbl[,5] <- as.numeric(tbl[,5])
  tbl[,5][is.na(tbl[,5])] <- 0 

  #Build ODDS from tbl
  ODDS <- NULL
  ODDS <- as.data.frame(t(as.data.frame(rbind(c(tbl[,1],tbl[,2])))))
  colnames(ODDS) <- "TEAM"
  ODDS$TEAM <- as.character(ODDS$TEAM)
  ODDS$MLINE <- c(tbl[,4],tbl[,5])
  ODDS$DATE <- c(tbl[,3],tbl[,3])
  row.names(ODDS) <- NULL 
  
  ###Add O/U from another webpage
  
  #Set URL for scraping mlb odds
  url = "http://www.donbest.com/mlb/odds/" 
  
  #Pull in odds tble from URL
  html.page <- htmlParse(url)  
  tableNodes <- getNodeSet(html.page, "//table")  
  tbl = readHTMLTable(tableNodes[[1]],                                  
                      colClasses = c("character"),                         
                      stringsAsFactors = FALSE) 
  
  #Isolate column of interest
  tbl <- as.data.frame(tbl[,8])
  tbl <- as.data.frame(tbl[which(tbl[,1] != "Mirage"),])              
  tbl <- as.data.frame(tbl[which(tbl[,1] != "NA"),]) 
  
  #Format string to contain O/U            
  OU <- ifelse(grepl("\\.",substring(tbl[,1],1,3))==TRUE,substring(tbl[,1],1,3),str_sub(tbl[,1],-3,-1))  
  
  #Add OU to ODDS
  ODDS$OU <- as.numeric(c(OU,OU))
  
  ###NEEDS ADJUSTMENT###
  #Sum of pRUNs total for opposing teams should equal OU
  #Add pRUNS from RG formula
  ODDS$CVRp <- round(ifelse(ODDS$MLINE < 0, abs(ODDS$MLINE)/(abs(ODDS$MLINE)+100), 1-(ODDS$MLINE/(ODDS$MLINE+100))),3)
  ODDS$pRUNS <- round(ifelse(ODDS$MLINE < 0, (ODDS$OU/2)+(1.5/2)+((ODDS$CVRp*-1.5)/2), (ODDS$OU/2)-(1.5/2)+((ODDS$CVRp*1.5)/2)),2)
  #######
  
  ###Replace full text team names with abbreviations
  ODDS[,1] <- ifelse(grepl("Braves",ODDS[,1]) == TRUE, "ATL",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Nationals",ODDS[,1]) == TRUE, "WSH",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Orioles",ODDS[,1]) == TRUE, "BAL",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Tigers",ODDS[,1]) == TRUE, "DET",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Brewers",ODDS[,1]) == TRUE, "MIL",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Boston",ODDS[,1]) == TRUE, "BOS",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Phillies",ODDS[,1]) == TRUE, "PHI",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Cubs",ODDS[,1]) == TRUE, "CHC",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Twins",ODDS[,1]) == TRUE, "MIN",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Indians",ODDS[,1]) == TRUE, "CLE",ODDS[,1])
  
  ODDS[,1] <- ifelse(grepl("White",ODDS[,1]) == TRUE, "CHW",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Royals",ODDS[,1]) == TRUE, "KC",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Giants",ODDS[,1]) == TRUE, "SF",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Dodgers",ODDS[,1]) == TRUE, "LAD",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Diamondbacks",ODDS[,1]) == TRUE, "ARI",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Rockies",ODDS[,1]) == TRUE, "COL",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Cardinals",ODDS[,1]) == TRUE, "STL",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Pirates",ODDS[,1]) == TRUE, "PIT",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Yankees",ODDS[,1]) == TRUE, "NYY",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Blue",ODDS[,1]) == TRUE, "TOR",ODDS[,1])
  
  ODDS[,1] <- ifelse(grepl("Reds",ODDS[,1]) == TRUE, "CIN",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Mets",ODDS[,1]) == TRUE, "NYM",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Padres",ODDS[,1]) == TRUE, "SD",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Marlins",ODDS[,1]) == TRUE, "MIA",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Rangers",ODDS[,1]) == TRUE, "TEX",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Rays",ODDS[,1]) == TRUE, "TB",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Angels",ODDS[,1]) == TRUE, "LAA",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Astros",ODDS[,1]) == TRUE, "HOU",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Mariners",ODDS[,1]) == TRUE, "SEA",ODDS[,1])
  ODDS[,1] <- ifelse(grepl("Athletics",ODDS[,1]) == TRUE, "OAK",ODDS[,1])
  
  #Export ODDS table
  write.csv(ODDS,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Odds/ODDS_",Sys.Date(),".csv"))
  
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
  

