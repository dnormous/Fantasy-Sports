library("XML")
library("stringr")
library("httr")

# Grab hitter stats
# Output data: STATS (exports csv's for CY,LY,LW,vL,vR,HM,AW)  
# STATUS: Fully functional on August 12, 2014

#Build list of URLs to pull splits data from ESPN (Thank you ESPN!)
  URLs <- c("http://espn.go.com/mlb/stats/batting/_/count/",
            "http://espn.go.com/mlb/stats/batting/_/year/2014/count/",
            "http://espn.go.com/mlb/stats/batting/_/split/61/count/",
            "http://espn.go.com/mlb/stats/batting/_/year/2014/split/31/count/",
            "http://espn.go.com/mlb/stats/batting/_/year/2014/split/32/count/",
            "http://espn.go.com/mlb/stats/batting/_/year/2014/split/33/count/",
            "http://espn.go.com/mlb/stats/batting/_/year/2014/split/34/count/")
  
  #Set ending piece of html address to have table include all players
  URLsEND <- "/qualified/false" 
  
  #Build name list of splits to bring in
  Frames <- c("CY","LY","LW","vL","vR","HM","AW")  
  
  #Initialize variable 'STATS'
  STATS <- NULL

  #Use loop to grab all player data for each URL (split) across multiple table webpages
  for (j in 1:length(URLs)) 
  {
    #Initialize variables
    df <- NULL
    df.d <- NULL
    df.t <- NULL

    #Loop through "Battings Stats" table pages (up to 34 of them)
    for (i in 0:34)
    {
      #Set the start of each new table webpage at increments of 1,41,81,etc
      x <- (i*40)+1
      url = paste0(URLs[j],x,URLsEND)
      
      #Grab table data from set URL
      #html.page <- htmlParse(url)  
      #tableNodes <- getNodeSet(html.page, "//table")  
      #tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
      
        response<- GET(url)
        #if (response$status_code!=200){ # HTTP request failed!!
        #  # do some stuff...
        #  print(paste("Failure:",i,"Status:",response$status_code))
        #  next
        #}
        
      #Using httr
        doc <- htmlParse(response, encoding = "UTF-8")
        tableNodes <- getNodeSet(doc, "//table")  
        tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
      
            
      #Construct full table by appending tbl to df
      df <- rbind(df,tbl)
      
      #Sleep processing for 1/2 second in order to relieve spamming of ESPN
      Sys.sleep(0.5)
    }
    
    
    #Loop through "Expanded Batting Stats" table pages (up to 34 of them)
    for (i in 0:34)
    {
      #Set the start of each new table webpage at increments of 1,41,81,etc
      x <- (i*40)+1
      url = paste0(URLs[j],x,URLsEND,"/type/expanded")
      
      #Grab table data from set URL
      #html.page <- htmlParse(url)  
      #tableNodes <- getNodeSet(html.page, "//table")  
      #tbl.d = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
      
      #Using httr
        response<- GET(url)
        doc <- htmlParse(response, encoding = "UTF-8")
        tableNodes <- getNodeSet(doc, "//table")  
        tbl.d = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
      
      #Construct full table by appending tbl to df
      df.d <- rbind(df.d,tbl.d)
      
      #Sleep processing for 1/2 second in order to relieve spamming of ESPN
      Sys.sleep(0.5)
    }
    
    #Loop through "Sabermetric Batting Stats" table pages (up to 34 of them)
    for (i in 0:34)
    {
      #Set the start of each new table webpage at increments of 1,41,81,etc
      x <- (i*40)+1
      url = paste0(URLs[j],x,URLsEND,"/type/sabermetric")
      
      #Grab table data from set URL
      #html.page <- htmlParse(url)  
      #tableNodes <- getNodeSet(html.page, "//table")  
      #tbl.t = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
      
      #Using httr
        response<- GET(url)
        doc <- htmlParse(response, encoding = "UTF-8")
        tableNodes <- getNodeSet(doc, "//table")  
        tbl.t = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 
      
      #Construct full table by appending tbl to df
      df.t <- rbind(df.t,tbl.t)
      
      #Sleep processing for 1/2 second in order to relieve spamming of ESPN
      Sys.sleep(0.5)
    }
    
    #For df, df.d and df.t: Move first row of df to be column names and remove other title rows from df
    colnames(df) <- df[1,]
    df <- df[,-1]
    df <- df[which(df[,1] != "PLAYER"),]
    
    colnames(df.d) <- df.d[1,]
    df.d <- df.d[,-1]
    df.d <- df.d[which(df.d[,1] != "PLAYER"),]
    
    colnames(df.t) <- df.t[1,]
    df.t <- df.t[,-1]
    df.t <- df.t[which(df.t[,1] != "PLAYER"),]
    
    #Join columns of df, df.d and df.t into df
    df <- merge(df,df.d[,c(1,3,5:length(df.d))],by="PLAYER",all.x=TRUE,all.y=FALSE)
    df <- merge(df,df.t[,c(1,4:length(df.t))],by="PLAYER",all.x=TRUE,all.y=FALSE)
    
    #Format df columns 3 and up as numeric
    for (i in 3:ncol(df)){df[,i] <- as.numeric(df[,i])}
    
  #Fix TEAM for players who were traded
    #team.list <- suppressWarnings(as.data.frame(do.call(rbind, strsplit(df$TEAM, '/', perl=TRUE))))
    #for(i in 1:ncol(team.list)){team.list[,i] <- as.character(team.list[,i])}
    #team <- NULL
    #ifelse(Frames[j] != "CY",team.list[,3] <- team.list[,1],"OK")
    #for (i in 1:nrow(team.list)){team[i] <- ifelse(team.list[i,1] == team.list[i,2],ifelse(team.list[i,1] == team.list[i,3],team.list[i,1],team.list[i,3]),team.list[i,2])}
    #df$TEAM <- team
    
    #Rename columns in df; Colnames different for CY (contains WAR)
    ifelse(j == 1,colnames(df)[c(6,7,21,35:38)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")
    ifelse(j == 2,colnames(df)[c(6,7,22,36:39)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")
    ifelse(j == 3,colnames(df)[c(6,7,22,35:38)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")
    ifelse(j == 4,colnames(df)[c(6,7,22,35:38)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")
    ifelse(j == 5,colnames(df)[c(6,7,22,35:38)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")  
    ifelse(j == 6,colnames(df)[c(6,7,22,35:38)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")
    ifelse(j == 7,colnames(df)[c(6,7,22,35:38)] <- c("DBL","TPL","PPA","GF","ABHR","BBPA","BBK"), "ok")
  
    ### BUILD wOBA & dkOBA ###
    df$wOBA = round((0.691*df$BB + 0.723*df$HBP + 0.891*(df$H-df$XBH) + 1.279*df$DBL + 1.628*df$TPL + 2.121*df$HR) / (df$AB + df$BB - df$IBB + df$SF + df$HBP),3)
    df$fdOBA = round((1*df$BB + 1*df$HBP + 1*(df$H-df$XBH) + 2*df$DBL + 3*df$TPL + 4*df$HR + 1*df$R + 1*df$RBI + 2*df$SB) / (df$AB + df$BB - df$IBB + df$SF + df$HBP),3)
    
    #Set column names with split type identifier
    colnames(df) <- paste0(colnames(df),"_",Frames[j])
    colnames(df)[1] <- "NAME"
    
    #Remove TEAM column from all stat split tables except the first one
    ifelse(Frames[j] == "CY", colnames(df)[2] <- "TEAM", df<-df[,-2])      
    
    #Covert player names to uppercase
    df[,1] <- toupper(df[,1])
    
    #Add LASTNAME column to CY table only
    x <- do.call(rbind, strsplit(df[,1], ' (?=[^ ]+$)', perl=TRUE))
    df$LASTNAME <- x[,2]
    ifelse(Frames[j] == "CY", "OK", df<-df[,-(ncol(df))])
    
    #Remove duplicated NAMES <-- Could be a problem...
    df <- df[!duplicated(df$NAME), ]
    
    #Assign df as a STATS dataframe
    assign(paste0("STATS_",Frames[j]),df)
    
    #Export df as a .csv
    write.csv(df,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Stats/Split Stats/STATS_",Frames[j],"_",Sys.Date(),".csv"))   
  }
  

  #Create full datatable of all stats
  STATS <- STATS_CY[which(STATS_CY$AB_CY>0),]
  STATS <- merge(STATS,STATS_LY,by="NAME",all.x=TRUE,all.y=FALSE)
  STATS <- merge(STATS,STATS_LW,by="NAME",all.x=TRUE,all.y=FALSE)
  STATS <- merge(STATS,STATS_vL,by="NAME",all.x=TRUE,all.y=FALSE)
  STATS <- merge(STATS,STATS_vR,by="NAME",all.x=TRUE,all.y=FALSE)
  STATS <- merge(STATS,STATS_HM,by="NAME",all.x=TRUE,all.y=FALSE)
  STATS <- merge(STATS,STATS_AW,by="NAME",all.x=TRUE,all.y=FALSE)
  
  ################################ 
  #muWOBA = "Matchup wOBA"
  #muwOBA <- (0.55*Batter L/R wOBA) + (0.1* Hm/Aw wOBA) + (0.35* Pitcher wOBA allowed)
  ################################ 
  
  #Export combined stats table STATS as .csv 
  write.csv(STATS,paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Stats/STATS_",Sys.Date(),".csv"))
  
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
