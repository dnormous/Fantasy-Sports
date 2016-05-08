setwd("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/")
#setwd("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/")


library("lpSolve")

#FD <- read.csv(paste0("FD Salaries/FD_", Sys.Date(),".csv"))
FDpitchers <- read.csv(paste0("Pick Sheets/Fpros/FprosPitchers_", Sys.Date(),".csv"))
FDhitters <- read.csv(paste0("Pick Sheets/Fpros/FprosHitters_", Sys.Date(),".csv"))
STATS <- read.csv(paste0("Stats/STATS_", Sys.Date(),".csv"))
ODDS <- read.csv(paste0("Odds/ODDS_", Sys.Date(),".csv"))

FDpitchers <- FDpitchers[,-1]
FDhitters <- FDhitters[,-1]
STATS <- STATS[,-1]
ODDS <- ODDS[,-1]

#Start PS with useful columns from DK
PS <- FDhitters[,c(1:8,12,13,15,16)]

#Add projected runs to PS
PS <- merge(PS,ODDS[,c(1,4,6)],by.x="Team",by.y="TEAM")

#SET VERSUS COLUMN TO LASTNAME ONLY
PS$OppPitcher <- toupper(as.character(t(as.data.frame(strsplit(as.character(PS$OppPitcher)," ")))[,2]))


#Build PS based on splits
colnames(PS) <- toupper(colnames(PS))
PS$NAME <- toupper(PS$NAME)
sl <- merge(PS,STATS[,c(1,3:length(STATS))],by="NAME")
PS <- sl[,1:15]
PS$AB <- ifelse(PS$ARM == "R",sl$AB_vR,sl$AB_vL)
PS$AVG <- ifelse(PS$ARM == "R",sl$AVG_vR,sl$AVG_vL)
PS$HRAB <- round(ifelse(PS$ARM == "R",sl$HR_vR/PS$AB,sl$HR_vL/PS$AB),3)
PS$wOBA <- round(ifelse(PS$ARM == "R",sl$wOBA_vR,sl$wOBA_vL),3)
PS$fdOBA <- round(ifelse(PS$ARM == "R",sl$fdOBA_vR,sl$fdOBA_vL),3)

#Remove rows with NA as a safety step
#PS <- PS[complete.cases(PS),]

#Export ODDS table
write.csv(PS,paste0("Pick Sheets/PickSheet_",Sys.Date(),".csv"))

#Remove data items that are no longer needed
nada <- ls()
nada <- nada[nada != "FD"] 
nada <- nada[nada != "STATS"] 
nada <- nada[nada != "P.STATS"] 
nada <- nada[nada != "ODDS"] 
nada <- nada[nada != "PS"] 
rm(list = (nada))
rm(nada)



# Solver for top daily lineups
# Output data: NA
# STATUS: FUNCTIONAL as of August 14, 2014. Quick solver in place, NO PITCHER SELECTION SET UP 

#Set player to exclude by pasting names in Ex list
#Ex <- c("BILLY HAMILTON")

#Set teams to avoid by pasting tea abbreviation in TMx list
#TMx <- c("")

#Set players to include by pasting names in Inc list 
#Inc <- NULL #c("")

#Solver setup and parameters  
#rwtot <- nrow(PS)    
#obj <- PS$fdOBA
#con <- rbind(t(model.matrix(~ POS+0, PS)), rep(1, rwtot),PS$SALARY,
#             PS$HRAB<0.00, 
#             PS$pRUNS<3.50,
#             PS$AB<90,
#             PS$AVG<0.25,
#             PS$RWPRJ<2.0,
#             as.numeric(PS$NAME %in% Ex),
#             as.numeric(PS$TEAM %in% TMx),
#             as.numeric(PS$NAME %in% Inc))
#
#dir <- c("==", "==", "==", "==", "==", "==", "==","<","<=","<=","<=","<","<=","=","=","=")
#rhs <- c(1, 1, 1, 1, 3, 1, 8,
#         (35000-8800),           #Salary limit
#         0,               #Players that can be below HRAB threshold
#         0,               #Players allowed from teams below pRUNS threshold
#         0,               #Players allowed that have less than the AB threshold
#         0,               #Players allowed with AVG below threshold
#         0,               #Players allowed with a RotoWire projection below threshold 
#         0,               #NO CHANGE REQD - Excludes players listed in Ex 
#         0,               #NO CHANGE REQD - Excludes players from teams listed in TMx 
#         length(Inc))     #NO CHANGE REQD - Includes players listed in Inc 
#
#result <- lp("max", obj, con, dir, rhs, binary.vec = 1:rwtot)
#Lineup <- PS[result$solution == 1, ]
#
#Lineup
#sum(Lineup$SALARY)
#sum(Lineup$fdOBA)
#sum(Lineup$RWPRJ)
#
#Solve for 2nd-best lineup
#
#con2 <- rbind(con, result$solution)
#dir2 <- c(dir, "<=")
#rhs2 <- c(rhs, 4)
#Lineup2 <- lp("max", obj, con2, dir2, rhs2, all.bin = TRUE)
#
#Lineup2 <- PS[Lineup2$solution == 1, ]
#
#Lineup2
#
#sum(Lineup2$SALARY)
#sum(Lineup2$fdOBA)


#Solve for lineup by Rotowire projection

#obj2 <- PS$RWPRJ
#RWLineup <- lp("max", obj2, con, dir, rhs, all.bin = TRUE)
#RWLineup <- PS[RWLineup$solution == 1, ]
#RWLineup

#sum(RWLineup$SALARY)
#sum(RWLineup$fdOBA)
#sum(RWLineup$RWPRJ)


#Export ODDS table
#write.csv(Lineup[,c(1:8,13:21)],paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Lineups/DKsolver/Lineup_",Sys.Date(),".csv"))
#write.csv(RWLineup[,c(1:8,13:21)],paste0("C:/Users/Derek/Google Drive/Derek/R/MLB 2015/Lineups/DKsolver/RWLineup_",Sys.Date(),".csv"))
