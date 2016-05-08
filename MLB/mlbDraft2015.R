#Load packages
library("ggplot2")
library("lattice")
library("gam")
library("ccaPP")
library("gridExtra")

#set working directory
setwd("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015")
getwd()

Hitters <- read.csv("mlb2015projs.csv")
Pitchers <- read.csv("mlb2015PitcherProjs.csv")

Catcher <- Hitters[grep("C", Hitters$Position), ]
Firstbase <- Hitters[grep("1B", Hitters$Position), ]
Secondbase <- Hitters[grep("2B", Hitters$Position), ]
Thirdbase <- Hitters[grep("3B", Hitters$Position), ]
Shortstop <- Hitters[grep("SS", Hitters$Position), ]
Outfield <- Hitters[grep("([A-Z]F)", Hitters$Position), ]

Starters <- Pitchers[grep("SP", Pitchers$Position), ]
Relief <- Pitchers[grep("RP", Pitchers$Position), ]


###     GRAPH SETTINGS      ###
###############################
pos <- Hitters
pos$var <- pos$hrs
pos <- pos[with(pos, order(-var)), ]
num <- 40

###############################

plotdata <- pos[1:num,]
plotdata$Player.Name <- as.character(plotdata$Player.Name)
breaks = seq(min(plotdata$var),max(plotdata$var), length.out = 6)
plotdata <- transform(plotdata, Player.Name=reorder(Player.Name, -var)) 
ggplot(plotdata, aes(x=Player.Name, y=var, fill=-var)) + geom_histogram(colour = "black", stat="identity") + coord_cartesian(ylim=c(breaks[1],breaks[6])) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + scale_y_continuous(breaks=breaks)  + scale_fill_gradientn(colours=rainbow(4)) 

