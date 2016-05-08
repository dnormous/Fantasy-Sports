library("XML")
library("stringr")
library("lpSolve")
library("ggplot2")


###############################################################################################

df <- NULL
tbl <- NULL

url = "http://www.rotowire.com/daily/nba/value-report.htm?site=DraftKings"  #<--Also have FanDuel, DraftDay, StarStreet 

html.page <- htmlParse(url)  
tableNodes <- getNodeSet(html.page, "//table")  
tbl = readHTMLTable(tableNodes[[1]],colClasses = c("character"),stringsAsFactors = FALSE) 

Encoding(tbl[,1]) <- iconv("UTF-8")
tbl[,1] <- str_replace_all(tbl[,1], "[^[:alnum:]]", ":")

tbl <- tbl[!grepl("::IR",tbl[,1]),]
tbl <- tbl[!grepl("::Out",tbl[,1]),]
tbl <- tbl[!grepl("::Inac",tbl[,1]),]

tbl[,1] <- gsub("::Prob","",tbl[,1])
tbl[,1] <- gsub("::Ques","",tbl[,1])

df <- str_split_fixed(tbl[,1], "::", 2)
dfx <- str_split_fixed(df[,2], ":", 2)
df[,2] <- dfx[,1]

for (i in 1:nrow(df)) {df[i,2] <- str_sub(df[i,2],-(nchar(df[i,2])/2),-1) }
tbl[,1] <- paste0(df[,2]," ",df[,1])
colnames(tbl)[1] <- "NAME"

tbl$Salary <- as.numeric(gsub("\\,","",(gsub("\\$","",tbl$Salary))))
tbl$FP <- as.numeric(tbl$FP)
colnames(tbl)[11] <- "FPperG"
tbl$FPperG <- as.numeric(tbl$FPperG)

data <- tbl[tbl$Pos == "PG",]
ggplot(data = data, aes(x=FPperG, y=Salary, colour=FPperG)) + geom_point() + geom_text(aes(label=NAME, angle=-45))

