library("stringr")
library("ggplot2")

#install.packages("devtools")
#library("devtools")
#install_github("ropensci/plotly")
library(plotly)
py <- plotly(username="dpierson", key="23e5qoe2rr")

  df <- read.csv(paste0("C:/Users/dpierson02/Google Drive/Derek/R/MLB 2015/Pick Sheets/Picksheet_",Sys.Date()-1,".csv"))
  df$SLOT <- suppressWarnings(as.numeric(gsub("th","",gsub("rd","",gsub("st","",gsub("nd","",df$SLOT))))))
  df <- df[df$POS != "P",]


library(scatterplot3d)
with(df, {
  scatterplot3d(SLOT, SALARY, pRUNS,        # x y and z axis
                color="blue", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                main="3-D Scatterplot Example 2",
                xlab="SLOT",
                ylab="SALARY",
                zlab="pRUNS",
                scale.y=4,
                box=F,
                angle=95)
})

ggplot(df, aes(pRUNS, SLOT, size=-SALARY, color=SALARY, label=NAME)) + 
  geom_point() + 
  scale_color_gradient(low="lightblue", high="red") +
  scale_y_continuous(breaks=1:9) +
  geom_text(aes(label=NAME),hjust=0, vjust=0, angle=45, size = 3)
  
