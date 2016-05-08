install.packages("devtools")
library("devtools")

devtools::install_github("plotly/R-api")

library(plotly)
p <- plotly(username="dpierson", key="23e5qoe2rr")


############################################################################################

p$plotly(c(0,1,2,3,4),c(0,3,2,4,5))

############################################################################################

x <- c(seq(0,0,length=1000),seq(1,1,length=1000),seq(2,2,length=1000))
y <- c(rlnorm(1000,0,1),rlnorm(1000,0,2),rlnorm(1000,0,3))
s <- list(
  type = 'box',
  jitter = 0.5
)
layout <- list(
  title = 'Fun with the Lognormal distribution',
  yaxis = list(
    type = 'log'
  )
)

response <- p$plotly(x,y, kwargs = list(layout = layout, style=s))

browseURL(response$url)

############################################################################################
data=MLBhitters[MLBhitters$POS == "2B",] 

x <- data$NAME
y <- data$HR
s <- list(
  type = 'box',
  jitter = 0.5
)
layout <- list(
  title = 'Season HR Totals for active Second Basemen',
  yaxis = list(
    type = ''
  )
)

TwoB_HR_Box <- p$plotly(x,y, kwargs = list(layout = layout, style=s))

browseURL(TwoB_HR_Box$url)

#############################################################################################




