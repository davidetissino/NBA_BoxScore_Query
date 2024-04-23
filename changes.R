library(dplyr)
library(tidyr)
library(lubridate)

## Regular Season
# regular season logs
rs_logs <- read.csv('./datasets/RS Logs pre.csv')

# keep only relevant columns
rs_logs <- rs_logs[,-c(2,4,6,7,31,32)]

# rearrange in proper order 
rs_logs <- rs_logs[,c(1,2,3,5,4,6,25,7:24,26)]

# change column names 
colnames(rs_logs)[1] <- 'SEASON'
colnames(rs_logs)[2] <- 'PLAYER'
colnames(rs_logs)[3] <- 'TEAM'
colnames(rs_logs)[5] <- 'DATE'
colnames(rs_logs)[6] <- 'W/L'
colnames(rs_logs)[11] <- 'FG%'
colnames(rs_logs)[12] <- '3PA'
colnames(rs_logs)[13] <- '3PM'
colnames(rs_logs)[14] <- '3P%'
colnames(rs_logs)[17] <- 'FT%'
colnames(rs_logs)[26] <- '+/-'

# % columns with two decimals
rs_logs$`FG%` <- rs_logs$`FG%` * 100
rs_logs$`3P%` <- rs_logs$`3P%` * 100
rs_logs$`FT%` <- rs_logs$`FT%` * 100

# from recent to last 
rs_logs <- rs_logs[order(rs_logs$DATE, decreasing = T), ]

# create csv file
write.csv(rs_logs, './datasets/RS Logs.csv', row.names = F)


## Playoffs
# playoffs box scores
p_offs <- read.csv('./datasets/PO Logs pre.csv')

# only rel columns 
p_offs <- p_offs[,-c(2,4,6,7,31,32)]

# order
p_offs <- p_offs[,c(1:3,5,4,6,25,7:24,26)]

# column names
colnames(p_offs)[1] <- 'SEASON'
colnames(p_offs)[2] <- 'PLAYER'
colnames(p_offs)[3] <- 'TEAM'
colnames(p_offs)[5] <- 'DATE'
colnames(p_offs)[6] <- 'W/L'
colnames(p_offs)[11] <- 'FG%'
colnames(p_offs)[12] <- '3PA'
colnames(p_offs)[13] <- '3PM'
colnames(p_offs)[14] <- '3P%'
colnames(p_offs)[17] <- 'FT%'
colnames(p_offs)[26] <- '+/-'


# % columns
p_offs$`FG%` <- p_offs$`FG%` * 100
p_offs$`3P%` <- p_offs$`3P%` * 100
p_offs$`FT%` <- p_offs$`FT%` * 100

# recent -> last
p_offs <- p_offs[order(p_offs$DATE, decreasing = T), ]

# csv
write.csv(p_offs, './datasets/PO Logs.csv', row.names = F)


