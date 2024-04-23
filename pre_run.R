library(dplyr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)


#### REGULAR SEASON ####

## Past RS Box Scores
rs_logs <- read.csv('./datasets/RS_Logs.csv')

# column names
colnames(rs_logs)[6] <- 'WL'
colnames(rs_logs)[11] <- 'FG_PCT'
colnames(rs_logs)[12] <- 'FG3M'
colnames(rs_logs)[13] <- 'FG3A'
colnames(rs_logs)[14] <- 'FG3_PCT'
colnames(rs_logs)[17] <- 'FT_PCT'
colnames(rs_logs)[26] <- 'pm'

##### Scrape Last Box Scores ====

# headers
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# scrape 
url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2023-24&SeasonType=Regular%20Season&Sorter=DATE'

res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

# df 
curr_rs <- data.frame(resp$resultSets$rowSet)

# assign column names 
colnames(curr_rs) <- resp[['resultSets']][['headers']][[1]]

# keep only relevant columns
curr_rs <- curr_rs[,-c(2,4,6,7,31,32)]

# rearrange in proper order 
curr_rs <- curr_rs[,c(1,2,3,5,4,6,25,7:24,26)]

# change column names
colnames(curr_rs)[1] <- 'SEASON'
colnames(curr_rs)[2] <- 'PLAYER'
colnames(curr_rs)[3] <- 'TEAM'
colnames(curr_rs)[5] <- 'DATE'
colnames(curr_rs)[6] <- 'WL'
colnames(curr_rs)[11] <- 'FG_PCT'
colnames(curr_rs)[12] <- 'FG3M'
colnames(curr_rs)[13] <- 'FG3A'
colnames(curr_rs)[14] <- 'FG3_PCT'
colnames(curr_rs)[17] <- 'FT_PCT'
colnames(curr_rs)[26] <- 'pm'

# from char to num
curr_rs <- curr_rs %>% mutate_at(7:26, as.numeric)

# % columns with two decimals
curr_rs$FG_PCT <- curr_rs$FG_PCT * 100
curr_rs$FG3_PCT <- curr_rs$FG3_PCT * 100
curr_rs$FT_PCT <- curr_rs$FT_PCT * 100

# from recent to last 
curr_rs <- curr_rs[order(curr_rs$DATE, decreasing = T), ]

##### Final Regular Season DF ====
# merge past RS with current 
rs_tot <- rbind(curr_rs, rs_logs)

# limit number of digits
rs_tot$FG_PCT <- round(rs_tot$FG_PCT, 1)
rs_tot$FG3_PCT <- round(rs_tot$FG3_PCT, 1)
rs_tot$FT_PCT <- round(rs_tot$FT_PCT, 1)

# modify season column (remove first "2")
rs_tot <- rs_tot %>% mutate(across(c('SEASON'), substr, 2, nchar(SEASON)))

# modify column season to have beginning/end season years (eg: from 2023 to 2023 - 2024)
rs_tot$SEASON <- as.numeric(rs_tot$SEASON)
rs_tot$SEASON <- rs_tot$SEASON + 1

# rearrange columns
rs_tot <- rs_tot[,c(1:6,8,7,18:26,9:17)]

# Write CSV
write.csv(rs_tot, './datasets/Regular_Season_Logs.csv', row.names = F)


#### PLAYOFFS ####

# Playoff Box Scores
p_offs <- read.csv('./datasets/PO_Logs.csv')

# column names
colnames(p_offs)[6] <- 'WL'
colnames(p_offs)[11] <- 'FG_PCT'
colnames(p_offs)[12] <- 'FG3M'
colnames(p_offs)[13] <- 'FG3A'
colnames(p_offs)[14] <- 'FG3_PCT'
colnames(p_offs)[17] <- 'FT_PCT'
colnames(p_offs)[26] <- 'pm'


# headers
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# scrape 
url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2023-24&SeasonType=Playoffs&Sorter=DATE'

res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

# df 
curr_po <- data.frame(resp$resultSets$rowSet)

# assign column names 
colnames(curr_po) <- resp[['resultSets']][['headers']][[1]]

# keep only relevant columns
curr_po <- curr_po[,-c(2,4,6,7,31,32)]

# rearrange in proper order 
curr_po <- curr_po[,c(1,2,3,5,4,6,25,7:24,26)]

# change column names
colnames(curr_po)[1] <- 'SEASON'
colnames(curr_po)[2] <- 'PLAYER'
colnames(curr_po)[3] <- 'TEAM'
colnames(curr_po)[5] <- 'DATE'
colnames(curr_po)[6] <- 'WL'
colnames(curr_po)[11] <- 'FG_PCT'
colnames(curr_po)[12] <- 'FG3M'
colnames(curr_po)[13] <- 'FG3A'
colnames(curr_po)[14] <- 'FG3_PCT'
colnames(curr_po)[17] <- 'FT_PCT'
colnames(curr_po)[26] <- 'pm'

# from char to num
curr_po <- curr_po %>% mutate_at(7:26, as.numeric)

# % columns with two decimals
curr_po$FG_PCT <- curr_po$FG_PCT * 100
curr_po$FG3_PCT <- curr_po$FG3_PCT * 100
curr_po$FT_PCT <- curr_po$FT_PCT * 100

# from recent to last 
curr_po <- curr_po[order(curr_po$DATE, decreasing = T), ]

## Final PO dataframe ====

# merge past RS with current 
p_offs <- rbind(curr_po, p_offs)

# limit number of digits
p_offs$FG_PCT <- round(p_offs$FG_PCT, 1)
p_offs$FG3_PCT <- round(p_offs$FG3_PCT, 1)
p_offs$FT_PCT <- round(p_offs$FT_PCT, 1)

# modify season column (remove first "2")
p_offs <- p_offs %>% mutate(across(c('SEASON'), substr, 2, nchar(SEASON)))

# modify column season to have beginning/end season years (eg: from 2023 to 2023 - 2024)
p_offs$SEASON <- as.numeric(p_offs$SEASON)
p_offs$SEASON <- p_offs$SEASON + 1

# rearrange columns
p_offs <- p_offs[,c(1:6,8,7,18:26,9:17)]

# write csvs
write.csv(p_offs, './datasets/Playoff_Logs.csv', row.names = F)



