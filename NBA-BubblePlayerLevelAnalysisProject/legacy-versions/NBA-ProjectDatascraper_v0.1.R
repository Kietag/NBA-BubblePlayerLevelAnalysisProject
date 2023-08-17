###This is a WIP Script for a webscraper to scrape data from Basketball-Reference
###This is version 0.1.

# Load relevant libraries
library(rvest)

PerGame <- data.frame()
Advanced <- data.frame()

PAchiwua <- "https://www.basketball-reference.com/players/a/achiupr01.html"
SAdams <- "https://www.basketball-reference.com/players/a/adamsst01.html"
BAdebayo <- "https://www.basketball-reference.com/players/a/adebaba01.html"
SAldama <- "https://www.basketball-reference.com/players/a/aldamsa01.html"
LAldridge <- "https://www.basketball-reference.com/players/a/aldrila01.html"
NAlexanderWalker <- "https://www.basketball-reference.com/players/a/alexani01.html"
GAllen <- ""
JAllen <- ""
JAlvarado <- ""
JAnderson <- ""
KAnderson <- ""
GAntetokounmpo <- ""
TAntetokounmpo <- ""
CaAnthony <- ""
CoAnthony <- ""
OGAnunoby <- ""
RArcidiacono <- ""
TAriza <- ""
DJAugustin <- ""
DAvdija <- ""

PlayerList <- list(PAchiwua, SAdams, BAdebayo, SAldama, LAldridge, NAlexanderWalker)

ListIncrement <- 1

for (i in 1:length(PlayerList)) {
  
  url <- PlayerList[[ListIncrement]]
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("table") %>% 
    .[[2]] %>% 
    html_table(fill=T)
  
  tempPerGame$Name <- c(paste(PlayerList[[ListIncrement]]))
  
  PerGame <- rbind(PerGame, tempPerGame)
  
  
  tempAdvanced <- pageobj %>%  
    html_nodes("table") %>% 
    .[[6]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(paste(PlayerList[[ListIncrement]]))
  
  Advanced <- rbind(Advanced, tempAdvanced)
  
  ListIncrement <- ListIncrement + 1

}


### Have a count variable start at 1, which increments each time the loop is completed. This
### variable will be used to extract the next player variable from the list and in turn change the URL which
### data is pulled from.

