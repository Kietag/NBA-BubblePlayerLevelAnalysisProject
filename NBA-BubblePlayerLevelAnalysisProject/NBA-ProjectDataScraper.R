### NBA-BubblePlayerLevelAnalysisProject

### This is a script for a webscraper to scrape data from Basketball-Reference player pages
### This is version 1.3


# Load libraries
library(rvest)
library(dplyr)
library(tidyr)
library(here)


# Create a blank data frame with 2 empty columns for player names and links to be scraped from the index page.
PlayerTable <- data.frame(Player = "", Links = "")

# Create a loop that progresses through the alphabet.
for (i in 1:26) {
  
  # Indexes on basketball-reference have a seperate page for each letter of the alphabet.
  # Keeps track of the current letter, to be used with 'paste0' to construct the correct index address to access.
  letCurrent = letters[i]
  
  # The url for the current page is created using the 'paste0' function.
  page <- read_html(paste0("https://www.basketball-reference.com/players/", letCurrent, "/"), as.data.frame=T)
  
  # A temporary cache of player links is created. All links from the players table are scraped.
  cachePlayerLinks <- page %>% html_nodes(xpath = '//*[@id="players"]') %>% html_nodes("a") %>%  html_attr('href')
  
  # Use subset to discard links that are not to players pages, such as college profiles.
  # Only links that include "players/./" are kept
  cachePlayerLinks <- subset(cachePlayerLinks, grepl("players/./", cachePlayerLinks))
  
  # Scrape the players table to a temporary cached data frame.
  cachePlayerTable <- page %>% html_nodes(xpath = '//*[@id="players"]') %>% 
    .[[1]] %>% html_table(fill = T)
  
  # Add the player page links as a column to the cached player table.
  cachePlayerTable$Links <- cachePlayerLinks
  
  # Included in the player table is when players started and ended play in the NBA.
  # Filter out all players who do not have 2020 season play in the range of when they played in the NBA.
  # This does not necessarily mean that they have data for the targeted 19/20 season. This issue will be resolved
  # in the data processing script.
  cachePlayerTable <- filter(cachePlayerTable, 2020 >= cachePlayerTable$From, 2020 <= cachePlayerTable$To)
  
  # Once irrelevant players have been dropped, keep only Player and Links columns and use
  # 'rbind' to add the temporary cached player table to the overall PlayerTable data frame.
  cachePlayerTable <- select(cachePlayerTable, Player, Links)
  PlayerTable <- rbind(PlayerTable, cachePlayerTable)
  
  # Sports Reference rules state "Currently we will block users sending requests to...
  # our sites more often than twenty requests in a minute."
  # Sys.Sleep is used to suspend operation after each request is processed, to avoid making too many requests of the site.
  Sys.sleep(3.2)
  
}

### End of Loop

# Drop the first empty row of the PlayerTable data frame. Also reset the row names to be
# numerically correct.
PlayerTable <- PlayerTable[-1,]
rownames(PlayerTable) <- NULL

# Create a blank frame for the final Advanced player data.
AdvancedLeague <- data.frame()
AdvancedPlayoff <- data.frame()

# A new loop will access each page for each player and extract the 'Advanced' table.
# Set an Increment counter to keep track of where the loop is for certain functions.
aInc <- 1


# Create a loop going through the list of url segments accompaning player name.
for (i in PlayerTable$Links) {
  
  # Set the url using the paste function, pulling the current players' web address from the Links column of PlayerTable.
  tryCatch({
    url <- paste("https://www.basketball-reference.com", PlayerTable$Links[[aInc]], sep = "")
    pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  # The advanced table is scraped using its' ID in the html. This is kept the same across all
  # basketball-reference player pages and is known as 'advanced' for league data and 'playoffs_advanced' for playoffs.
  # Both league and playoff data is scraped and stored temporarily in a data frame.    
    tempAdvancedLeague <- pageobj %>%  
     html_nodes("#advanced") %>% 
      .[[1]] %>% 
     html_table(fill=T)
    tempAdvancedLeague$LeaguePlayoff <- "League"
    
    tempAdvancedPlayoff <- pageobj %>%  
      html_nodes("#playoffs_advanced") %>% 
      .[[1]] %>% 
      html_table(fill=T)
    tempAdvancedPlayoff$LeaguePlayoff <- "Playoff"
    
    }, error = function(e){
      e
    })
  # Add a 'Name' column to the temporary dataset and include the name of the player whose data it is with all entries
  # associated with them.
   tempAdvancedLeague$Name <- PlayerTable$Player[[aInc]]
   tempAdvancedPlayoff$Name <- PlayerTable$Player[[aInc]]

  # 'rbind' is used to add the data from the temporary data frame to the overall 'Advanced' frame.
    AdvancedLeague <- rbind(AdvancedLeague, tempAdvancedLeague)
    AdvancedPlayoff <- rbind(AdvancedPlayoff, tempAdvancedPlayoff)
  
  # At the end of each loop cycle, the counter increments.  
    aInc <- aInc + 1
  
  # Sports Reference rules state "Currently we will block users sending requests to...
  # our sites more often than twenty requests in a minute."
  # Sys.Sleep is used to suspend operation after each request is processed, to avoid making too many requests of the site.
    Sys.sleep(3.2)

}

#### End of loop

# Use 'rbind' to combine the league and playoff data
Advanced <- rbind(AdvancedLeague, AdvancedPlayoff)

#Save the advanced datasets to the 'Data' folder.

here::here()
save(Advanced, file = file.path("data","Advanced.rda"))
