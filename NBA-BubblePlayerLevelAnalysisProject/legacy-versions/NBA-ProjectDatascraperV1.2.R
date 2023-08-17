### This is a script for a webscraper to scrape data from Basketball-Reference player pages
### This is version 1.2


# Load libraries
library(rvest)
library(dplyr)
library(janitor)
library(tidyr)
library(forcats)
library(lmerTest)
library(ggplot2)
library(lme4)
library(here)

# Create blank frames
Advanced <- data.frame()

# Pull all addresses from the directory for players in alphabetical order. This will include some extraneous addresses
# to be removed later.

# Create a blank vector for the addresses which are collected as the loop progresses
brefLinks <- ""

for (i in 1:26) {
  
  # Keeps track of the current letter, to be used with 'paste0' to construct the correct address to access.
  letCurrent = letters[i]
  
  page <- read_html(paste0("https://www.basketball-reference.com/players/", letCurrent, "/"))
  tempBrefLinks <- page %>% html_nodes("a") %>% html_attr('href')
  brefLinks <- c(brefLinks, tempBrefLinks)
  
  Sys.sleep(3.2)
  
}

PlayerTable <- data.frame(Player = "", Links = "")

page <- read_html(paste0("https://www.basketball-reference.com/players/", letcurrent, "/"), as.data.frame=T)
cachePlayerLinks <- page %>% html_nodes(xpath = '//*[@id="players"]') %>% html_nodes("a") %>%  html_attr('href')
cachePlayerLinks <- subset(cachePlayerLinks, grepl("players/./", cachePlayerLinks))

cachePlayerTable <- page %>% html_nodes(xpath = '//*[@id="players"]') %>% 
  .[[1]] %>% html_table(fill = T)

cachePlayerTable$Links <- cachePlayerLinks

cachePlayerTable <- filter(cachePlayerTable, 2020 >= cachePlayerTable$From, 2020 <= cachePlayerTable$To)

cachePlayerTable <- select(cachePlayerTable, Player, Links)
PlayerTable <- rbind(PlayerTable, cachePlayerTable)

# Use the subset function to select only relevant addresses with the "players/[LETTER]/" segment
# "." is a wildcard character, the use of which eliminates a few addresses that simply end in "players/"
subs_brefLinks <- subset(brefLinks, grepl("players/./", brefLinks))

###############################################################################################################################

# A new loop will access each page for each player and extract the 'Advanced' table.
# Set an Increment counter to keep track of where the loop is for certain functions
aInc <- 1

# Create a counter for tables of the wrong size. This will be explained later
wrongColCount <- 1
# These counters will be reset each loop

# Create a loop going through each alphabetised directory
for (i in subs_brefLinks) {
  
  # Set the url using the paste function, pulling the current players' web address from the subset.
  tryCatch({
    url <- paste("https://www.basketball-reference.com", subs_brefLinks[[aInc]], sep = "")
    pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  # Tables are scraped from the website using its table ID in the html. This is universal across all
  # basketball-reference player pages.
    
  # Check if the advanced table is present. Some players who are just drafted do not
  # have this table, which would result in an error.
    
    advancedTablePresent <- pageobj %>% html_node("#all_advanced-playoffs_advanced")
    
    if (!is.na(advancedTablePresent)) {
      
    tempAdvanced <- pageobj %>%  
     html_nodes("#all_advanced-playoffs_advanced") %>% 
      .[[1]] %>% 
     html_table(fill=T)
    
    } else {
      
      tempAdvanced <- data.frame(Name = NA)
      
    }
  # Add a name column to the temporary dataset
   tempAdvanced$Name <- c(subs_brefLinks[[aInc]])
  
  # Use the ncol function to check the number of columns of the current temporary 'Advanced' table.
  # basketball-reference contains certain older tables, formatted differently
  # with fewer statistics which are irrelevant to the current project.
    colCheckA <- ncol(tempAdvanced)
  
  # Due to the differing number of columns these older tables can't be bound using 'rbind'
  # An if else statement is used to check if the tempAdvanced frame has the correct number of columns.
    if (colCheckA == 30){
      Advanced <- rbind(Advanced, tempAdvanced)
    }
    else {
      wrongColCount <- wrongColCount + 1
   }
  
  # At the end of each loop cycle, the counter increments.  
    aInc <- aInc + 1
    currentPlayer <- subs_brefLinks[[aInc]]
  
  # Sports Reference rules state "Currently we will block users sending requests to...
  # our sites more often than twenty requests in a minute."
  # Sys.Sleep is used to suspend operation after each request is processed, to avoid making too many requests of the site.
    Sys.sleep(3.2)
  })
}

#### END OF LOOP

#Save the advanced datasets to the 'Data' folder.

here::here()
save(Advanced, file = file.path("R Project","data","Advanced.rda"))
