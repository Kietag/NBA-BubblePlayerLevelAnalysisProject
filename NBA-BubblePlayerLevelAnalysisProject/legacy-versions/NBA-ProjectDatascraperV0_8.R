###This is a WIP Script for a webscraper to scrape data from Basketball-Reference player pages
###This is version 0.8. -#! WIP !#-


# Load relevant libraries
library(rvest)
library(dplyr)
library(janitor)
library(tidyr)
library(forcats)
library(lmerTest)
library(ggplot2)
library(lme4)

# Create blank frames
PerGame <- data.frame()
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
}


# Use the subset function to select only relevant addresses with the "players/[LETTER]/" segment
# "." is a wildcard character, the use of which eliminates a few addresses that simply end in "players/"
subs_brefLinks <- subset(brefLinks, grepl("players/./", brefLinks))

###############################################################################################################################

# A new loop will access each page for each player and extract the 'Per Game' and 'Advanced' table.
# Set an Increment counter to keep track of where the loop is for certain functions
aInc <- 1

# Create a counter for tables of the wrong size. This will be explained later
wrongColCount <- 1
# These counters will be reset each loop

# Create a loop going through each alphabetised directory
for (i in subs_brefLinks) {
  
  # Set the url using the paste function, pulling the current players' web address from the subset.
  url <- paste("https://www.basketball-reference.com", subs_brefLinks[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  # Tables are scraped from the website using it's table ID in the html. This is universal across all basketball-reference player pages.
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  # Add a name column to the temporary dataset
  tempPerGame$Name <- c(subs_brefLinks[[aInc]])
  
  # Use the ncol function to check the number of columns of the current temporary 'Per Game' table.
  # basketball-reference contains certain older tables, formatted differently
  # with fewer statistics which are irrelevant to the current project.
  colCheck <- ncol(tempPerGame)
  
  # Due to the differing number of columns these older tables can't be bound using 'rbind'
  # An if else statement is used to check if the tempPerGame frame has the correct number of columns.
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  # The advanced data set is scraped using its table ID.   
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(subs_brefLinks[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  # At the end of each loop cycle, the counter increments.  
  aInc <- aInc + 1
  
  # Sports Reference rules state "Currently we will block users sending requests to...
  # our sites more often than twenty requests in a minute."
  # Sys.Sleep is used to suspend operation after each request is processed, to avoid making too many requests of the site.
  Sys.sleep(3.5)
  
}

########################################################################### END OF LOOPS
