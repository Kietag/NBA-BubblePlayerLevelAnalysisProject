###This is a WIP Script for a webscraper to scrape data from Basketball-Reference player pages
###This is version 0.5.


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

# Pull all hyperlinks from the directory for players in alphabetical order
page <- read_html("https://www.basketball-reference.com/players/a/")
brefa <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/b/")
brefb <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/c/")
brefc <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/d/")
brefd <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/e/")
brefe <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/f/")
breff <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/g/")
brefg <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/h/")
brefh <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/i/")
brefi <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/j/")
brefj <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/k/")
brefk <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/l/")
brefl <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/m/")
brefm <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/n/")
brefn <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/o/")
brefo <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/p/")
brefp <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/q/")
brefq <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/r/")
brefr <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/s/")
brefs <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/t/")
breft <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/u/")
brefu <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/v/")
brefv <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/w/")
brefw <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/x/")
brefx <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/y/")
brefy <- page %>% html_nodes("a") %>% html_attr('href')

page <- read_html("https://www.basketball-reference.com/players/z/")
brefz <- page %>% html_nodes("a") %>% html_attr('href')

# Use the subset function to select only relevant links with the "players/[LETTER]/" segment
sub_brefa <- subset(brefa, grepl("players/a/", brefa))
sub_brefb <- subset(brefb, grepl("players/b/", brefb))
sub_brefc <- subset(brefc, grepl("players/c/", brefc))
sub_brefd <- subset(brefd, grepl("players/d/", brefd))
sub_brefe <- subset(brefe, grepl("players/e/", brefe))
sub_breff <- subset(breff, grepl("players/f/", breff))
sub_brefg <- subset(brefg, grepl("players/g/", brefg))
sub_brefh <- subset(brefh, grepl("players/h/", brefh))
sub_brefi <- subset(brefi, grepl("players/i/", brefi))
sub_brefj <- subset(brefj, grepl("players/j/", brefj))
sub_brefk <- subset(brefk, grepl("players/k/", brefk))
sub_brefl <- subset(brefl, grepl("players/l/", brefl))
sub_brefm <- subset(brefm, grepl("players/m/", brefm))
sub_brefn <- subset(brefn, grepl("players/n/", brefn))
sub_brefo <- subset(brefo, grepl("players/o/", brefo))
sub_brefp <- subset(brefp, grepl("players/p/", brefp))
sub_brefq <- subset(brefq, grepl("players/q/", brefq))
sub_brefr <- subset(brefr, grepl("players/r/", brefr))
sub_brefs <- subset(brefs, grepl("players/s/", brefs))
sub_breft <- subset(breft, grepl("players/t/", breft))
sub_brefu <- subset(brefu, grepl("players/u/", brefu))
sub_brefv <- subset(brefv, grepl("players/v/", brefv))
sub_brefw <- subset(brefw, grepl("players/w/", brefw))
sub_brefx <- subset(brefx, grepl("players/x/", brefx))
sub_brefy <- subset(brefy, grepl("players/y/", brefy))
sub_brefz <- subset(brefz, grepl("players/z/", brefz))


###############################################################################################################################

# Set an Increment counter to keep track of where the loop is for certain functions
aInc <- 1

# Create a counter for tables of the wrong size. This will be explained later
wrongColCount <- 1
# These counters will be reset each loop

# Create a loop going through each alphabetised directory
for (i in sub_brefa) {
  
# Set the url using the paste function, pulling the current players' web address from the subset
  url <- paste("https://www.basketball-reference.com", sub_brefa[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
# Tables are scraped from the website using it's table ID in the html. This is universal across all basketballreference player pages.
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
# Add a name column to the temporary dataset
  tempPerGame$Name <- c(sub_brefa[[aInc]])
  
# Use the ncol function to check the number of columns of the current temporary 'Per Game' table.
# BasketballReference contains certain very old tables with fewer statistics which are irrelevant to the current project.
  colCheck <- ncol(tempPerGame)

# Due to the differing number of columns these older tables can't be bound using 'rbind'
# An if else statement is used to check if the tempPerGame fra,e has the correct number of columns.
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
  
  tempAdvanced$Name <- c(sub_brefa[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }

# At the end of each loop cycle, the counter increments.  
  aInc <- aInc + 1
  
}
 
# Between each loop, the counters are reset to 1.
aInc <- 1
wrongColCount <- 1

###########################################################################################################################

for (i in sub_brefb) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefb[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempPerGame$Name <- c(paste(sub_brefb[[aInc]]))
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefb[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefc) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefc[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefc[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefc[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefd) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefd[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefd[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefd[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefe) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefe[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefe[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefe[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_breff) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_breff[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_breff[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_breff[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefg) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefg[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefg[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefg[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefh) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefh[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefh[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefh[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefi) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefi[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefi[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefi[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefj) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefj[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefj[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefj[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1
###################################################################################################################################  
for (i in sub_brefk) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefk[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefk[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefk[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefl) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefl[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefl[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefl[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefm) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefm[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefm[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefm[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefn) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefn[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefn[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefn[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefo) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefo[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefo[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefo[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefp) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefp[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefp[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefp[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefq) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefq[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefq[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefq[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefr) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefr[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefr[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefr[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefs) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefs[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefs[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefs[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_breft) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_breft[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_breft[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_breft[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefu) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefu[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefu[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefu[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefv) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefv[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefv[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefv[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefw) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefw[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefw[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefw[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefx) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefx[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefx[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefx[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefy) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefy[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefy[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefy[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
}

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefz) {
  
  
  url <- paste("https://www.basketball-reference.com", sub_brefz[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  
  tempPerGame$Name <- c(sub_brefz[[aInc]])
  
  colCheck <- ncol(tempPerGame)
  
  if (colCheck == 31){
    PerGame <- rbind(PerGame, tempPerGame)
    
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  tempAdvanced <- pageobj %>%  
    html_nodes("#all_advanced-playoffs_advanced") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(sub_brefz[[aInc]])
  
  colCheckA <- ncol(tempAdvanced)
  
  if (colCheckA == 30){
    Advanced <- rbind(Advanced, tempAdvanced)
  }
  else {
    wrongColCount <- wrongColCount + 1
  }
  
  aInc <- aInc + 1
  
  
}

aInc <- 1
wrongColCount <- 1
########################################################################### END OF LOOPS


# Data Cleaning

RFPerGame <- PerGame
RFAdvanced <- Advanced

RFPerGame <- subset(RFPerGame, !grepl("Did Not Play", RFPerGame$Tm))

RFPerGame$Age <- as.numeric(as.character(RFPerGame$Age))
RFAdvanced$Age <- as.numeric(as.character(RFAdvanced$Age))

RFPerGame <- RFPerGame[!is.na(RFPerGame$Age), ]
RFAdvanced <- RFAdvanced[!is.na(RFAdvanced$Age), ]



RFPerGame <- subset(RFPerGame, !grepl("Career", RFPerGame$Season))
RFPerGame <- subset(RFPerGame, !grepl("season", RFPerGame$Season))
RFPerGame <- subset(RFPerGame, !grepl("season", RFPerGame$Season))


RFAdvanced<- subset(RFAdvanced, !grepl("Career", RFAdvanced$Season))
RFAdvanced<- subset(RFAdvanced, !grepl("season", RFAdvanced$Season))
RFAdvanced<- subset(RFAdvanced, !grepl("Season", RFAdvanced$Season))

RFAdvanced <- subset(RFAdvanced, select = -c(20,25))

#################################################################################

# Removes entries from any player who did not compete in the 19/20 season
SCAdvanced <- RFAdvanced %>%  group_by(Name) %>% filter(any(Season == "2019-20"))
SCPerGame <- RFPerGame %>%  group_by(Name) %>% filter(any(Season == "2019-20"))

# Renames each season to the first year of each season; this is then turned into a numeric column
SCAdvanced$Season <- substr(SCAdvanced$Season, 1, 4)
SCAdvanced$Season <- as.numeric(SCAdvanced$Season)

# Calculate player experience as the range of a given players seasons
SCAdvanced <- SCAdvanced %>% group_by(Name) %>% mutate(Exp = (max(Season) - min(Season))) %>% ungroup()

# Code 2019 seasons as during COVID-19
SCAdvanced$COVID <- ifelse(SCAdvanced$Season == 2019, 1, 0)
# 1 = COVID, 0 = Non-Covid

# Convert relevant columns to numeric
SCAdvanced$Age <- as.numeric(SCAdvanced$Age)
SCAdvanced$PER <- as.numeric(SCAdvanced$PER)
SCAdvanced$G <- as.numeric(SCAdvanced$G)
SCAdvanced$MP <- as.numeric(SCAdvanced$MP)


SCAdvanced$Pos <- as.factor(SCAdvanced$Pos)
levels(SCAdvanced$Pos)



# Create a df arranged by season, grouped by player
ArrSCAdvanced <- SCAdvanced %>% group_by(Name) %>% arrange(SCAdvanced, Season, .by_group = TRUE)

# Create a df containing only league fixtures by grouping by both player and season, and taking only where minutes are higher
ArrSCAdvancedLea <- ArrSCAdvanced %>% group_by(Name, Season) %>% slice_max(MP)
# Create a playoff df, created as only differing rows from the full df and league df
ArrSCAdvancedPlay <- setdiff(ArrSCAdvanced, ArrSCAdvancedLea)

ArrSCAdvanced$Pos <- fct_collapse(ArrSCAdvanced$Pos, C = c("C", "C,PF", "C,SF"), PF = c("PF", "PF,SF", "PF,SG"), PG = c("PG", "PG,SF,SG", "PG,SG"), SF = c("SF", "SF,SG"), SG = c("SG"))
ArrSCAdvancedLea$Pos <- fct_collapse(ArrSCAdvancedLea$Pos, C = c("C", "C,PF", "C,SF"), PF = c("PF", "PF,SF", "PF,SG"), PG = c("PG", "PG,SF,SG", "PG,SG"), SF = c("SF", "SF,SG"), SG = c("SG"))
ArrSCAdvancedPlay$Pos <- fct_collapse(ArrSCAdvancedPlay$Pos, C = c("C", "C,PF", "C,SF"), PF = c("PF", "PF,SF", "PF,SG"), PG = c("PG", "PG,SF,SG", "PG,SG"), SF = c("SF", "SF,SG"), SG = c("SG"))


ArrSCAdvancedLea$LeagPlay <- "League"
ArrSCAdvancedPlay$LeagPlay <- "Playoff"
ArrSCAdvanced$COVID <- as.factor(ArrSCAdvanced$COVID)
ArrSCAdvancedLea$COVID <- as.factor(ArrSCAdvancedLea$COVID)
ArrSCAdvancedPlay$COVID <- as.factor(ArrSCAdvancedPlay$COVID)
levels(ArrSCAdvanced$COVID)




#################################################################################### Data Analysis

DescStats <- c("Age", "G", "MP", "PER", "Exp")
DescStatsArrSCAdvanced <- ArrSCAdvanced[DescStats]
summary(DescStatsArrSCAdvanced)

# Standardisation


ArrSCAdvanced[c("PER", "Age", "MP", "Exp")] <- scale(ArrSCAdvanced[c("PER", "Age", "MP", "Exp")], center = TRUE, scale = TRUE)
ArrSCAdvancedLea[c("PER", "Age", "MP", "Exp")] <- scale(ArrSCAdvancedLea[c("PER", "Age", "MP", "Exp")], center = TRUE, scale = TRUE)
ArrSCAdvancedPlay[c("PER", "Age", "MP", "Exp")] <- scale(ArrSCAdvancedPlay[c("PER", "Age", "MP", "Exp")], center = TRUE, scale = TRUE)






##################################################################################

setwd()
save(SCAdvanced, file = "SCAdvanced.Rdata")
save(SCPerGame, file = "SCPerGame.Rdata")

save(ArrSCAdvanced, file = "ArrSCAdvanced.Rdata")
save(ArrSCAdvancedLea, file = "ArrSCAdvancedLea.Rdata")
save(ArrSCAdvancedPlay, file = "ArrSCAdvancedPlay.Rdata")





