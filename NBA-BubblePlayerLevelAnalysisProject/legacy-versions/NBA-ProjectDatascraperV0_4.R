###This is a WIP Script for a webscraper to scrape data from Basketball-Reference player pages
###This is version 0.4.


# Load relevant libraries
library(rvest)
library(dplyr)

# Create blank frames
PerGame <- data.frame()
Advanced <- data.frame()

###COMMENTS FROM NEMANJA: One thing that you could do here is to iterate through the letters from a to z over this page https://www.basketball-reference.com/players/a/. You just change the letter at the end and scrape all hyperlinks.
#You can get all the hyperlinks with this type of code
#Loop:
#page <- read_html("https://www.basketball-reference.com/players/a/")
#A<-page %>% html_nodes("a") %>% html_attr('href') # Gets all the hyperlinks 
#B<-page %>% html_nodes("a") %>% html_text() # Gets the text behind the hyperlinks

# Next step is to filter the player-related hyperlinks. You can do that by taking all the links that regular expression (https://www.dummies.com/article/technology/programming-web-design/r/how-to-search-text-by-pattern-in-r-141639/) "/players/a/" inside so it would be "/players/" + "/letter that you are iterating through/"

# This gets you all the links for players. Then you put the rest of your pipeline that you have written.

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

aInc <- 1
wrongColCount <- 1

###################################################################################################################################  
for (i in sub_brefa) {
  

  url <- paste("https://www.basketball-reference.com", sub_brefa[[aInc]], sep = "")
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  tempPerGame <- pageobj %>%  
    html_nodes("#all_per_game-playoffs_per_game") %>% 
    .[[1]] %>% 
    html_table(fill=T)
  

  tempPerGame$Name <- c(sub_brefa[[aInc]])
  
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
  
  tempAdvanced$Name <- c(sub_brefa[[aInc]])
  
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
  
  
  aInc <- aInc + 1
  
}

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
###########################################################################



# Create list with each of the URL variables
PlayerList <- list(PAchiwua, SAdams, BAdebayo, LAldridge, NAlexanderWalker, GAllen, JAllen, JAlvarado, JAnderson, KAnderson
                   , GAntetokounmpo, TAntetokounmpo, CaAnthony, CoAnthony, OGAnunoby, RArcidiacono, TAriza, DJAugustin, DAvdija)

# Create a variable which will track the increment of the loop
ListIncrement <- 1

# Create a loop which will run until the end of the list of players
for (i in 1:length(PlayerList)) {
  
  # Set the URL using the current listed player variable using ListIncrement
  url <- PlayerList[[ListIncrement]]
  pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
  
  # Pulls the second object from the web page (Per Game stats table)
  tempPerGame <- pageobj %>%  
    html_nodes("table") %>% 
    .[[2]] %>% 
    html_table(fill=T)
  
  # Adds a column to the temporary pulled table which adds the player name as a column
  tempPerGame$Name <- c(paste(PlayerList[[ListIncrement]]))
  
  # Using the 'rbind' function, the contents of the temporary table are added to the total table
  PerGame <- rbind(PerGame, tempPerGame)
  
  # Same as previous script in the loop, but for the advanced stats table
  tempAdvanced <- pageobj %>%  
    html_nodes("table") %>% 
    .[[6]] %>% 
    html_table(fill=T)
  
  tempAdvanced$Name <- c(paste(PlayerList[[ListIncrement]]))
  
  Advanced <- rbind(Advanced, tempAdvanced)
  
  # Add 1 to the ListIncrement variable to keep track of loop progress
  ListIncrement <- ListIncrement + 1

}


### Have a count variable start at 1, which increments each time the loop is completed. This
### variable will be used to extract the next player variable from the list and in turn change the URL which
### data is pulled from.

