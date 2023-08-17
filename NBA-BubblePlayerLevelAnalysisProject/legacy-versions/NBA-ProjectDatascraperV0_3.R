###This is a WIP Script for a webscraper to scrape data from Basketball-Reference player pages
###This is version 0.2.


# Load relevant libraries
library(rvest)

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
  


# Create variables with the URL to the appropriate webpage attached
PAchiwua <- "https://www.basketball-reference.com/players/a/achiupr01.html"
SAdams <- "https://www.basketball-reference.com/players/a/adamsst01.html"
BAdebayo <- "https://www.basketball-reference.com/players/a/adebaba01.html"
LAldridge <- "https://www.basketball-reference.com/players/a/aldrila01.html"
NAlexanderWalker <- "https://www.basketball-reference.com/players/a/alexani01.html"
GAllen <- "https://www.basketball-reference.com/players/a/allengr01.html"
JAllen <- "https://www.basketball-reference.com/players/a/allenja01.html"
JAlvarado <- "https://www.basketball-reference.com/players/a/alvarjo01.html"
JAnderson <- "https://www.basketball-reference.com/players/a/anderju01.html"
KAnderson <- "https://www.basketball-reference.com/players/a/anderky01.html"
GAntetokounmpo <- "https://www.basketball-reference.com/players/a/antetgi01.html"
TAntetokounmpo <- "https://www.basketball-reference.com/players/a/antetth01.html"
CaAnthony <- "https://www.basketball-reference.com/players/a/anthoca01.html"
CoAnthony <- "https://www.basketball-reference.com/players/a/anthoco01.html"
OGAnunoby <- "https://www.basketball-reference.com/players/a/anunoog01.html"
RArcidiacono <- "https://www.basketball-reference.com/players/a/arcidry01.html"
TAriza <- "https://www.basketball-reference.com/players/a/arizatr01.html"
DJAugustin <- "https://www.basketball-reference.com/players/a/augusdj01.html"
DAvdija <- "https://www.basketball-reference.com/players/a/avdijde01.html"

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

