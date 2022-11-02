library(textdata)
library(quanteda)

# Loading the required libraries
library(rvest)

## Loading required package: xml2
library(xml2)


# Which tv show, if you want another show, first check on the website which tv show url is used. 
tvshow <- "how-i-met-your-mother"

# Creating download directory and change to it
directory = paste("texts/", tvshow, sep="")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)

# Setting base url and complte url
baseurl <- "http://www.springfieldspringfield.co.uk/"
url <- paste(baseurl,"episode_scripts.php?tv-show=", tvshow, sep="")

scrape_url <- read_html(url)
# node selector
s_selector <- ".season-episode-title"

# scrape href nodes in .season-episode-title
all_urls_season <- html_nodes(scrape_url, s_selector) %>%
  html_attr("href")

str(all_urls_season)

head(all_urls_season)


# Loop through all season urls 
for (i in all_urls_season) {
  uri <- read_html(paste(baseurl, i, sep="/"))
  # same thing here first check which node we need to select, so forst do a inspect of the site
  script_selector <- ".scrolling-script-container"
  # scrape all script text to a variable
  text <- html_nodes(uri, script_selector) %>% 
    html_text()
  
  # Get last five characters of all_urls_season as season for saving this to seperate text files
  substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
  }
  seasons <- substrRight(i, 5)
  # Write each script to a seperate text file
  write.csv(text, file = paste(directory, "/", tvshow, "_", seasons, ".txt", sep=""), row.names = FALSE)
}

# set filepath to scripts
cname <- file.path(directory)
# see if the filepath contains our scripts
docname <- dir(cname)

library(readtext)

datalist_himym <- readtext::readtext("texts/how-i-met-your-mother/*.txt")
