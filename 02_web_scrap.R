#*****************************************************************************# 
# This script loads, wrangle and visualize TV shows scripts from the TV series#
# How I Met Your Mother (HIMYM) using the quanteda package                    #                                                            
#                                                                             #
#                                                                             #
# Depends on:                                                                 #
# First author: Jorge Roa                                                     # 
# E-mail: jurjoo@gmail.com                                                    #
# Second author: Augusto Fonseca                                              # 
# E-mail: cesaraccf@gmail.com                                                 #
# Third author: Alexander Kraes                                               #
# E-mail: alexander.kraess@sciencespo.fr                                      #
#                                                                             # 
#                                                                             #
#*****************************************************************************# 
#******************************************************************************
#******************************************************************************
# How we met Quanteda                                                         # 
# Analyzing the TV show 'How I Met Your Mother' with quanteda                 # 
#                                                                             # 
#                                                                             #
# *****************************************************************************
#******************************************************************************

rm(list = ls()) # to clean the workspace


# 01.- Load librarys -----------------------------------------------------------

library(readtext)
library(tidyverse)
library(rvest)
library(stringr)
library(xml2)
library(polite)
library(httr) #Package for working with HTTP organised by HTTP verbs 

# 02.- Web scrap TV shows scripts ----------------------------------------------


## 02.01- Define URLS and read HTML---------------------------------------------

v_tv_show <- "how-i-met-your-mother"

v_url_web <- "http://www.springfieldspringfield.co.uk/"

session_information <- bow(v_url_web) #Do a bow with the polite package
session_information

v_url <- paste(v_url_web,"episode_scripts.php?tv-show=", v_tv_show, sep="")

rvest_himym <- session(v_url, 
                       add_headers(`From` = "jurjoo@gmail.com", 
                                   `UserAgent` = R.Version()$version.string))

html_url_scrape <- rvest_himym %>% read_html(v_url)

node_selector <- ".season-episode-title"

directory_path <- paste("texts/how-i-met-your-mother/", v_tv_show, sep="")

## 02.02.-Loop for download TV scripts-------------------------------------------

### 02.02.01.-scrape href nodes in .season-episode-title-------------------------

html_url_all_seasons <- html_nodes(html_url_scrape, node_selector) %>%
  html_attr("href")

### 02.02.02.-One loop for all our URL's----------------------------------------

for (x in html_url_all_seasons) {
  read_ur <- read_html(paste(v_url_web, x, sep="/"))
  
  Sys.sleep(runif(1, 0, 1)) #Be polite
  
  # Element node that was checked and that contain the place of the scripts.
  selector <- ".scrolling-script-container"
  # Scrape the text
  text_html <- html_nodes(read_ur, selector) %>% 
    html_text()
  
  # Last five characters of html_url_all_seasons for saving this to separate text files (This is our pattern).
  sub_data <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }
  seasons_final <- sub_data(x, 5)
  # Write each text file
  write.csv(text_html, file = paste(directory_path, "_", seasons_final, ".txt", sep=""), row.names = FALSE)
}

# 03.- Webscrapp Tv Show tables--------------------------------------------------

## 03.01.- Information about tv episodes-----------------------------------------

url_himym <- "https://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_episodes"

rvest_himym_table <- session(url_himym, 
                             add_headers(`From` = "jurjoo@gmail.com", 
                                         `UserAgent` = R.Version()$version.string))

l_tables_himym <- rvest_himym_table %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

#This generates a list with all the tables that contain the page. In our case, 
#we want the table from the second element till the 10th. 
l_tables_himym <- l_tables_himym[c(2:10)]


### 03.01.01- Data cleaning to obtain clean tables---------------------------------

#Reduce the list in one data frame since all of the tables share the same structure 
df_himym <- data.frame(Reduce(bind_rows, l_tables_himym)) 


#We do the same for the characters of HIMYM
url_himym_characters <- "https://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_characters"

rvest_himym_table_2 <- session(url_himym_characters, 
                               add_headers(`From` = "jurjoo@gmail.com", 
                                           `UserAgent` = R.Version()$version.string))

l_tables_himym_characters <- rvest_himym_table_2 %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

df_characters <- as.data.frame(l_tables_himym_characters[[1]]) %>% 
  select(Character)

df_characters_w <- df_characters %>% 
  filter(!stringr::str_starts(Character, "Futu"),
         !(Character %in% c("Character", "Main Characters", 
                            "Supporting Characters"))) %>% 
  mutate(name = str_extract(Character,"([^ ]+)"),
         name = replace(name, name == "Dr.", "Sonya"))

### 03.01.02- Data cleaning to wrangle html tables------------------------------

df_himym <- data.frame(Reduce(bind_rows, l_tables_himym)) 

df_himym_filt <- df_himym %>% filter(str_length(No.overall) < 4)

df_himym_filt_dupl <- df_himym %>% filter(str_length(No.overall) > 4)

df_himym_filt_dupl_1 <- df_himym_filt_dupl %>% 
  mutate(No.overall = as.numeric(replace(No.overall, str_length(No.overall) > 4, substr(No.overall, 1, 3))),
         No..inseason = as.numeric(replace(No..inseason, str_length(No..inseason) > 3, substr(No..inseason, 1, 2))),
         Prod.code = replace (Prod.code, str_length(Prod.code) > 3, substr(Prod.code, 1, 6)))

df_himym_filt_dupl_2 <- df_himym_filt_dupl %>% 
  mutate(No.overall = as.numeric(replace(No.overall, str_length(No.overall) > 4, substr(No.overall, 4, 6))),
         No..inseason = as.numeric(replace(No..inseason, str_length(No..inseason) > 3, substr(No..inseason, 3, 4))),
         Title = replace(Title, Title == "\"The Magician's Code\"", "\"The Magician's Code Part 2\""),
         Title = replace(Title, Title == "\"The Final Page\"", "\"The Final Page Part 2\""),
         Title = replace(Title, Title == "\"Last Forever\"" , "\"Last Forever Part 2\"" ),
         Prod.code = replace(Prod.code, str_length(Prod.code) > 3, substr(Prod.code, 7, 12)))

df_himym_final <- bind_rows(df_himym_filt, 
                            df_himym_filt_dupl_1, 
                            df_himym_filt_dupl_2) %>% 
  arrange(No.overall, No..inseason) %>% 
  mutate(year = str_extract(Original.air.date, '[0-9]{4}+'),
         Season = as.numeric(stringr::str_extract(Prod.code, "^.{1}"))) %>% 
  rename(Chapter = No..inseason)

df_himym_final$US.viewers.millions. <- as.numeric(str_replace_all(df_himym_final$US.viewers.millions., "\\[[0-9]+\\]", ""))


# 04.- Load TV scripts and merge data-------------------------------------------
df_texts_himym <- readtext::readtext("texts/how-i-met-your-mother/*.txt")

v_season <- as.numeric(stringr::str_extract(df_texts_himym$doc_id, "\\d+"))

v_chapter <- as.numeric(stringi::stri_extract_last_regex(df_texts_himym$doc_id, "[0-9]+"))

df_texts_himym_w <- df_texts_himym %>% mutate(Season = v_season, Chapter = v_chapter)

df_himym_final_doc <- full_join(as.data.frame(df_texts_himym_w), df_himym_final, by = c("Season", "Chapter")) %>% 
  mutate(Season_w = paste("Season", Season),
         Title_season = paste0(Title, " S", Season, " EP", Chapter))


# 05.- Final dataframe to be used as a corpus text -----------------------------

#df_himym_final_doc_2 <- df_himym_final_doc %>% mutate(text = replace(text, is.character(text), str_remove_all(text, "\\w+[A-Z]")))
df_himym_final_doc

save(df_himym_final_doc, file = "data/df_himym_final_doc.Rdata")
save(df_characters_w, file = "data/df_characters_w.Rdata")
