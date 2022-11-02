library(readtext)
library(tidyverse)
library(quanteda)
library(rvest)
library(quanteda.textstats)
library(quanteda.textplots)
library(stringr)
library(spacyr)
library(xml2)
library(ggsci)


## Webscrapp Tv Show tables

datalist_himym <- readtext::readtext("texts/how-i-met-your-mother/*.txt")

url_himym <- "https://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_episodes"

url_himym_characters <- "https://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_characters"

l_tables_himym <- url_himym %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

#This generates a list with all the tables that contain the page. In our case, 
#we want the table from the second element till the 10th. 
l_tables_himym <- l_tables_himym[c(2:10)]



#Data cleaning to obtain clean tables

#Reduce the list in one data frame since all of the tables share the same structure 
df_himym <- data.frame(Reduce(bind_rows, l_tables_himym)) 

#We do the same for the characters of HIMYM
l_tables_himym_characters <- url_himym_characters %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

df_characters <- as.data.frame(l_tables_himym_characters[[1]]) %>% 
  select(Character)

df_characters_w <- df_characters %>% 
  filter(!stringr::str_starts(Character, "Futu"),
         !(Character %in% c("Character", "Main Characters", "Supporting Characters"))) %>% 
  mutate(name = str_extract(Character,"([^ ]+)"),
         name = replace(name, name == "Dr.", "Sonya"))

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

datalist_himym <- readtext::readtext("texts/how-i-met-your-mother/*.txt")

v_season <- as.numeric(stringr::str_extract(datalist_himym$doc_id, "\\d+"))

v_chapter <- as.numeric(stringi::stri_extract_last_regex(datalist_himym$doc_id, "[0-9]+"))

datalist_himym_w <- datalist_himym %>% mutate(Season = v_season, Chapter = v_chapter)

df_himym_final_doc <- full_join(as.data.frame(datalist_himym_w), df_himym_final, by = c("Season", "Chapter")) %>% 
  mutate(Season_w = paste("Season", Season))

## Final dataframe to be used as a corpus text

df_himym_final_doc

# First Step: Define a corpus

corp_himym <- corpus(df_himym_final_doc)  # build a new corpus from the texts

docnames(corp_himym) <- df_himym_final_doc$Title

summary(corp_himym, n = 15)


## Convert corpus into tokens and wrangle it

corp_himym_s1 <- corpus_subset(corp_himym, Season == 1)

toks_himym_s1 <- tokens(corp_himym_s1, remove_punct = TRUE,
                        remove_separators = TRUE, 
                        remove_numbers = TRUE, 
                        remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))

toks_himym_dm_s1 <- toks_himym_s1 %>% dfm()


tstat_simil <- textstat_simil(toks_himym_dm_s1)

tstat_dist <- textstat_dist(toks_himym_dm_s1)




### Similarity between episodes

clust <- hclust(as.dist(tstat_simil))
dclust <- as.dendrogram(clust)

dclust <- reorder(dclust, 1:22)

nodePar <- list(lab.cex = 1, pch = c(NA, 19), 
                cex.axis=1.5,
                cex = 2, col = "#0080ff")

#Talk about different methods above the correlation 
par(mar=c(15,7,2,1))

plot(dclust,nodePar = nodePar,
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,
     main="How I Met Your Mother Season 1",
     type = "triangle",ylim=c(0,1),
     ylab = "Similarity between episodes (correlation %)",
     edgePar = list(col = 4:7, lwd = 7:7),
     panel.first = abline(h=c(seq(.10, 1, .10)), col="grey80"))

rect.hclust(clust, k = 5, border = "#ffb1b1")


## Distance between episodes (by correlation)

tstat_dist <- textstat_dist(toks_himym_dm_s1)
clust_dist <- hclust(as.dist(tstat_dist))
dclust_dist <- as.dendrogram(clust_dist)

dclust_dist <- reorder(dclust_dist, 22:1)

nodePar_2 <- list(lab.cex = 1.2, pch = c(NA, 19), 
                  cex = 1.8, col = "#ffc733")

par(mar=c(15,7,2,1))

plot(dclust_dist, nodePar = nodePar_2,
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,
     main="How I Met Your Mother Season 1",
     type = "triangle",ylim=c(0,120),
     ylab = "Distance between episodes (correlation %)",
     edgePar = list(col = 18:19, lwd = 7:7),
     panel.first =abline(h=c(seq(10, 120, 10)), col="grey80"))
rect.hclust(clust_dist, k = 5, border = "#d80000")


# Appearances of actors by season

## Characters by season

toks_himym <- tokens(corp_himym, remove_punct = TRUE,
                     remove_separators = TRUE, 
                     remove_numbers = TRUE, 
                     remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))#Add additional words

df_actors <- toks_himym %>% tokens_select(c("Ted", "Marshall", "Lily", "Robin", "Barney", "Mother")) %>%
  tokens_group(groups = Season) %>% 
  dfm()


df_final_actors <-  as.data.frame(textstat_frequency(df_actors, groups = c(1:9)))

ggplot(df_final_actors, aes(x = group, y = frequency, group = feature, color = feature)) +
  geom_line() +
  geom_point() +
  ylim(0,580)



## Wordcloud of principal characters that appears in HIMYM

toks_himym_characters <- tokens(corp_himym, remove_punct = TRUE,
                                remove_separators = TRUE, 
                                remove_numbers = TRUE, 
                                remove_symbols = TRUE) %>% 
  tokens_keep(c(unique(df_characters_w$name)))

dfm_general_characters <- toks_himym_characters %>%
  dfm()


textplot_wordcloud(dfm_general_characters, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   min_count = 1, #Minimum frequency
                   color = RColorBrewer::brewer.pal(4, "Dark2"))


## Wordcloud of secondary characters that appears in HIMYM 


toks_himym_sec_characters <- tokens(corp_himym, 
                                    remove_punct = TRUE,
                                    remove_separators = TRUE, 
                                    remove_numbers = TRUE, 
                                    remove_symbols = TRUE) %>% 
  tokens_keep(c(unique(df_characters_w$name))) %>% 
  tokens_remove(c("Ted", "Barney", "Lily", "Robin", "Marshall"))

dfm_general_sec_characters <- toks_himym_sec_characters %>%
  dfm()

textplot_wordcloud(dfm_general_sec_characters, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   #comparison = TRUE,
                   labelsize = 1.5,
                   min_count = 1, #Minimum frequency
                   color = RColorBrewer::brewer.pal(8, "Spectral"))


## spaCy and spaCyr 


#Explain what is spaCy and spaCyr

#Remember that spaCyr is a package where we can use the amazing functions of spaCy for analysis of text. 

## Using spaCyr for our TV show

#library(spacyr)
#
#spacy_install()
#
#spacy_initialize(model = "en_core_web_sm")

#We will not run this piece of chunk because it takes 5 minutes. 
#Here we are just installing from Python dependencies de package and the model.


load("data/df_spaCyr_himym.Rdata")

sp_parse_var <- full_join(sp_parse_doc, df_himym_final_doc, by = c("doc_id"))

sp_parse_var_PROPN <- sp_parse_var %>% filter(pos=="PROPN" & stringr::str_starts(entity, "PERSON_B"))

sp_parse_var_ADJ <- sp_parse_var %>% filter(pos=="ADJ")

## Get wordcloud using an spaCyr output




toks_himym_ADJ <- tokens(corp_himym, remove_punct = TRUE,
                         remove_separators = TRUE, 
                         remove_numbers = TRUE, 
                         remove_symbols = TRUE) %>% 
  tokens_keep(c(unique(sp_parse_var_ADJ$lemma))) %>%
  tokens_remove(c(stopwords("english"), "oh", "yeah", "okay", "like", 
                  "get", "got", "can", "one", "hey", "go",
                  "Ted", "Marshall", "Lily", "Robin", "Barney", "just", 
                  "know", "well", "right", "even", "see", 
                  "sure", "back", "first", "said", "maybe", "wedding", 
                  "whole", "wait"))

df_general_ADJ <- toks_himym_ADJ %>%
  tokens_group(groups = Season_w) %>%
  dfm() %>% dfm_subset(Season < 9)

textplot_wordcloud(df_general_ADJ, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   comparison = TRUE,
                   labelsize = 1.5,
                   min_count = 1, #Minimum frequency
                   color = ggsci::pal_lancet(palette = "lanonc"))
#color = RColorBrewer::brewer.pal(10, "Spectral"))

mtext("How I Met Your Mother",
      side=1, 
      line=1.3, 
      at=1, 
      adj=-.8, 
      cex=1.2)

# Network plot

## How the characters are related each other? 

token_characters_himym <- tokens(corp_himym, 
                                 remove_punct = TRUE,
                                 remove_separators = TRUE, 
                                 remove_numbers = TRUE, 
                                 remove_symbols = TRUE) %>% 
  tokens_keep(c(unique(df_characters_w$name))) %>% 
  tokens_tolower() %>% 
  fcm(context = "window", window = 5, tri = FALSE)

topfeats <- stringr::str_to_sentence(names(topfeatures(token_characters_himym, 70)))

topfeats <- names(topfeatures(token_characters_himym, 70))

set.seed(100)

textplot_network(fcm_select(token_characters_himym, topfeats),
                 edge_color = "#008eed", 
                 edge_size = 2, 
                 vertex_labelcolor = "#006fba", 
                 omit_isolated = TRUE,
                 min_freq = .1)


# Frequency of adjectives

freq_gen_dfm <- toks_himym_ADJ %>%
  dfm()

df_freq_gen_dfm <-  as.data.frame(textstat_frequency(freq_gen_dfm, n=10, groups = Season_w))



ggplot(df_freq_gen_dfm, aes(x = group, y = frequency, group = feature, color = feature)) +
  geom_line() +
  geom_point()+
  #coord_flip() +
  labs(x = NULL, y = "Frequency")

