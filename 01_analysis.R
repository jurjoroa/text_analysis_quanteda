library(tidyverse)
library(quanteda)
library(rvest)
library(quanteda.textstats)
library(quanteda.textplots)

#Define a url to get the table from Wikipedia. 
url_himym <- "https://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_episodes"

url_himym_characters <- "https://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_characters"
#Read the html file and identify the nodes that contain the table attribute. 
l_tables_himym <- url_himym %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)
l_tables_himym_characters <- url_himym_characters %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)

df_characters <- as.data.frame(l_tables_himym_characters[[1]]) %>% select(Character)

df_characters_w <- df_characters %>% filter(!stringr::str_starts(Character, "Futu"),
                                            !(Character %in% c("Character", "Main Characters", "Supporting Characters"))) %>% 
  mutate(name = str_extract(Character,"([^ ]+)"),
         name = replace(name, name=="Dr.", "Sonya"))




#This generates a list with all the tables that contain the page. In our case, we want the table from the second element till the 10th. 
l_tables_himym <- l_tables_himym[c(2:10)]

#Reduce the list in one data frame since all of the tables share the same structure 

df_himym <- data.frame(Reduce(bind_rows, l_tables_himym)) 



df_himym_filt <- df_himym %>% filter(str_length(No.overall)<4)

df_himym_filt_dupl <- df_himym %>% filter(str_length(No.overall)>4)


df_himym_filt_dupl_1 <- df_himym_filt_dupl %>% mutate(No.overall = as.numeric(replace(No.overall, str_length(No.overall)>4, substr(No.overall,1,3))),
                                  No..inseason = as.numeric(replace(No..inseason, str_length(No..inseason)>3, substr(No..inseason,1,2))),
                                  Prod.code = replace (Prod.code, str_length(Prod.code)>3, substr(Prod.code,1,6)))

df_himym_filt_dupl_2 <- df_himym_filt_dupl %>% mutate(No.overall = as.numeric(replace(No.overall, str_length(No.overall)>4, substr(No.overall,4,6))),
                                  No..inseason = as.numeric(replace(No..inseason, str_length(No..inseason)>3, substr(No..inseason,3,4))),
                                  Title = replace(Title, Title=="\"The Magician's Code\"", "\"The Magician's Code Part 2\""),
                                  Title = replace(Title, Title=="\"The Final Page\"", "\"The Final Page Part 2\""),
                                  Title = replace(Title, Title=="\"Last Forever\"" , "\"Last Forever Part 2\"" ),
                                  Prod.code = replace(Prod.code, str_length(Prod.code)>3, substr(Prod.code,7,12)))


df_himym_final <- bind_rows(df_himym_filt, 
                            df_himym_filt_dupl_1, 
                            df_himym_filt_dupl_2) %>% arrange(No.overall, No..inseason) %>% 
                  mutate(year = str_extract(Original.air.date, '[0-9]{4}+'),
                         Season = as.numeric(stringr::str_extract(Prod.code, "^.{1}"))) %>% 
  rename(Chapter = No..inseason)

df_himym_final$US.viewers.millions. <- as.numeric(str_replace_all(df_himym_final$US.viewers.millions., "\\[[0-9]+\\]", ""))



v_numbers <- as.numeric(stringr::str_extract(docname, "\\d+"))
v_numbers_last <- as.numeric(stringi::stri_extract_last_regex(docname, "[0-9]+"))

datalist_himym_w <- datalist_himym %>% mutate(Season = v_numbers,
                                              Chapter = v_numbers_last)

df_himym_final_doc <- full_join(datalist_himym_w, df_himym_final, by = c("Season", "Chapter")) %>% 
  mutate(Season_w = paste("Season", Season))


corp_himym <- corpus(df_himym_final_doc)  # build a new corpus from the texts

docnames(corp_himym) <- df_himym_final_doc$Title

summary(corp_himym)

corp_himym_s1 <- corpus_subset(corp_himym, Season == 1)


summary(corp_himym_s1)

df_himym_final_doc$Title[df_himym_final_doc$Season==1]


toks_himym <- tokens(corp_himym, remove_punct = TRUE,
               remove_separators = TRUE, 
               remove_numbers = TRUE, 
               remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))#Add additional words

toks_himym_dm <- toks_himym %>% dfm()

toks_himym_s1 <- tokens(corp_himym_s1, remove_punct = TRUE,
                     remove_separators = TRUE, 
                     remove_numbers = TRUE, 
                     remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))

toks_himym_dm_s1 <- toks_himym_s1 %>% dfm()



texts(corp_himym)[2]

textplot_wordcloud(toks_himym_dm, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   min_count = 1000, #Minimum frequency
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


topfeatures(toks_himym_dm, 20) 


#For remove words like I, me, my, myself, we, our. 
head(stopwords("english"), 20)


#Text Analysis

library(quanteda.textstats)


summary(corp_himym_s1)




#The difference comes from the maths behind and the general purpose of what distance and similarity are and how can they be measured.
#
#textstat_dist() returns distance scores compute with euclidean or manhattan methods, etc. Euclidean distance for example measures how distant two things are in Cartesian space.
#
#textstat_simil() returns similarity scores compute with cosine, correlation, jaccard, dice, etc. methods. These on the contrary tell you how two objects are similares.


corp_himym_s1 <- corpus_subset(corp_himym, Season == 1)

toks_himym_s1 <- tokens(corp_himym_s1, remove_punct = TRUE,
                        remove_separators = TRUE, 
                        remove_numbers = TRUE, 
                        remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))

toks_himym_dm_s1 <- toks_himym_s1 %>% dfm()


tstat_simil <- textstat_simil(toks_himym_dm_s1)

tstat_dist <- textstat_dist(toks_himym_dm_s1)

doc_similarity <- as.data.frame(as.matrix(tstat_simil))

doc_similarity$Title <- rownames(doc_similarity)

doc_similarity <- doc_similarity %>% relocate(Title) %>%                   # Using dplyr functions
  mutate_if(is.numeric,
            round,
            digits = 2)

rownames(doc_similarity) <- NULL


clust <- hclust(as.dist(tstat_simil))
dclust <- as.dendrogram(clust)

dclust <- reorder(dclust, 1:22)

nodePar <- list(lab.cex = 1.2, pch = c(NA, 19), 
                cex = 1.8, col = "#0080ff")

#Talk about different methods above the correlation 
par(mar=c(15,7,2,1))

plot(dclust,nodePar = nodePar,
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,
     main="How I Met Your Mother Season 1",
     type = "triangle",ylim=c(0,1),
     ylab = "Similarity between episodes (correlation %)",
     edgePar = list(col = 4:7, lwd = 5:5),
     panel.first = abline(h=c(seq(.10, 1, .10)), col="grey80"))

rect.hclust(clust, k = 5, border = "#ffb1b1")




tstat_dist <- textstat_dist(toks_himym_dm_s1)
clust_dist <- hclust(as.dist(tstat_dist))
dclust_dist <- as.dendrogram(clust_dist)

dclust_dist <- reorder(dclust_dist, 22:1)

nodePar_2 <- list(lab.cex = 1.2, pch = c(NA, 19), 
                cex = 1.8, col = "#ffc733")
#tstat_dist
par(mar=c(15,4,2,1))


plot(dclust_dist, nodePar = nodePar_2,
     main="How I Met Your Mother Season 1",
     type = "triangle",ylim=c(0,120),
     ylab = "Distance between episodes (correlation %)",
     edgePar = list(col = 18:19, lwd = 3:3),
     panel.first =abline(h=c(seq(10, 120, 10)), col="grey80"))
rect.hclust(clust_dist, k = 5, border = "#d80000")




corp_himym_s_ <- corpus_subset(corp_himym, Season == 5)

toks_himym_s_ <- tokens(corp_himym_s_, remove_punct = TRUE,
                        remove_separators = TRUE, 
                        remove_numbers = TRUE, 
                        remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))

toks_himym_dm_s_ <- toks_himym_s_ %>% dfm()


tstat_simil <- textstat_simil(toks_himym_dm_s_)

tstat_dist <- textstat_dist(toks_himym_dm_s_)

doc_similarity <- as.data.frame(as.matrix(tstat_simil))

doc_similarity$Title <- rownames(doc_similarity)

doc_similarity <- doc_similarity %>% relocate(Title) %>%                   # Using dplyr functions
  mutate_if(is.numeric,
            round,
            digits = 2)

rownames(doc_similarity) <- NULL


clust <- hclust(as.dist(tstat_simil))
dclust <- as.dendrogram(clust)

dclust <- reorder(dclust, 1:22)

nodePar <- list(lab.cex = 1.2, pch = c(NA, 19), 
                cex = 1.8, col = "#0080ff")

#Talk about different methods above the correlation 
par(mar=c(15,7,2,1))

plot(dclust,nodePar = nodePar,
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,
     main="How I Met Your Mother Season 1",
     type = "triangle",ylim=c(0,1),
     ylab = "Similarity between episodes (correlation %)",
     edgePar = list(col = 4:7, lwd = 5:5),
     panel.first = abline(h=c(seq(.10, 1, .10)), col="grey80"))

rect.hclust(clust, k = 5, border = "#ffb1b1")




tstat_dist <- textstat_dist(toks_himym_dm_s1)
clust_dist <- hclust(as.dist(tstat_dist))
dclust_dist <- as.dendrogram(clust_dist)

dclust_dist <- reorder(dclust_dist, 22:1)

nodePar_2 <- list(lab.cex = 1.2, pch = c(NA, 19), 
                  cex = 1.8, col = "#ffc733")
#tstat_dist
par(mar=c(15,4,2,1))


plot(dclust_dist, nodePar = nodePar_2,
     main="How I Met Your Mother Season 1",
     type = "triangle",ylim=c(0,120),
     ylab = "Distance between episodes (correlation %)",
     edgePar = list(col = 18:19, lwd = 3:3),
     panel.first =abline(h=c(seq(10, 120, 10)), col="grey80"))
rect.hclust(clust_dist, k = 5, border = "#d80000")

#For patterns
#mydict <- dictionary(list(all_terms = c("Ted", "Marshall", "Lily", "Robin", "Barney")))


df_actors <- toks_himym %>% tokens_select(c("Ted", "Marshall", "Lily", "Robin", "Barney", "Mother")) %>%
  tokens_group(groups = Season) %>% 
  dfm()

df_s1_actors <- toks_himym_s1 %>% tokens_select(c("Ted", "Marshall", "Lily", "Robin", "Barney", "Mother")) %>%
  tokens_group(groups = Season) %>% 
  dfm()

df_final_actors <-  as.data.frame(textstat_frequency(df_actors, groups = c(1:9)))

ggplot(df_final_actors, aes(x = group, y = frequency, group = feature, color = feature))+
  geom_line()+geom_point()+ylim(0,580)
  


library(spacyr)

spacy_install()

spacy_initialize()

spacy_initialize(model = "en_core_web_sm")
txt <- c(d1 = "spaCy is great at fast natural language processing.",
         d2 = "Mr. Smith spent two years in North Carolina.")


library(stringr)

sp_parse_doc <- spacy_parse(df_himym_final_doc, tag=TRUE)

sp_parse_var <- full_join(sp_parse_doc, df_himym_final_doc, by=c("doc_id"))

sp_parse_var_PROPN <- sp_parse_var %>% filter(pos=="PROPN" & stringr::str_starts(entity, "PERSON_B"))

sp_parse_var_ADJ <- sp_parse_var %>% filter(pos=="ADJ")

save(sp_parse_doc, file="data/df_spaCyr_himym.Rdata")


unique(sp_parse_var_PROPN$token)

unique(sp_parse_var$pos)

#spacyr

toks_himym_words <- tokens(corp_himym, remove_punct = TRUE,
                     remove_separators = TRUE, 
                     remove_numbers = TRUE, 
                     remove_symbols = TRUE) %>%
  tokens_remove(c(stopwords("english"), "oh", "yeah", "okay", "like", "get", "got", "can", "one", "hey", "go",
                  "Ted", "Marshall", "Lily", "Robin", "Barney", "just", "know"))#Add 

toks_himym_words_2 <- tokens(corp_himym, remove_punct = TRUE,
                           remove_separators = TRUE, 
                           remove_numbers = TRUE, 
                           remove_symbols = TRUE) %>% tokens_keep(c(unique(df_characters_w$name)))


df_general <- toks_himym_words_2 %>%
  dfm()

df_final_general <-  as.data.frame(textstat_frequency(df_general, n=10, groups = Season))

df_final_general

library(viridis)


textplot_wordcloud(df_general, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   min_count = 1, #Minimum frequency
                   color = RColorBrewer::brewer.pal(4, "Dark2"))

toks_himym_words_2_sec <- tokens(corp_himym, remove_punct = TRUE,
                             remove_separators = TRUE, 
                             remove_numbers = TRUE, 
                             remove_symbols = TRUE) %>% tokens_keep(c(unique(df_characters_w$name))) %>% 
  tokens_remove(c("Ted", "Barney", "Lily", "Robin", "Marshall"))


df_general_sec <- toks_himym_words_2_sec %>%
  #tokens_group(groups = Season) %>%
    dfm() #%>% dfm_subset(Season < 9)

textplot_wordcloud(df_general_sec, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   #comparison = TRUE,
                   labelsize = 1.5,
                   min_count = 1, #Minimum frequency
                   color = RColorBrewer::brewer.pal(8, "Spectral"))



sp_parse_var_ADJ



sp_parse_var_PROPN <- sp_parse_var %>% filter(pos=="PROPN" & stringr::str_starts(entity, "PERSON_B"))

sp_parse_var_ADJ <- sp_parse_var %>% filter(pos=="ADJ")


toks_himym_ADJ <- tokens(corp_himym, remove_punct = TRUE,
                             remove_separators = TRUE, 
                             remove_numbers = TRUE, 
                             remove_symbols = TRUE) %>% tokens_keep(c(unique(sp_parse_var_ADJ$lemma))) %>%
  tokens_remove(c(stopwords("english"), "oh", "yeah", "okay", "like", "get", "got", "can", "one", "hey", "go",
                  "Ted", "Marshall", "Lily", "Robin", "Barney", "just", "know", "well", "right", "even", "see", 
                  "sure", "back", "first", "said", "maybe", "wedding", "whole", "wait"))

df_general_ADJ <- toks_himym_ADJ %>%
  tokens_group(groups = Season_w) %>%
  dfm() %>% dfm_subset(Season < 9)

ggsci::pal_lancet(palette = "lanonc", )
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



toks_himym_words <- tokens(corp_himym, remove_punct = TRUE,
                           remove_separators = TRUE, 
                           remove_numbers = TRUE, 
                           remove_symbols = TRUE) %>%
  tokens_remove(c(stopwords("english"), "oh", "yeah", "okay", "like", "get", "got", "can", "one", "hey", "go",
                  "Ted", "Marshall", "Lily", "Robin", "Barney", "just", "know")) %>%
  tokens_tolower() %>% 
  fcm(context = "window", window = 5, tri = FALSE)

toks_2 <- tokens(corp_himym, remove_punct = TRUE,
                                           remove_separators = TRUE, 
                                           remove_numbers = TRUE, 
                                           remove_symbols = TRUE) %>% tokens_keep(c(unique(df_characters_w$name))) %>% 
  tokens_tolower() %>% 
  fcm(context = "window", window = 5, tri = FALSE)

topfeats <- stringr::str_to_sentence(names(topfeatures(toks_2, 70)))
topfeats <- names(topfeatures(toks_2, 50))


set.seed(100)
textplot_network(fcm_select(toks_2, topfeats),
                 edge_color = "#008eed", 
                 edge_size = 2, 
                 vertex_labelcolor = "#006fba", 
                 omit_isolated = TRUE,
                 min_freq = .1)

vertex_size_f <- fcm_select(toks_2, pattern = topfeats)

v_proportion <- rowSums(vertex_size_f)/min(rowSums(vertex_size_f))

x_p <- c("ted", "marshall", "lily", "barney", "robin")


final_v <- replace(v_proportion, names(v_proportion) %in% x_p, 
                   v_proportion[names(v_proportion) %in% x_p]/15)

options(ggrepel.max.overlaps = Inf)

textplot_network(fcm_select(toks_2, pattern = topfeats),
                 edge_color = "#008eed", 
                 edge_size = 8, 
                 vertex_labelcolor = "#006fba",
                 omit_isolated = TRUE,
                 #vertex_labelsize = final_v,
                 min_freq = .8)

toks_3 <- tokens(corp_himym, remove_punct = TRUE,
                 remove_separators = TRUE, 
                 remove_numbers = TRUE, 
                 remove_symbols = TRUE) %>%
  tokens_tolower() %>% 
  fcm(context = "document", count = "frequency")

topfeats <- stringr::str_to_sentence(names(topfeatures(toks_2, 70)))
topfeats <- names(topfeatures(toks_2, 70))

tstat_simil <- textstat_simil(toks_himym_dm_s1)
toks_himym_dm_s1 <- toks_himym_s1 %>% dfm()

topfeats <- names(topfeatures(toks_3, 70))

textplot_network(fcm_select(toks_3, topfeats),
                 edge_color = "#008eed", 
                 edge_size = 8,
                 vertex_labelcolor = "#006fba", 
                 omit_isolated = TRUE,
                 min_freq = .1)

freq_gen_dfm <- toks_himym_ADJ %>%
  dfm()

df_freq_gen_dfm <-  as.data.frame(textstat_frequency(freq_gen_dfm, n=10, groups = Season_w))

df_freq_gen_dfm

ggplot(df_freq_gen_dfm, aes(x = group, y = frequency, group = feature, color = feature)) +
  geom_line() +
  geom_point()+
  #coord_flip() +
  labs(x = NULL, y = "Frequency")









library(tidyr)
extract_numeric(docname)

df_tinder <- read_csv("data-raw/tinder_google_play_reviews.csv")


df_tinder_10k <- df_tinder[1:10000,]

corp_tinder <- corpus(df_tinder_10k$content)  # build a new corpus from the texts

summary(corp_tinder)


toks <- tokens(df_tinder_10k$content, remove_punct = TRUE,
                                  remove_separators = TRUE, 
                                remove_numbers = TRUE, 
                                remove_symbols = TRUE) %>%
  tokens_remove(stopwords("english"))



toks2 <- tokens_wordstem(toks) %>%
  tokens_remove(stopwords("english")) %>% #quitar separadores
  types()
toks2


corp <- corpus(c(df_tinder_10k$content))

corp_dm <- tokens(corp) %>% dfm()

toks_dm <- tokens(toks) %>% dfm()

summary(corp)



library(quanteda.textplots)


textplot_wordcloud(toks_dm, random_order = FALSE, rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))



