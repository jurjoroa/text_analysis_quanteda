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

library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stringr)
library(spacyr)
library(ggsci)
library(ggrepel)
library(RColorBrewer)
library(cowplot)
library(magick)
library(gghighlight)


obj_img <- image_read(path = "https://bit.ly/3twmH2Y")

## 01.01- Load data- -----------------------------------------------------------

#If you want to know how we generated this data, go to the script 02_web_scrap please
load("data/df_himym_final_doc.Rdata")
load("data/df_characters_w.Rdata")


# 01.- Quanteda analysis -------------------------------------------------------

# 02.- First step: Define a corpus ---------------------------------------------

#df_himym_final_doc

corp_himym <- corpus(df_himym_final_doc)  #Build a new corpus from the texts

docnames(corp_himym) <- df_himym_final_doc$Title

summary(corp_himym, n = 15)


# 03.- Second step: Convert corpus into tokens and wrangle it ------------------

corp_himym_stat <- corp_himym

docnames(corp_himym_stat) <- df_himym_final_doc$Title_season


corp_himym_s1_simil <- corpus_subset(corp_himym_stat, Season == 1) #We want to analyze just the first season



toks_himym_s1 <- tokens(corp_himym_s1_simil, #corpus from all the episodes from the first season
                        remove_punct = TRUE, #Remove punctuation of our texts
                        remove_separators = TRUE, #Remove separators of our texts
                        remove_numbers = TRUE, #Remove numbers of our texts
                        remove_symbols = TRUE) %>% #Remove symbols of our texts
  tokens_remove(stopwords("english")) #Remove stop words of our texts

# 03.- Third step: Convert our tokens into a Document Feature Matrix -----------

toks_himym_dm_s1 <- toks_himym_s1 %>% 
                    dfm() #Convert our tokens into a document feature matrix

# 04.- Now we can do some analysis --------------------------------------------

# 05.- Similarity between episodes --------------------------------------------

## 05.01.- textstat_simil function- --------------------------------------------

tstat_simil <- textstat_simil(toks_himym_dm_s1) #Check similarity between episodes of the first season

clust <- hclust(as.dist(tstat_simil)) #Convert our object into a cluster (For visualization purposes)

dclust <- as.dendrogram(clust)  #Convert our cluster into a dendogram (For visualization purposes)

dclust <- reorder(dclust, 1:22) #Order our visualization

#Seetle colors
nodePar <- list(lab.cex = 1, pch = c(NA, 19), 
                cex.axis = 1.5,
                cex = 2, col = "#0080ff")


## 05.02.- Plot Similarity between episodes--------------------------------------------


png("images/similarity.png", width = 300, height = 300, units='mm', res = 500)

#Talk about different methods above the correlation 
par(mar = c(18.1, 6, 2, 3))

#Plot dendogram
plot(dclust, nodePar = nodePar,
     las = 1,
     cex.axis = 2, cex.main = 2, cex.sub = 2,
     main = "How I Met Your Mother Season 1",
     type = "triangle",
     ylim = c(0,1),
     ylab = "",
     edgePar = list(col = 4:7, lwd = 7:7),
     panel.first = abline(h = c(seq(.10, 1, .10)), col = "grey80"))

title(ylab = "Similarity between episodes (correlation %)", mgp = c(4, 1, 1), cex.lab = 2)    

rect.hclust(clust, k = 5, border = "red")

dev.off()


# 06.- Distance between episodes (by correlation) ------------------------------

## 06.01.- textstat_dist function- ---------------------------------------------

tstat_dist <- textstat_dist(toks_himym_dm_s1) #Check distance between episodes of the first season
clust_dist <- hclust(as.dist(tstat_dist)) #Convert our object into a cluster (For visualization purposes)
dclust_dist <- as.dendrogram(clust_dist) #Convert our cluster into a dendrogram (For visualization purposes)

dclust_dist <- reorder(dclust_dist, 22:1) #Order our visualization

nodePar_2 <- list(lab.cex = 1.2, pch = c(NA, 19), 
                  cex = 1.8, col = 11)

## 06.02.- Plot Distance between episodes (by correlation)----------------------


#png("images/distance.png", width = 300, height = 300, units='mm', res = 500)

par(mar = c(21, 6, 2, 3))

plot(dclust_dist, nodePar = nodePar_2,
     cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 2,
     main = "How I Met Your Mother Season 1",
     type = "triangle", ylim = c(0, 120),
     ylab = "",
     edgePar = list(col = 11:19, lwd = 7:7),
     panel.first = abline(h = c(seq(10, 120, 10)), col = "grey80"))

title(ylab = "Distance between episodes (correlation %)", mgp = c(4, 1, 1), cex.lab = 2)    

rect.hclust(clust_dist, k = 5, border = "red")


#dev.off()

# 07.- Appearances of actors by season------------------------------------------

## 07.01.- Characters by season--------------------------------------------------

#Remember our second step: tokenize our corpus. 

toks_himym <- tokens(corp_himym, #corpus from all the episodes from the first season
                     remove_punct = TRUE, #Remove punctuation of our texts
                     remove_separators = TRUE,  #Remove separators of our texts
                     remove_numbers = TRUE, #Remove numbers of our texts
                     remove_symbols = TRUE) %>% #Remove symbols of our texts
  tokens_remove(stopwords("english")) #Add additional words

#Remember our third step: DFM object

dfm_actors <- toks_himym %>% 
  tokens_select(c("Ted", "Marshall", "Lily", "Robin", "Barney", "Mother")) %>% #We just want to analyze these characters
  tokens_group(groups = Season) %>% #We group our tokens (scripts) by season
  dfm() #Transform the token into a DFM object

## 07.02.- textstat_frequency function------------------------------------------

df_final_actors <-  as.data.frame(textstat_frequency(dfm_actors, groups = c(1:9))) %>% 
                    mutate(Season = paste("Season", group),
                           `Principal Characters` = replace(feature, is.character(feature), str_to_title(feature))) %>% 
                    select(-feature)

## 07.03.- Plot frequency of actors--------------------------------------------

ggplot1 <- ggplot(df_final_actors, aes(x = group, y = frequency, group = `Principal Characters`, color = `Principal Characters`)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
  geom_point(size = 3.2) +
  scale_y_continuous(breaks = seq(0, 5600, by = 50), limits = c(0,560))+
  theme_minimal(base_size = 14) +
  labs(x = "Number of Season",
       y = "Frequencies of appreances",
       title = "Appearances of principal characters by Season",
       caption="Description: This plot show the number of times that the \n principal characters appears in HIMYM per season.")+
       theme(panel.grid.major=element_line(colour="#cfe7f3"),
             panel.grid.minor=element_line(colour="#cfe7f3"),
             plot.title = element_text(margin = margin(t = 10, r = 20, b = 30, l = 30)),
             #axis.text.x=element_text(size=15),
             #axis.text.y=element_text(size=15),
             plot.caption=element_text(size=12, hjust=.1, color="#939393"),
             legend.position="bottom",
             plot.margin = margin(t = 20,  # Top margin
                                  r = 50,  # Right margin
                                  b = 40,  # Bottom margin
                                  l = 10), # Left margin
             text=element_text(family="sans")) + 
#geom_segment(aes(x = 8.5, y = 75, xend = 8.8, yend = 70),
#             arrow = arrow(length = unit(0.1, "cm")))+
  guides(colour = guide_legend(ncol = 6))

ggdraw(ggplot1) + draw_image(obj_img, x = .97, y = .97, 
                               hjust = 1.1, vjust = .7, 
                               width = 0.11, height = 0.1)


# 08.- Wordcloud of PRINCIPAL characters that appears in HIMYM------------------

## 08.01.- Wordcloud steps------------------------------------------------------

### 08.01.01.- Second step: Tokens----------------------------------------------

toks_himym_characters <- tokens(corp_himym, #corpus from all the episodes from all season
                                remove_punct = TRUE, #Remove punctuation of our texts
                                remove_separators = TRUE, #Remove separators of our texts
                                remove_numbers = TRUE, #Remove numbers of our texts
                                remove_symbols = TRUE) %>% #Remove symbols of our texts
  tokens_keep(c(unique(df_characters_w$name))) #This function allow us to keep just the tokens that we want. 

#In this case, we just want the characters.

### 08.01.02.- Third step: DFM object-------------------------------------------

dfm_general_characters <- toks_himym_characters %>%
                          dfm()

## 08.02.- Generate Wordcloud --------------------------------------------------

#png("images/wordcloud_princ.png", width = 50, height = 50, units='mm', res = 500)

textplot_wordcloud(dfm_general_characters, 
                   rotation = 0.25,
                   min_size = 1.4, max_size = 8,
                   min_count = 1, #Minimum frequency
                   color = brewer.pal(11, "RdBu"))
#RColorBrewer::display.brewer.all()

#dev.off()

# 09.- Wordcloud of SECONDARY characters that appears in HIMYM------------------

## 09.01.- Wordcloud steps------------------------------------------------------

### 09.01.01.- Second step: Tokens----------------------------------------------

toks_himym_sec_characters <- tokens(corp_himym, #corpus from all the episodes from all season
                                    remove_punct = TRUE, #Remove punctuation of our texts
                                    remove_separators = TRUE, #Remove separators of our texts
                                    remove_numbers = TRUE, #Remove numbers of our texts
                                    remove_symbols = TRUE) %>% #Remove symbols of our texts
  tokens_keep(c(unique(df_characters_w$name))) %>% #We want to keep all the characters
  tokens_remove(c("Ted", "Barney", "Lily", "Robin", "Marshall")) #But we remove the principal characters

### 09.01.02.- Third step: DFM object-------------------------------------------

dfm_general_sec_characters <- toks_himym_sec_characters %>%
                              dfm()

## 09.02.- Generate Wordcloud --------------------------------------------------


#png("images/wordcloud_sec.png", width = 50, height = 50, units='mm', res = 500)


textplot_wordcloud(dfm_general_sec_characters, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   min_size = 1, max_size =5,
                   labelsize = 1.5,
                   min_count = 1, #Minimum frequency
                   color = RColorBrewer::brewer.pal(8, "Spectral"))

#dev.off()

# 10.- spaCy and spaCyr ------------------

#Explain what is spaCy and spaCyr

#Remember that spaCyr is a package where we can use the amazing functions of spaCy for analysis of text. 

## Using spaCyr for our TV show

#library(spacyr)
#
#spacy_install()
#
#spacy_initialize(model = "en_core_web_sm")

#sp_parse_doc <- spacy_parse(df_himym_final_doc, tag=TRUE)


#We will not run this piece of chunk because it takes 5 minutes. 
#Here we are just installing from Python dependencies the package and the model.

## 10.01.- Load data------------------------------------------------------------

load("data/df_spaCyr_himym.Rdata")


## 10.02.- Review structure-----------------------------------------------------

#Look how the spacyr package separate our sentences into words and classified it with 
#Verbs, prepositions, Adverbs, Adjectives, etc. 


head(sp_parse_doc)

## 10.03.- Filter data by type of word------------------------------------------

sp_parse_var <- full_join(sp_parse_doc, df_himym_final_doc, by = c("doc_id"))

#In this case, we will just look the proper names and adjectives.

sp_parse_var_PROPN <- sp_parse_var %>% filter(pos=="PROPN" & stringr::str_starts(entity, "PERSON_B"))

sp_parse_var_ADJ <- sp_parse_var %>% filter(pos=="ADJ")


## 10.04.- Get wordcloud using an spaCyr output---------------------------------

### 10.04.01.- Second step: Tokens----------------------------------------------

toks_himym_ADJ <- tokens(corp_himym, #corpus from all the episodes from all season
                         remove_punct = TRUE, #Remove punctuation of our texts
                         remove_separators = TRUE,  #Remove separators of our texts
                         remove_numbers = TRUE, #Remove numbers of our texts
                         remove_symbols = TRUE) %>%  #Remove symbols of our texts
  tokens_keep(c(unique(sp_parse_var_ADJ$lemma))) %>% #We want to keep all the adjective
  tokens_remove(c(stopwords("english"), "oh", "yeah", "okay", "like", 
                  "get", "got", "can", "one", "hey", "go",
                  "Ted", "Marshall", "Lily", "Robin", "Barney", "just", 
                  "know", "well", "right", "even", "see", 
                  "sure", "back", "first", "said", "maybe", "wedding", 
                  "whole", "wait")) #But we remove stopwords and other words that the package didn't classify it correctly. 

### 10.04.02.- Third step: Tokens----------------------------------------------

df_general_ADJ <- toks_himym_ADJ %>%
  tokens_group(groups = Season_w) %>% #group by season
  dfm() %>% dfm_subset(Season < 9)

### 10.04.03.- Wordcloud of adjectives -----------------------------------------

#Because of a function limitation, the maximum comparison that we can do is 8 groups

#png("images/wordcloud_adj.png", width = 200, height = 200, units='mm', res = 500)


textplot_wordcloud(df_general_ADJ, 
                   random_order = FALSE, 
                   rotation = 0.25,
                   comparison = TRUE,
                   labelsize = 1.5, 
                   min_count = 1, #Minimum frequency
                   color = ggsci::pal_lancet(palette = "lanonc"))

#dev.off()

## 10.05.- Get frequency of adjectives------------------------------------------

### 10.05.01.- Remember out third step: DFM object------------------------------

freq_gen_dfm <- toks_himym_ADJ %>%
  dfm()

#Generate dataframe
df_freq_gen_dfm <-  as.data.frame(textstat_frequency(freq_gen_dfm, # Our DFM object
                                                     n = 10, #Number of observations displayed
                                                     groups = Season)) #Grouped by season
                                  
df_freq_gen_dfm_match <- df_freq_gen_dfm %>% mutate(total = 1) %>% 
                                  group_by(feature) %>% 
                                  summarise(total = sum(total)) %>% 
                                  filter(total== 9)

df_freq_gen_dfm_final <- right_join(df_freq_gen_dfm, df_freq_gen_dfm_match,
                                   by = "feature") %>% rename(Word = feature) %>% 
                                   mutate(Word = str_to_title(Word))

### 10.05.02.- Plot frequency of adjectives-------------------------------------

ggplot2 <- ggplot(df_freq_gen_dfm_final, aes(x = group, y = frequency, group = Word, color = Word)) +
  geom_line(size = 1.5, show.legend = TRUE) +
  scale_color_manual(values = rev(brewer.pal(n = 7, name = "Dark2"))) +
  geom_point(size = 3.2) +
  theme_minimal(base_size = 14) +
  labs(x = "Number of Season",
       y = "Frequencies of words",
       title = "Frequency of adjectives",
       caption="Description: This plot shows the top adjectives that appears in every season of HIMYM")+
  theme(panel.grid.major=element_line(colour="#cfe7f3"),
        panel.grid.minor=element_line(colour="#cfe7f3"),
        plot.title = element_text(margin = margin(t = 10, r = 20, b = 30, l = 30)),
        #axis.text.x=element_text(size=15),
        #axis.text.y=element_text(size=15),
        plot.caption=element_text(size=12, hjust=.1, color="#939393"),
        legend.position="bottom",
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 40,  # Bottom margin
                             l = 10), # Left margin
        text=element_text()) + 
  #geom_segment(aes(x = 8.5, y = 75, xend = 8.8, yend = 70),
  #             arrow = arrow(length = unit(0.1, "cm")))+
  guides(colour = guide_legend(ncol = 4)) +
  gghighlight(max(frequency) > 140,
              keep_scales = TRUE,
              unhighlighted_params = list(colour = NULL, alpha = 0.2))
  

ggdraw(ggplot2) + draw_image(obj_img, x = .97, y = .97, 
                             hjust = 1.1, vjust = .7, 
                             width = 0.11, height = 0.1)

# 11.- Network plot ------------------

# How the characters are related each other? 

## 11.01.- Network steps------------------------------------------------------

### 11.01.01.- Second step: Tokens----------------------------------------------


token_characters_himym <- tokens(corp_himym, #corpus from all the episodes from all season
                                 remove_punct = TRUE, #Remove punctuation of our texts
                                 remove_separators = TRUE, #Remove separators of our texts
                                 remove_numbers = TRUE, #Remove numbers of our texts
                                 remove_symbols = TRUE) %>%  #Remove symbols of our texts
  tokens_keep(c(unique(df_characters_w$name))) %>% #We want to keep all the characters
  tokens_tolower() #We want lower cases in our tokens



### 11.01.02.- Extra step: create a feature co-ocurrence matrix (FCM)------------

fcm_characters_himym <- token_characters_himym %>%
                        fcm(context = "window", window = 5, tri = FALSE)

## 11.02.- Network plot of all characters----------------------------------------

#Vector with all the characters
v_top_characters <- stringr::str_to_sentence(names(topfeatures(fcm_characters_himym, 70)))

set.seed(100)

textplot_network(fcm_select(fcm_characters_himym, v_top_characters),
                 edge_color = "#008eed", 
                 edge_size = 2, 
                 vertex_labelcolor = "#006fba", 
                 omit_isolated = TRUE,
                 min_freq = .1)


## 11.03.- Network plot with 30 principal characters----------------------------

#Vector with 30 characters
v_top_characters_2 <- stringr::str_to_sentence(names(topfeatures(fcm_characters_himym, 30)))

textplot_network(fcm_select(fcm_characters_himym, v_top_characters_2),
                 edge_color = "#008eed", 
                 edge_size = 5, 
                 vertex_labelcolor = "#006fba",
                 omit_isolated = TRUE,
                 min_freq = .1)


## 11.03.- Network plot of Ted -------------------------------------------------

fcm_characters_himym_ted <- token_characters_himym %>%
  tokens_remove(c("marshall", "lily", "barney", "robin")) %>% #Here we just want ted, that why we remove the other principal characters
  fcm(context = "window", window = 5, tri = FALSE)

#Vector with 30 characters
v_top_characters_3 <- stringr::str_to_sentence(names(topfeatures(fcm_characters_himym_ted, 30)))

#Create a FCM matrix with our characters
vertex_size_f <- fcm_select(fcm_characters_himym_ted, pattern = v_top_characters_3)

#Create a proportion 
v_proportion <- rowSums(vertex_size_f)/min(rowSums(vertex_size_f))

#Vector of Ted
x_p <- c("ted")

#Replace that proportion in our vector
final_v <- replace(v_proportion, names(v_proportion) %in% x_p, 
                   v_proportion[names(v_proportion) %in% x_p]/15)

textplot_network(fcm_select(fcm_characters_himym_ted, v_top_characters_3),
                 edge_color = "#008eed", 
                 edge_size = 5, 
                 vertex_labelcolor = "#006fba",
                 omit_isolated = TRUE,
                 vertex_labelsize = final_v,
                 min_freq = .1)


# 12.- Text stat collocation ---------------------------------------------------

#Identify and score multi-word expressions, or adjacent fixed-length collocations, from text.
#textstat_collocations()

### 12.01.01.- Second step: Tokens----------------------------------------------

toks_himym_s1 <- tokens(corp_himym_s1_simil, #Define our corpus for the first season
                        padding = TRUE) %>% #Leave an empty string where the removed tokens previously existed
  tokens_remove(stopwords("english")) #Remove stopwords of our token


## 12.02.- textstat_collocations function --------------------------------------

himym_s1_collocations <-textstat_collocations(toks_himym_s1, #Our token object
                                              tolower = F) #Keep capital letters


df_himym_s1_coll <- data.frame(himym_s1_collocations) %>% 
                        rename(`Total of collocations` = count)

## 12.02.- Plot allocations --------------------------------------

ggplot3 <- ggplot(df_himym_s1_coll, aes(x = z, y = lambda, label = collocation)) +
  geom_point(alpha = 0.2, aes(size = `Total of collocations`), color = "#00578a")+
  geom_point(data = df_himym_s1_coll %>% filter(z > 15), 
             aes(x = z, y = lambda, size = `Total of collocations`),
             color = '#00578a') + 
  geom_text_repel(data = df_himym_s1_coll %>% filter(z > 15), #Function from ggrepel package. Show scatterplots with text.
                  aes(label = collocation, size = count), size = 3,
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.3, "lines")) + 
  scale_y_continuous(breaks = seq(0, 16, by = 1), limits = c(0,16))+
  theme_minimal(base_size = 14) +
  labs(x = "Z statistic",
       y = "Lambda",
       title = "Allocations of words in the first season",
       caption = "Description: This plot identifies and scores multi-word expressions of the 1st season")+
  theme(panel.grid.major = element_line(colour = "#cfe7f3"),
        panel.grid.minor = element_line(colour = "#cfe7f3"),
        plot.title = element_text(margin = margin(t = 10, r = 20, b = 30, l = 30)),
        #axis.text.x=element_text(size=15),
        #axis.text.y=element_text(size=15),
        plot.caption = element_text(size=12, hjust=.1, color="#939393"),
        legend.position="bottom",
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))

ggdraw(ggplot3) + draw_image(obj_img, x = .97, y = .97, 
                             hjust = 1.1, vjust = .7, 
                             width = 0.11, height = 0.1)


#lambda collocation scoring metric
#array data is simply the number of times a given value appears



# 13.- Locate keywords-in-context ----------------------------------------------


## 13.01.- Set dataframe to merge with other information--------------------------

df_title_s_chp <- df_himym_final_doc %>% 
                  select(Title, Season, Chapter, No.overall, 
                         Season_w, US.viewers.millions.)

### 13.02.01.- First step: Define a corpus --------------------------------------

corp_himym <- corpus(df_himym_final_doc)  # build a new corpus from the texts

docnames(corp_himym) <- df_himym_final_doc$Title #Rename docnames with Title of the episode

corp_himym_s5 <- corpus_subset(corp_himym, #our corpus
                               Season == 5) #Filter by season


### 13.02.02.- Second step: Define a token --------------------------------------

toks_himym_s5 <- tokens(corp_himym_s5, #Corpus of season 5
                        padding = TRUE)

## 13.03- kwic function---------------------------------------------------------


kw_himym_s5_love <- kwic(toks_himym_s5, #token object.
                         pattern = "love*", #pattern that we want to look for.
                         window = 10) #how many words you want before and after your pattern.

### 13.03.01- Wrangle dataframe of kwic output----------------------------------


df_kw_himym_s5_love <- as.data.frame(kw_himym_s5_love)  %>% 
  rename(Title = docname,`Pre Sentence` = pre, `Post Sentence` = post)%>% 
  rename_with(str_to_title, .cols = everything()) %>%  left_join(df_title_s_chp, 
                                                                 by ="Title") %>% 
  relocate(Title, Season, Chapter)

df_kw_himym_s5_love

### 13.04.01.- Second step: Define a token --------------------------------------

toks_himym <- tokens(corp_himym,  #Define our corpus for all seasons
                     padding = TRUE) #Leave an empty string where the removed tokens previously existed

kw_himym_legendary <- kwic(toks_himym, #token object.
                           pattern = "legendary*",  #pattern that we want to look for.
                           window = 10) #how many words you want before and after your pattern.

### 13.04.02.- Wrangle dataframe of kwic output----------------------------------

df_kw_himym_legendary <- as.data.frame(kw_himym_legendary)  %>% 
  rename(Title = docname,`Pre Sentence` = pre, `Post Sentence` = post)%>% 
  rename_with(str_to_title, .cols = everything()) %>%  left_join(df_title_s_chp, 
                                                                 by = "Title") %>% 
  relocate(Title, Season, Chapter)

df_kw_himym_legendary


### 13.05.01.- Second step: Define a token --------------------------------------


kw_himym_wait_for <- kwic(toks_himym, #token object.
                          pattern = phrase("wait for it"),  #Here we can specify even a phrase
                          window = 10) #how many words you want before and after your pattern.

### 13.05.02.- Wrangle dataframe of kwic output----------------------------------

df_kw_himym_wait_for <- as.data.frame(kw_himym_wait_for)  %>% 
  rename(Title = docname,`Pre Sentence` = pre, `Post Sentence` = post)%>% 
  rename_with(str_to_title, .cols = everything()) %>%  left_join(df_title_s_chp, 
                                                                 by = "Title") %>% 
  relocate(Title, Season, Chapter)


df_kw_himym_wait_for



# 14.- Sentiment analyis --------------------------------------------------------


## 14.01.- Second step: Define a token --------------------------------------

toks_himym <- tokens(corp_himym, #Our corpus object
                     remove_punct = TRUE, #Remove punctuation in our texts
                     remove_separators = TRUE, #Remove separators in our texts
                     remove_numbers = TRUE, #Remove numbers in our texts
                     remove_symbols = TRUE) %>% #Remove symbols in our texts
  tokens_remove(stopwords("english"))#Add additional words

#tidy_sou <- df_himym_final_doc %>%
#  unnest_tokens(word, text) This is another way on spacyr

## 14.02- Get positive and negative words --------------------------------------

df_positive_words <- get_sentiments("bing") %>% #We have four options: "bing", "afinn", "loughran", "nrc" 
  filter(sentiment == "positive")

df_negative_words <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

## 14.03.- Define a dictionary with positive and negative words from bing --------------------------------------

l_sentiment_dictionary <- dictionary(list(positive = df_positive_words, 
                                        negative = df_negative_words))

#dfm_sentiment_himym <- dfm(toks_himym) %>% dfm_lookup(dictionary = sentiment_dictionary)


## 14.04.- Load a file --------------------------------------
#It is a DFM object, which comes from a token off all the season of HIMYM

load(file = "data/dfm_sentiment_himym.Rdata")

#Rename doc:id with the Titles of every episode
docnames(dfm_sentiment_himym) <- df_himym_final_doc$Title


## 14.05.- Wrangle dataframe --------------------------------------

#Format in long to plot positive and negative words
df_sentiment_himym <- convert(dfm_sentiment_himym, "data.frame") %>% 
  gather(positive.word, negative.word, key = "Polarity", value = "Words") %>% 
  rename(Title = doc_id) %>% 
  mutate(Title = as_factor(Title)) %>% 
  left_join(df_title_s_chp, by ="Title") %>%
  mutate(Polarity = replace(Polarity, is.character(Polarity), 
                            str_replace_all(Polarity, 
                                            pattern = "negative.word",
                                            replacement = "Negative words")),
         Polarity = replace(Polarity, is.character(Polarity), 
                            str_replace_all(Polarity, 
                                            pattern = "positive.word",
                                            replacement = "Positive words")))

## 14.06.- Plot total of positive and negative words per season and episode -----

ggplot4 <- ggplot(df_sentiment_himym, aes(x = Chapter, y = Words, fill = Polarity, group = Polarity)) + 
  geom_bar(stat = 'identity', position = position_dodge(), size = 1) + 
  facet_wrap(~ Season_w)+
  scale_fill_manual(values = c("#c6006f", "#004383")) + 
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  theme_minimal(base_size = 14) +
  labs(x = "Episodes",
       y = "Frequency of words",
       title = "Total of positve and negative words per season",
       caption="Description: This plot identifies total of positive and negative words \n per season and episode")+
  theme(panel.grid.major = element_line(colour="#cfe7f3"),
        panel.grid.minor = element_line(colour="#cfe7f3"),
        plot.title = element_text(margin = margin(t = 10, r = 20, b = 30, l = 30)),
        #axis.text.x=element_text(size=15),
        #axis.text.y=element_text(size=15),
        plot.caption = element_text(size = 12, hjust = .1, color = "#939393"),
        legend.position = "bottom",
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))

ggdraw(ggplot4) + draw_image(obj_img, x = .97, y = .97, 
                             hjust = 1.1, vjust = .7, 
                             width = 0.11, height = 0.1)

## 14.07.- Weight the feature frequencies in a dfm -----------------------------

#dfm_weight()

#This step is the same as the last one, but here we are taking into account the weights to do a fair comparison


dfm_sentiment_himym_prop <- dfm_weight(dfm_sentiment_himym, scheme = "prop")
dfm_sentiment_himym_prop

### 14.07.01- Wrangle dfm weight dataframe--------------------------------------

df_sentiment_himym_prop <- convert(dfm_sentiment_himym_prop, "data.frame") %>% 
  gather(positive.word, negative.word, key = "Polarity", value = "Words") %>% 
  rename(Title = doc_id) %>% 
  mutate(Title = as_factor(Title)) %>% 
  left_join(df_title_s_chp, by = "Title") %>%
  mutate(Polarity = replace(Polarity, is.character(Polarity), 
                            str_replace_all(Polarity, 
                                            pattern = "negative.word",
                                            replacement = "Negative words")),
         Polarity = replace(Polarity, is.character(Polarity), 
                            str_replace_all(Polarity, 
                                            pattern = "positive.word",
                                            replacement = "Positive words")))

### 14.07.02.- Plot total of positive and negative words per season and episode -----

#This step is the same as the last one, but here we are taking into account the weights to do a fair comparison

ggplot5 <- ggplot(df_sentiment_himym_prop, aes(x = Chapter, y = Words, fill = Polarity, group = Polarity)) + 
  geom_bar(stat = 'identity', position = position_dodge(), size = 1) + 
  facet_wrap(~ Season_w) +
  scale_fill_manual(values = c("#c6006f", "#004383")) + 
  scale_y_continuous(breaks = seq(0, .8, by = .2))+
  theme_minimal(base_size = 14) +
  labs(x = "Episodes",
       y = "Frequency of words",
       title = "Weighted positve and negative words per season",
       caption = "Description: This plot identifies the weighted total of positive and negative words \n per season and episode")+
  theme(panel.grid.major = element_line(colour = "#cfe7f3"),
        panel.grid.minor = element_line(colour = "#cfe7f3"),
        plot.title = element_text(margin = margin(t = 10, r = 20, b = 30, l = 30)),
        #axis.text.x=element_text(size=15),
        #axis.text.y=element_text(size=15),
        plot.caption = element_text(size = 12, hjust = .1, color = "#939393"),
        legend.position = "bottom",
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))

ggdraw(ggplot5) + draw_image(obj_img, x = .97, y = .97, 
                             hjust = 1.1, vjust = .7, 
                             width = 0.11, height = 0.1)




## 14.08.- Wrangle dfm weight dataframe with measures---------------------------

#Scaling Policy Preferences from Coded Political Texts
#WILL LOWE, KENNETH BENOIT, SLAVA MIKHAYLOV, MICHAEL LAVER
#Balance between positive words/negative words using a log scale 

#Here we 
df_sentiment_himym_prop_measure <- convert(dfm_sentiment_himym_prop, "data.frame") %>% 
  rename(Sentiment = positive.word)  %>% rename(Title = doc_id) %>% 
  left_join(df_title_s_chp, by = "Title")  %>%
  mutate(measure = log((Sentiment + 0.5)/(negative.word + .5))) %>%
  select(-Season) %>% 
  rename(Season = Season_w)


## 14.09.- Plot measure of positivity among season------------------------------


ggplot6 <- ggplot(df_sentiment_himym_prop_measure, aes(x = No.overall, y = measure, 
                                            color = Season, group = Season)) +
  scale_color_manual(values = brewer.pal(n = 9, name = "Set1"))+
  geom_line(size = 1.5) +
  geom_point(size = 3.2) + 
  scale_x_continuous(breaks = seq(0, 208, by = 20))+
  theme_minimal(base_size = 14) +
  labs(x = "Number of episode",
       y = "Rate",
       title = "Measure of positivity among episodes",
       caption="Description: This plot shows the positivity rate of every episode")+
  theme(panel.grid.major = element_line(colour = "#cfe7f3"),
        panel.grid.minor = element_line(colour = "#cfe7f3"),
        plot.title = element_text(margin = margin(t = 10, r = 20, b = 30, l = 30)),
        plot.caption = element_text(size=12, hjust = .1, color = "#939393"),
        legend.position = "bottom",
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 40,  # Bottom margin
                             l = 10), # Left margin
        text = element_text()) + 
  guides(colour = guide_legend(ncol = 3)) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "red", size = 1)


ggdraw(ggplot6) + draw_image(obj_img, x = .97, y = .97, 
                             hjust = 1.1, vjust = .7, 
                             width = 0.11, height = 0.1)


