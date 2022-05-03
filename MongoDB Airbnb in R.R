#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)
library(textreadr)
library(textshape)
library(pdftools)
library(rvest)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
library(janeaustenr)
library(gutenbergr)
library(tidyr)
library(scales)
library(tm)
library(Matrix)
library(textdata)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
library(widyr)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name

#connection_string <- 'mongodb+srv://<<user name>>:<<password>>@<<server name>>'
connection_string <- 'mongodb+srv://yuanning:LoVe91021@cluster0.uq6fu.mongodb.net/myFirstDatabase?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

is.na(airbnb_all)
which(is.na(airbnb_all))

#remove data missing data NA
airbnb_na<-na.omit(airbnb_all)


airbnb_dtm <- airbnb_all %>%
  unnest_tokens(word, summary) %>%
  count(name, word) %>%
  cast_dtm(name, word, n)

airbnb_dtm

  
#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":60}}')
View(mydf)

#2 writing an analytical query on the data::
mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')
View(mydf_analytical)


airbnb_all %>%
  count(review_scores$review_scores_ratin >= 90)


#### bigrams analysis ####
airbnb_bigram_high <- airbnb_all %>%
  filter(review_scores$review_scores_rating > 90 )%>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

airbnb_bigram_high

airbnb_bigram_high %>%
  count(bigram,sort=TRUE) 
#this has many stop words, need to remove them 

airbnb_bigrams_separated_high <- airbnb_bigram_high %>%
  separate(bigram, c("word1", "word2"), sep = " ")

airbnb_bigrams_filtered_high <- airbnb_bigrams_separated_high %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
  

#creating the new bigram, "no-stop-words":
airbnb_bigram_count_high <- airbnb_bigrams_filtered_high %>%
  count(word1, word2, sort = TRUE) %>%
  drop_na()
#want to see the new bigrams
airbnb_bigram_count_high


######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph_high <- airbnb_bigram_count_high %>%
  filter(n>30) %>%
  graph_from_data_frame()

bigram_graph_high

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_high, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



#### bigrams analysis ####
airbnb_bigram_low <- airbnb_all %>%
  filter(review_scores$review_scores_rating < 90 )%>%
  unnest_tokens(bigram, summary, token = "ngrams", n=2)

airbnb_bigram_low

airbnb_bigram_low %>%
  count(bigram,sort=TRUE) 
#this has many stop words, need to remove them 

airbnb_bigrams_separated_low <- airbnb_bigram_low %>%
  separate(bigram, c("word1", "word2"), sep = " ")

airbnb_bigrams_filtered_low <- airbnb_bigrams_separated_low %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


#creating the new bigram, "no-stop-words":
airbnb_bigram_count_low <- airbnb_bigrams_filtered_low %>%
  count(word1, word2, sort = TRUE) %>%
  drop_na()
#want to see the new bigrams
airbnb_bigram_count_low


######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph_low <- airbnb_bigram_count_low %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph_low

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_low, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



###tf-idf###
bigram_united_high <- airbnb_bigrams_filtered_high %>%
  unite(bigram, word1, word2, sep=" ") 
#we need to unite what we split in the previous section

bigram_tf_idf_high <- bigram_united_high %>%
  count(address$country, bigram) %>%
  bind_tf_idf(bigram, address$country , n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf_high

####
bigram_tf_idf_prot_high <- bigram_united_high %>%
  count(property_type, bigram) %>%
  bind_tf_idf(bigram, property_type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf_prot_high


###tf-idf###
bigram_united_low <- airbnb_bigrams_filtered_low %>%
  unite(bigram, word1, word2, sep=" ") 
#we need to unite what we split in the previous section

bigram_tf_idf_low <- bigram_united_low %>%
  count(address$country, bigram) %>%
  bind_tf_idf(bigram, address$country , n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf_low

####
bigram_tf_idf_prot_low <- bigram_united_low %>%
  count(property_type, bigram) %>%
  bind_tf_idf(bigram, property_type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf_prot_low



### Sentiment Analysis ###

########################################################
##### Comparing different sentiment libraries on Airbnb ####
########################################################


#overall rating larger than 90
airbnb_token <- airbnb_all %>%
  filter(review_scores$review_scores_rating >= 90 ) %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)


afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc") #feelings
bing <- get_sentiments("bing")

library(dplyr) #combined 3 lexicon into a dataframe called sentiments
sentiments <- bind_rows(mutate(afinn,lexicon ="afinn"),
                        mutate(nrc,lexicon ="nrc"),
                        mutate(bing,lexicon ="bing"))



afinn <- airbnb_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  airbnb_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  airbnb_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- airbnb_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



#by nrc

nrc_counts <- airbnb_token %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

nrc_counts


nrc_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()




#overall the review rating is lower than 90


#overall rating larger than 90
airbnb_token_1 <- airbnb_all %>%
  filter(review_scores$review_scores_rating < 90 ) %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)


afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc") #feelings
bing <- get_sentiments("bing")

library(dplyr) #combined 3 lexicon into a dataframe called sentiments
sentiments <- bind_rows(mutate(afinn,lexicon ="afinn"),
                        mutate(nrc,lexicon ="nrc"),
                        mutate(bing,lexicon ="bing"))


afinn <- airbnb_token_1 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  airbnb_token_1%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  airbnb_token_1 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts_1 <- airbnb_token_1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_1

bing_counts_1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



#by nrc

nrc_counts_1 <- airbnb_token_1 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

nrc_counts_1


nrc_counts_1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()




#export the dataframe to excel for visualization
#install.packages("writexl")
library(writexl)
my_airbnb <- cbind(airbnb_all$name,airbnb_all$review_scores,airbnb_all$summary,airbnb_all$space,airbnb_all$description,
                   airbnb_all$neighborhood_overview,airbnb_all$notes,airbnb_all$transit,airbnb_all$access,airbnb_all$interaction,
                   airbnb_all$house_rules,airbnb_all$property_type,airbnb_all$room_type,airbnb_all$bed_type,airbnb_all$minimum_nights,
                   airbnb_all$maximum_nights,airbnb_all$cancellation_policy,airbnb_all$accommodates,airbnb_all$bedrooms,
                   airbnb_all$beds,airbnb_all$number_of_reviews,airbnb_all$bathrooms,airbnb_all$price, airbnb_all$security_deposit,
                   airbnb_all$cleaning_fee,airbnb_all$extra_people,airbnb_all$guests_included)

write_xlsx(b,"C:/Users/ning9/OneDrive/Desktop/NLP/CSV/airbnb.df.xlsx")