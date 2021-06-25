#Installing the necessary packages for our purposes#
install.packages("rtweet")
library(rtweet)
install.packages("tidyverse")
library(tidyverse)
install.packages(c("tidytext", "stringr", "tm", "wordcloud", "reshape2", "wordcloud2", "lubridate"))
library(tidytext)
library(stringr)
library(tm)
library(wordcloud)
library(reshape2)
library(wordcloud2)
library(lubridate)
install.packages("rtweet")
library(rtweet)

# i deleted my personal codes after running the commands#
consumer_key <- "#######"

consumer_secret <- "#######"

access_token <- "#######"

access_secret <- "#######"

# also here i deleted my privat ecodes after running the commands#
#authenticating via web browser#
token <- create_token(
  app = "SentiMiner" , 
  consumer_key = "######",
  consumer_secret = "######",
  access_token = "######",
  access_secret = "######",
  set_renv = FALSE
)

# downloading tweets#
#binding more datasets
#creating a 
covidconspiracies2<- search_tweets("5G & covid -filter:replies -filter:quote -filter:retweets", n=5000, lang ="en",retryonratelimit = TRUE)
covidconspiracies3<- search_tweets("5G & coronavirus -filter:replies -filter:quote -filter:retweets", n=5000, lang ="en",retryonratelimit = TRUE)
covidconspiracies<- search_tweets("5G & covid 19 -filter:replies -filter:quote -filter:retweets", n=5000, lang ="en",retryonratelimit = TRUE)
covida= rbind(covidconspiracies, covidconspiracies2, covidconspiracies3)
covidtot1= unique(covida)
rm( covida, covidconspiracies, covidconspiracies2, covidconspiracies3,)

library(rtweet)

#extracting user's data
#calculating number of tweets per user
user <- users_data(covidtot1)
user_id_tweet = covidtot1 %>% 
  group_by(user_id, screen_name) %>% 
  summarize(Count=n()) %>% 
  arrange(desc(Count))
head(user_id_tweet)

#saving tweets in rds, open and load tweets in rds 
saveRDS(covidtot1,"covidtot1.rds")
readRDS("covidtot1.rds")
covidtot1=readRDS("covidtot1.rds")

library(tidyverse)
install.packages("dplyr")
library(dplyr)
library(lubridate)

#how many tweets per hour
# i have not used this graph in my report
tw_minute=covidtot1 %>% 
  select(created_at) %>% 
  mutate(date=round_date(created_at, unit = "hour")) %>% 
  count(date)

library(tidyverse)

#how many tweets per day
#have used it in my report 
covidtot1 %>% 
  select(created_at) %>% 
  mutate(date=round_date(created_at, unit = "day")) %>% 
  ggplot(aes(x=date))+
  geom_bar(aes(y=..count..),fill="lightblue", col="blue")

#highlights specific time span 
#have not used this graph in my report 
important_dates=ymd_hms("2020-05-02 17:05:00", "2020-05-02 17:10:00")
tw_minute %>% 
  ggplot(aes(date,n))+
  geom_line(col="darkblue")+
  geom_vline(xintercept = important_dates, linetype=2, col="red")


##**TEXT MINING##**

install.packages(c("tidytext", "stringr", "tm", "wordcloud", "reshape2", "wordcloud2", "lubridate"))
library(tidytext)
library(stringr)
library(tm)
library(wordcloud)
library(reshape2)
library(wordcloud2)
library(lubridate)
library(tidyverse)

#creating a new data frame 
text_df=covidtot1 %>% 
  select(text) %>% 
  mutate(line=1:length(text))

replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&|<|>|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

library(tidytext)
tidy_tweets <- text_df %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "^00"),
         !str_detect(word,"5g|covid"),
         str_detect(word, "[a-z]"))
tidy_tweets %>%
  count(word, sort=T) %>%
  top_n(10)

count_tw=tidy_tweets %>%
  count(word) %>%
  filter(substr(word,1,1) != '#',
         substr(word,1,1) != '##',
         substr(word,1,1) != '@')%>%
  mutate(word=reorder(word,n))

#finding most common words in tweets 
#have not used it in my report 
tidy_tweets %>%
  count(word) %>%
  filter(substr(word, 1, 1) != '#', 
         substr(word, 1, 1) != '@', 
         n > 300) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('Word count')) +
  ggtitle(paste('Most common words in tweets')) +
  theme(legend.position="none") +
  coord_flip()

# finding most common bigrams 
replace_text="https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
tidy_bigrams <- text_df %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text,replace_text , "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word,
         !str_detect(word, "^00"),
         !str_detect(next_word, "^00"),
         !next_word %in% stop_words$word,
         substr(word, 1, 1) != '@', 
         substr(next_word, 1, 1) != '@', 
         substr(word, 1, 1) != '#', 
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # 
         str_detect(next_word, "[a-z]")) %>%
  filter(line == lead(line)) %>% 
  unite(bigram, word, next_word, sep = ' ') %>%
  select(bigram,line)
#counting bigrams
count_bigrams=tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  mutate(bigram = reorder(bigram, n))

#most common bigram graphs
#used in my report
tidy_bigrams %>% 
  count(bigram, sort=TRUE) %>%
  filter(n >= 60) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('bigram count')) +
  ggtitle(paste('Most common bigrams in tweets')) +
  theme(legend.position="none") +
  coord_flip()

install.packages(wordcloud)
library(wordcloud)

#creating a wordcloud 
#used in the report
tidy_tweets %>%
  count(word) %>%
  filter(substr(word,1,1)!='#',
         substr(word,1,1)!='@')%>%
  with(wordcloud(scale=c(5,.7),
                 word,
                 n,
                 max.words=100,
                 min.freq=2,
                 random.order=F,
                 rot.per=.15,
                 colors=brewer.pal(8,"Accent")))


library(wordcloud2)

#trying different shapes to the wordcloud
frame=tidy_tweets %>%
  count(word, sort=T)

frame=data.frame(word=frame$word, freq=frame$n)

figPath = system.file("examples/t.png",package = "wordcloud2")

wordcloud2(frame,figPath = figPath, size = 1.5,color = "skyblue")
wordcloud2(frame, colour = "rando-light")
wordcloud2(frame, shape='diamond')

##**SENTIMENT ANALYSIS##**



installed.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)

get_sentiments("afinn") #afinn lexicon
get_sentiments("bing") #bing lexicon 
get_sentiments("nrc") #nrc lexion 

get_sentiments("nrc") %>% 
  count(sentiment)

#importing the lexicons in R 
afinn=get_sentiments("afinn")
bing=get_sentiments("bing")
nrc=get_sentiments("nrc")

#matching the tweet's words with the lexicon's ones
tweets_afinn=tidy_tweets%>%
  inner_join(afinn)

tweets_bing=tidy_tweets%>%
  inner_join(bing)

tweets_nrc=tidy_tweets%>%
  inner_join(nrc)


#summing up the individual score for each tweet
sentiment_A=tidy_tweets %>%
  inner_join(afinn) %>%
  group_by(line=line)%>%
  summarize(sentiment=sum(value))%>%  #*value invece di score#*
  mutate(method="AFINN")

sentiment_A= sentiment_A %>%
  inner_join(text_df, by="line")

sentiment_B_N=bind_rows(tidy_tweets %>%
                         inner_join(bing) %>%
                         mutate(method="Bing et al."),
                       tidy_tweets%>%
                         inner_join(nrc)%>%
                         filter(sentiment %in% c("positive","negative"))%>%
                         mutate(method="NRC"))%>%
  

   count(method,line=line,sentiment)%>%
  spread(sentiment,n,fill=0)%>%
  mutate(sentiment=positive-negative)

sentiment_B_N=sentiment_B_N %>%
inner_join(text_df,by="line")

#normalization
sentiment_BING= sentiment_B_N %>%
  filter(method=="Bing et al.")

sentiment_NRC=sentiment_B_N %>%
  filter(method=="NRC")

#summaries
summary=sentiment_A %>%
  inner_join(sentiment_BING, by="line") %>%
  inner_join(sentiment_NRC, by="line") %>% 
  select(line, text, sentiment, sentiment.x, sentiment.y) %>% #*oppure text senza .x#*
  rename(line=line, tweet=text, afinn=sentiment.x,
         bing=sentiment.y,
         NRC=sentiment)

summary=summary %>%
  mutate(afinn_n=afinn/(sqrt(afinn^2)+15),
         bing_n=bing/(sqrt(bing^2)+15),
         NRC_n=NRC/(sqrt(NRC^2)+15))

library(tidyverse)
concordance = summary %>%
  select(line, afinn_n, bing_n, NRC_n)

#comparing concordance and results
concordance$concordance = ifelse(summary$afinn_n > 0.2 & summary$bing_n >
                                  0.2& summary$NRC_n>0.2, "YES", ifelse(summary$afinn <
                                                                          (-0.2)&summary$bing_n < (-0.2) & summary$NRC_n < (-0.2),
                                                                        "YES", ifelse(summary$afinn_n < 0.2 & summary$afinn_n > -0.2 &
                                                                                        summary$bing_n < 0.2 & summary$bing_n > -0.2 & summary$NRC_n < 
                                                                                        0.2 & summary$NRC_N > -0.2, "YES", "NO"))
                                
concordance %>% 
  count(concordance)
concordance %>% 
  select(afinn_n, bing_n, NRC_n) %>%
  cor()

instainstall.packages(tidytext)
install.packages(ggplot2)
library(ggplot2)
library(dplyr)

#data visualization
#used in the report
bing_word_counts <- tidy_tweets %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y" ) +
  labs(y = "Contribution to sentiment" ,
       x = NULL) +
  coord_flip()

library(plyr)

#creating wordcloud
#not used in the report
tidy_tweets%>%
  inner_join(bing)%>%
  count(alist(word, sentiment))%>%
  acast(word ~ sentiment, value.var="n", fill=0 )%>%
  comparison.cloud(colors=c("red","darkgreen"),max.words=100)

#most common words of afinn lexicon 
pos_afinn=tweets_afinn%>%
  group_by(word,value)%>%
  filter(value>0) %>% 
  count() %>% 
  arrange(desc(value))

install.packages("tidyr")

library(reshape2)
library(dplyr)


pos_afinn=tweets_afinn%>%
  filter(value>0)%>% 
  count(word,sort=TRUE)


library(tidyverse)

#visualizing the most common words in afinn lexicon 
#used in the report
pos_afinn %>%
  mutate(word= reorder(word, n)) %>%
  filter(n >20) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('n')) +
  ggtitle(paste('Most common positive words (AFINN)')) +
  theme(legend.position="none") +
  coord_flip()


install.packages("reshape2")
library(reshape2)
library(tidytext)
library(stringr)
library(tm)
library(tidyverse)
library(plyr)



install.packages("rtweet")
library(rtweet)

#making wordcloud by emotion 
#not used in the report
tidy_tweets %>%
  inner_join(nrc) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=T) %>%
  acast(word ~ sentiment,value.var="n", fill=0 ) %>%
  comparison.cloud(scale=c(1,0.7),colors=brewer.pal(8,"Paired"),
  max.words=300, random.order=F,title.size = 1))


#bar of emotions 
#used in my report 
tidy_tweets%>%
  inner_join(nrc) %>%
  filter(!sentiment%in%as_tibble("negative","positive")) %>%
  ggplot(aes(x=sentiment)) +
  geom_bar(aes(y=..count.., fill=sentiment)) +
  scale_fill_brewer(palette="Paired") +
  labs(x="emotion categories", y="number of words")


#calculating frequencies of the scores per tweet according to lexicon 
#creating  bar for each lexicon 
freqsent_AFINN=table(sentiment_A$sentiment)
freqsent_AFINN
barplot(freqsent_AFINN,
        xlab ="tweets_afinn", 
        ylab="Frequency" ,
        col=heat.colors(3))

freqsent_bing=table(tweets_bing$sentiment)
freqsent_bing
barplot(freqsent_bing,
        xlab ="tweets_bing", 
        ylab="Frequency" ,
        col=heat.colors(3))

freqsent_nrc=table(tweets_nrc$sentiment)
freqsent_nrc
barplot(freqsent_nrc,
        xlab ="tweets_nrc", 
        ylab="Frequency" ,
        col=heat.colors(3))

install.packages("tidytext")
library(dplyr)

#calculating frequencies of afinn lexicon for each type of sentiment according to normalization 
summary %>% 
  select(afinn_n) %>% 
  filter(afinn_n < -0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )

summary %>% 
  select(afinn_n) %>% 
  filter(afinn_n >= -0.2 & afinn_n <= 0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )

summary %>% 
  select(afinn_n) %>% 
  filter(afinn_n > 0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )
          
# calculating frequencies of bing lexicon for each type of sentiment according to normalization 
summary %>% 
  select(bing_n) %>% 
  filter(bing_n < -0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )


summary %>% 
  select(bing_n) %>% 
  filter(bing_n >= -0.2 & bing_n <= 0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )


summary %>% 
  select(bing_n) %>% 
  filter(bing_n > 0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )

#calculating frequencie of nrc lexicon for each type of sentiment according to normalization
summary %>% 
  select(NRC_n) %>% 
  filter(NRC_n > 0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )

summary %>% 
  select(NRC_n) %>% 
  filter(NRC_n >= -0.2 & NRC_n <= 0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )


summary %>% 
  select(NRC_n) %>% 
  filter(NRC_n < -0.2) %>% 
  summarise(abs_freq = n( ),
            rel_freq = abs_freq / nrow (summary),
            perc_freq = rel_freq*100
  )

         