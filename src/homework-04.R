

# clear console & environment

cat("\014")    
rm(list=ls(all=TRUE))

# packages

library("gcookbook")
library("ggplot2"  )

#------------------------------------------------------
# 1.
#------------------------------------------------------

# reading data

tweets <-  read.csv2( file = "data/clinton_trump_tweets.csv" )

#------------------------------------------------------
# 2.
#------------------------------------------------------

# reproducing 'tweet1.png' using ggplot2

ggplot(data = tweets, aes(x = tweets$handle, fill = tweets$handle)) +
       geom_bar(stat = "count") +
       scale_fill_manual( name = "Candidates",labels = c("Hillary Clinton",
                         "Donald Trump"),values = c("blue","red")) +
       ggtitle("Candidate Tweets") + 
       theme(plot.title       = element_text(hjust = 0.5),
             axis.title.x     = element_blank(),
             axis.text.x      = element_blank(), 
             axis.ticks.x     = element_blank(), 
             axis.title.y     = element_text("Numbers"), 
             panel.background = element_rect(fill = 'white', colour = 'white')) +
       labs(y="Tweet frequency")

# export .png

ggsave("fig/tweet1.png", width = 6.16, height = 4.52, dpi = 100)

#------------------------------------------------------
# 3.
#------------------------------------------------------

# checking languages
table(tweets$lang)

# print
print(tweets$text[which(tweets$lang=="da")])
print(tweets$text[which(tweets$lang=="et")])
print(tweets$text[which(tweets$lang=="fi")])
print(tweets$text[which(tweets$lang=="fr")])
print(tweets$text[which(tweets$lang=="tl")])

# rewritting 'lang' to 'en'
tweets$lang[which(tweets$lang=="da")] <- c("en")
tweets$lang[which(tweets$lang=="fi")] <- c("en")
tweets$lang[which(tweets$lang=="fr")] <- c("en")

# new dataset of tweets in 'en' and 'es' languages only
tweets_enes <- tweets[(tweets$lang == "en") | (tweets$lang == "es"),]

# reproducing 'tweet2.png' using ggplot2
ggplot(data = tweets_enes, aes(x = tweets_enes$handle, 
                               fill = tweets_enes$lang)) +
       geom_bar(stat = "count", position = position_dodge()) +
       scale_fill_manual( name   = "Language",
                          labels = c("English", "Spanish"),
                          values = c("darkgray","cornflowerblue")) +
       ggtitle("Language of Tweets") + 
       scale_x_discrete(labels=c("Hillary Clinton","Donald Trump")) +
       theme(plot.title       = element_text(hjust = 0.5),
             axis.title.x     = element_blank(),
             axis.ticks.x     = element_blank(),
             axis.title.y     = element_text("Numbers"),
             panel.background = element_rect(fill = 'white', colour = 'white')) +
       labs(y="Tweet frequency")

# export .png

ggsave("fig/tweet2.png", width = 6.16, height = 4.52, dpi = 100)

#------------------------------------------------------
# 4.
#------------------------------------------------------

# examples

popular_tweets ( "Hillary Clinton", 10 )
popular_tweets ( "Donald Trump"   , 15 )

#------------------------------------------------------
#                       III.feladat
#------------------------------------------------------

# packages

if (!("fivethirtyeight" %in% installed.packages())) {
  install.packages("fivethirtyeight", dependencies  =  T)}
library(fivethirtyeight)

# loading data

data(package ="fivethirtyeight")
data(hiphop_cand_lyrics)
head(hiphop_cand_lyrics)

#------------------------------------------------------
# 1.
#------------------------------------------------------

# first plot

hiphop1<- ggplot(hiphop_cand_lyrics, 
                 aes(x = hiphop_cand_lyrics$album_release_date, 
                  fill = hiphop_cand_lyrics$candidate)) +
                 ggtitle("Every mention of 2016 primary candidates in hip-hop 
                         songs") +
                 labs(fill = "") +
                 scale_fill_manual(values = c("#A6D854", # B.Carson
                                              "#66C2A5", # Bernie
                                              "#FFD92F", # Christie
                                              "#FEC075", # Trump
                                              "#94D7FA", # Hillary
                                              "#FF8974", # Jeb
                                              "#FCCDE5", # Mike
                                              "#E78AC3"  # Cruz
                                              ))+
                 geom_dotplot(binwidth = .35, method = 'histodot', 
                 stackgroups = TRUE) + ylim(0, 42) +
                 theme(plot.title   = element_text(hjust = 0.5),
                       axis.title.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position  = "top",
                       legend.direction = "horizontal")

# export .png
ggsave("fig/hiphop1.png", width = 6.16, height = 4.52, dpi = 100)

#------------------------------------------------------

# second plot

# new dataframes

pos <- hiphop_cand_lyrics[(hiphop_cand_lyrics$sentiment == "positive"),]
neu <- hiphop_cand_lyrics[(hiphop_cand_lyrics$sentiment == "neutral") ,]
neg <- hiphop_cand_lyrics[(hiphop_cand_lyrics$sentiment == "negative"),]



# 'positive' plot

pos_plot <- ggplot(pos, aes(x    = pos$album_release_date,
                           fill = pos$candidate)) +
                  ggtitle("\n \n POSITIVE \n \n \n \n") +
                  labs(fill = "") +
                  scale_fill_manual( values = c("#A6D854", # B.Carson
                                                "#66C2A5", # Bernie
                                                "#FEC075", # Trump
                                                "#94D7FA"  # Hillary
                                                )) +
                  geom_dotplot(binwidth = 1, method = 'histodot',
                  stackgroups = TRUE) +
                  ylim(0,35) +
                  theme(plot.title   = element_text(hjust = 0.5),
                        axis.title.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.position  = "top",
                        legend.direction = "horizontal")



# 'neutral' plot

neu_plot <- ggplot(neu, aes(   x = neu$album_release_date, 
                           fill = neu$candidate))+
                  ggtitle("\n \n NEUTRAL \n \n \n \n")+
                  labs(fill = "") +
                  scale_fill_manual( values = c("#FFD92F", # Christie
                                                "#FEC075", # Trump
                                                "#94D7FA", # Hillary
                                                "#FF8974", # Jeb
                                                "#FCCDE5"  # Mike
                                                )) +
                  geom_dotplot(binwidth = 1, method = 'histodot',
                               stackgroups = TRUE) +
                  ylim(0,35) +
                  theme(plot.title   = element_text(hjust = 0.5),
                        axis.title.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.position  = "top",
                        legend.direction = "horizontal")



# 'negative' plot

neg_plot <- ggplot(neg, aes(   x = neg$album_release_date, 
                             fill = neg$candidate))+
                    ggtitle("Candidate mentions, by sentiment \n \n NEGATIVE")+
                    labs(fill = "") +
                    scale_fill_manual( values = c("#66C2A5", # Bernie
                                                  "#FEC075", # Trump
                                                  "#94D7FA", # Hillary
                                                  "#FF8974", # Jeb
                                                  "#FCCDE5", # Mike
                                                  "#E78AC3"  # Cruz
                                          )) +
                    geom_dotplot(binwidth = 1, method = 'histodot',
                                 stackgroups = TRUE) +
                    ylim(0,35) +
                    theme(plot.title   = element_text(hjust = 0.5),
                          axis.title.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.title.y = element_blank(),
                          legend.position  = "top",
                          legend.direction = "horizontal")


# multiplot

multiplot(pos_plot,neg_plot,neu_plot, cols=3) 

#export

dev.copy(png,"fig/hiphop2.png",width=8,height=6,units="in",res=100)
dev.off()

#------------------------------------------------------
# 2.
#------------------------------------------------------

hiphop3 <- ggplot(hiphop_cand_lyrics,
                  aes(x    = hiphop_cand_lyrics$sentiment,
                      y    = hiphop_cand_lyrics$album_release_date,
                      fill = hiphop_cand_lyrics$sentiment)) + 
                  geom_boxplot() +
                  ggtitle("The distribution of emotions on years") +
                  scale_fill_manual( name = "Emotions",
                  labels = c("negative", "neutral","positive"),
                  values = c("red","darkgray","cornflowerblue"))
                
#export

ggsave("fig/hiphop3.png", width = 6.16, height = 4.52, dpi = 100)

#------------------------------------------------------
#                       IV.feladat
#------------------------------------------------------
#------------------------------------------------------
# 1.
#------------------------------------------------------

# plots
emo <- ggplot(tweets, aes(x = tweets$text_emotion, fill = tweets$handle)) +
  geom_bar(stat = "count", position = position_dodge())

emo

senti <- ggplot(tweets, aes(x = tweets$text_sentiment, fill = tweets$handle)) +
  geom_bar(stat = "count", position = position_dodge())

senti
# table 
  emo_clinton  <- table( tweets[ tweets$handle == 
                                 "HillaryClinton", 
                                 c("text_emotion") ] )

     emo_trump <- table( tweets[ tweets$handle == 
                                 "realDonaldTrump", 
                                 c("text_emotion") ] )

senti_clinton  <- table( tweets[ tweets$handle == 
                                 "HillaryClinton", 
                                 c("text_sentiment") ] )

   senti_trump <- table( tweets[ tweets$handle == 
                                 "realDonaldTrump", 
                                 c("text_sentiment") ] )

# two sample t-test (emotions & sentiments)

t.test(emo_clinton  , emo_trump  , paired = TRUE)
# there is no statistical difference assuming alpha=5% level of sign.

t.test(senti_clinton, senti_trump, paired = TRUE)
# there is no statistical difference assuming alpha=5% level of sign.

# time-series
#ggplot(tweets, aes(x = tweets$time, y=tweets$time, fill = tweets$text_emotion)) + geom_line()

#time2<-substr(tweets$time, 3, 10)
#tweets$time2<-gsub('-', '', time2)

#tweets$time2 <- time
#plot(time, tweet$text_emotion)

#ggplot(tweets,  aes(x = tweets$time2 ,  fill = tweets$text_emotion))

#------------------------------------------------------
# 2.
#------------------------------------------------------

# new dataset
# step (1)
tweets_writers <- tweets[(tweets$source_url ==
                            "http://twitter.com/download/iphone")  |
                         (tweets$source_url ==
                            "http://twitter.com/download/android"),]
# step (2)
tweets_writers <- tweets_writers[(tweets_writers$handle != "HillaryClinton"),]

# emotions
writers_emo <- ggplot(tweets_writers, aes(x    = tweets_writers$text_emotion,
                                          fill = tweets_writers$source_url)) +
                      geom_bar(stat = "count", position = position_dodge()) +
                      scale_fill_manual( values = c("lawngreen","skyblue"))

writers_emo

# sentiments
writers_senti <- ggplot(tweets_writers, aes(x    = tweets_writers$text_sentiment, 
                                            fill = tweets_writers$source_url)) +
                        geom_bar(stat = "count", position = position_dodge()) +
                        scale_fill_manual( values = c("lawngreen","skyblue"))

writers_senti
# table 
emo_iphone  <- table( tweets_writers[ tweets_writers$source_url == 
                                       "http://twitter.com/download/iphone", 
                                     c("text_emotion") ] )

emo_android <- table( tweets_writers[ tweets_writers$source_url == 
                                       "http://twitter.com/download/android", 
                                     c("text_emotion") ] )

senti_iphone  <- table( tweets_writers[ tweets_writers$source_url == 
                                       "http://twitter.com/download/iphone", 
                                     c("text_sentiment") ] )

senti_android <- table( tweets_writers[ tweets_writers$source_url == 
                                       "http://twitter.com/download/android", 
                                     c("text_sentiment") ] )

# two sample t-test (emotions & sentiments)

t.test(emo_iphone  , emo_android  , paired = TRUE)
# there IS statistical difference assuming alpha=5% level of sign.

t.test(senti_iphone, senti_android, paired = TRUE)
# there IS statistical difference assuming alpha=5% level of sign.

#------------------------------------------------------
# 3.
#------------------------------------------------------

# packages
install.packages(c("markdownr","knitr"))
library(c("markdownr","knitr"))