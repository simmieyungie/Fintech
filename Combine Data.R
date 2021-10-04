#Combine playstore and Appstore data
#List of important columns from Playstore
#
#read in dataset
library(tidyverse)
library(lubridate)
#install.packages("texter")
library(texter)

#read in app store data
Appstore <- read.csv("Data/AppStoreReviewsUpdate.csv")%>%
  select(-X, -index, -developerResponse, -isEdited)

#read in playstore data
playstore <- read.csv("Data/PlayStoreReviews.csv")


playstore %>% colnames()

#content, score. at. repliedAt, brand, replyContent

Appstore %>% colnames()

#review, rating, date, modified, brand, body
new_appstore <- Appstore %>%
  rename(content = review, score = rating,
         at = date, repliedAt = modified, replyContent = body) %>%
  select(content, score, at, repliedAt, brand, replyContent) %>%
  mutate(ReviewSource = "AppStore")



#Playstore
unique(playstore$brand)

#edit playstore data
new_playstore <- playstore %>%
  mutate(brand = gsub("com.abegapp", "abeg", brand)) %>% #rename brand names
  mutate(brand = gsub("com.flutterwave.flybarter", "flutterwave", brand)) %>%
  select(content, score, at, repliedAt, brand, replyContent) %>%  #select relevant cols %>%
  mutate(ReviewSource = "Playstore")


#Combine data
df <- rbind.data.frame(new_appstore, new_playstore)


#Analysis
# Average Rating
df %>%
  group_by(ReviewSource,brand) %>%
  summarise(average_rating = mean(score)) %>%
  write.csv("AnalysisFiles/AverageRating.csv")


# Rating count of 1 - 5
df %>%
  group_by(ReviewSource, brand, score) %>%
  count() %>%
  write.csv("AnalysisFiles/RatingCount.csv")


# Review Trend
df %>%
  mutate(at = lubridate::ymd_hms(at)) %>%
  mutate(date = date(at)) %>%
  group_by(date, ReviewSource, brand) %>%
  count() %>%
  write.csv("AnalysisFiles/ReviewTrend.csv")


## Number of reviews collected
df %>%
  group_by(ReviewSource,brand) %>%
  count()%>%
  write.csv("AnalysisFiles/TotalReviews.csv")

#Number of reviews responded to
df %>%
  filter(content != "") %>%
  mutate(replystatus =  #if content is not empty but reply is empty then no reply
           if_else(content != "" & replyContent == "", "No Reply", "Reply")) %>%
  group_by(ReviewSource, brand, replystatus) %>%
  count() %>%
  ungroup() %>%
  write.csv("AnalysisFiles/CountReviewsRespondedTo.csv")

# Difference between review time and response time
df %>%
  mutate(at = ymd_hms(at)) %>%
  mutate(repliedAt = ymd_hms(repliedAt)) %>%
  mutate(responseTime = round(difftime(repliedAt, at, units = "hours"),1)) %>%
  drop_na(responseTime) %>%
  filter(responseTime >= 0) %>%  #filter negative response time as some are re-response in a convo so created time is ahead of response
  group_by(ReviewSource,brand) %>%
  summarise(AvgResponse = mean(responseTime))%>%
  write.csv("AnalysisFiles/AverageReviewResponseTime.csv")

# Average rating of responded reviews versus not responded to
df %>%
  filter(content != "") %>%
  mutate(replystatus =  #if content is not empty but reply is empty then no reply
           if_else(content != "" & replyContent == "", "No Reply", "Reply")) %>%
  group_by(ReviewSource,brand, replystatus) %>%
  summarise(AverageRating = mean(score))%>%
  write.csv("AnalysisFiles/AverageReviewReplyRating.csv")

# Overall Reaction for Apple
unique(df$ReviewSource)

#Collect data for Apple
Apple <- df %>%
  filter(ReviewSource = "AppStore")

#Extract the review sourc
x <- unique(df$ReviewSource)

# #extract the brand source
# y <-unique(df$brand)

#iterate first based on review source
plyr::rbind.fill(lapply(x, function(source){
  #filter for the source
  df2 <- filter(df, ReviewSource == source)
  # Get Overall Reaction
  a <- plyr::rbind.fill(lapply(unique(df2$brand), function(x){
    #Get sentiment
    sentimentAnalyzer(df2 %>%
                        filter(brand == x) %>%
                        select(content), details = T) %>%
      mutate(brand = x) #add brand name
  })) %>%
    mutate(ReviewSource = source) #add source name
})) %>%
  write.csv("AnalysisFiles/OverallReactions.csv")

#Get top words
plyr::rbind.fill(lapply(x, function(source){
  #filter for the source
  df2 <- filter(df, ReviewSource == source)
  # Get Overall Reaction
  a <- plyr::rbind.fill(lapply(unique(df2$brand), function(x){
    #Get the top words
    top_words(df2 %>%
                filter(brand == x) %>%
                select(content), size = 20) %>%
      mutate(brand = x)
  })) %>%
    mutate(ReviewSource = source) #add source name
})) %>%
  write.csv("AnalysisFiles/TopWords.csv")


#Bing Trend
plyr::rbind.fill(lapply(x, function(source){
  #filter for the source
  df2 <- filter(df, ReviewSource == source)
  # Get Overall Reaction
  a <- plyr::rbind.fill(lapply(unique(df2$brand), function(x){
    #Get sentiments
    top_Sentiments(df2 %>%
                      filter(brand == x) %>%
                      select(content), plot = F) %>%
      mutate(brand = x)
  })) %>%
    mutate(ReviewSource = source) #add source name
})) %>%
  write.csv("AnalysisFiles/TopSentiments.csv")


#Bigrams
plyr::rbind.fill(lapply(x, function(source){
  #filter for the source
  df2 <- filter(df, ReviewSource == source)
  # Get Overall Reaction
  a <- plyr::rbind.fill(lapply(unique(df2$brand), function(x){
    #Get sentiments
    top_bigrams(df2 %>%
                  filter(brand == x) %>%
                  select(content),bigram_size = 20) %>%
      mutate(brand = x)
  })) %>%
    mutate(ReviewSource = source) #add source name
})) %>%
  write.csv("AnalysisFiles/Bigrams.csv")


#Twiter and Instagram Analysis
