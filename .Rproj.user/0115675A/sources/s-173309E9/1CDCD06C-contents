#read in dataset
library(tidyverse)
library(lubridate)
#install.packages("texter")
library(texter)

#data
playstore <- read.csv("Data/PlayStoreReviews.csv")

playstore %>%
  mutate()
#analysis
# Average Rating
playstore %>%
  group_by(brand) %>%
  summarise(average_rating = mean(score))

# Rating count of 1 - 5
playstore %>%
  group_by(brand, score) %>%
  count() %>%
  ggplot(., aes(score, n, fill = brand)) +
  geom_bar(stat = "identity", position = "dodge")
#Notes
#What are each of these groups saying. Why are they giving these ratings

# Review Trend
playstore %>%
  mutate(at = lubridate::ymd_hms(at)) %>%
  mutate(date = date(at)) %>%
  group_by(date, brand) %>%
  count() %>%
  ggplot(., aes(date, n, group = brand)) +
  geom_line(aes(color = brand))

# Number of reviews collected
playstore %>%
  group_by(brand) %>%
  count()

# Number of reviews responded to
playstore %>%
  filter(content != "") %>%
  mutate(replystatus =  #if content is not empty but reply is empty then no reply
           if_else(content != "" & replyContent == "", "No Reply", "Reply")) %>%
  group_by(brand, replystatus) %>%
  count() %>%
  ungroup()#%>%
  # ggplot(., aes(replystatus, n, fill = brand)) +
  # geom_bar(stat = "identity", position = "dodge")

# Difference between review time and response time
playstore %>%
  mutate(at = ymd_hms(at)) %>%
  mutate(repliedAt = ymd_hms(repliedAt)) %>%
  mutate(responseTime = round(difftime(repliedAt, at, units = "hours"),1)) %>%
  drop_na(responseTime) %>%
  filter(responseTime >= 0) %>%  #filter negative response time as some are re-response in a convo so created time is ahead of response
  group_by(brand) %>%
  summarise(AvgResponse = mean(responseTime))

# Average rating of responded reviews versus not responded to
playstore %>%
  filter(content != "") %>%
  mutate(replystatus =  #if content is not empty but reply is empty then no reply
           if_else(content != "" & replyContent == "", "No Reply", "Reply")) %>%
  group_by(brand, replystatus) %>%
  summarise(AverageRating = mean(score))

# Overall Reaction
plyr::rbind.fill(lapply(unique(playstore$brand), function(x){
  sentimentAnalyzer(playstore %>%
                      filter(brand == x) %>%
                      select(content), details = T) %>%
    mutate(brand = x)
  })) %>%
  ggplot(., aes(x = sentiment, y = n, fill = brand)) +
  geom_bar(stat = "identity", position = "dodge")
#Note
#Resample to equal size and retest

# Top words used in reviews
plyr::rbind.fill(lapply(unique(playstore$brand), function(x){
  top_words(playstore %>%
                      filter(brand == x) %>%
                      select(content), size = 20) %>%
    mutate(brand = x)
}))
# Top positive and top negative words
plyr::rbind.fill(lapply(unique(playstore$brand), function(x){
  top_Sentiments(playstore %>%
              filter(brand == x) %>%
              select(content), plot = F) %>%
    mutate(brand = x)
}))

# Bigram for phrases
plyr::rbind.fill(lapply(unique(playstore$brand), function(x){
  top_bigrams(playstore %>%
                   filter(brand == x) %>%
                   select(content),bigram_size = 20) %>%
    mutate(brand = x)
}))





