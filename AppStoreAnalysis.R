#read in dataset
library(tidyverse)
library(lubridate)
#install.packages("texter")
library(texter)


#Combine google and apple datasets
Appstore <- read.csv("Data/AppStoreReviewsUpdate.csv")%>%
  select(-X, -index, -developerResponse, -isEdited)

#
Appstore %>% head(5)

#ANALYSIS FOR APPSTORE
# Average Rating
Appstore %>%
  group_by(brand) %>%
  summarise(average_rating = mean(rating))

# Rating count of 1 - 5
Appstore %>%
  group_by(brand, rating) %>%
  count() %>%
  ggplot(., aes(rating, n, fill = brand)) +
  geom_bar(stat = "identity", position = "dodge")

# Review Trend
Appstore %>%
  mutate(date = lubridate::ymd_hms(date)) %>%
  mutate(date = date(date)) %>%
  group_by(date, brand) %>%
  count() %>%
  ggplot(., aes(date, n, group = brand)) +
  geom_line(aes(color = brand))


# Number of reviews collected
Appstore %>%
  group_by(brand) %>%
  count()

# Number of reviews responded to
Appstore %>%
  filter(review != "") %>%
  mutate(replystatus =  #if content is not empty but reply is empty then no reply
           if_else(review != "" & body == "", "No Reply", "Reply")) %>%
  group_by(brand, replystatus) %>%
  count() %>%
  ungroup() %>%
  group_by(brand, replystatus) %>%
  summarise(AverageRating = mean(rating))

# ## Difference between review time and response time
# Appstore %>%
#   mutate(at = ymd_hms(date)) %>%
#   mutate(repliedAt = ymd_hms(modified)) %>%
#   mutate(responseTime = round(difftime(at, repliedAt, units = "hours"),1)) %>%
#   drop_na(responseTime) %>%
#   filter(responseTime >= 0) %>%  #filter negative response time as some are re-response in a convo so created time is ahead of response
#   group_by(brand) %>%
#   summarise(AvgResponse = mean(responseTime))

# Number of reviews responded to
Appstore %>%
  filter(review != "") %>%
  mutate(replystatus =  #if content is not empty but reply is empty then no reply
           if_else(review != "" & body == "", "No Reply", "Reply")) %>%
  group_by(brand, replystatus) %>%
  count() %>%
  ungroup()#%>%

#playstore - appstore
#score - rating
#content - review
#
