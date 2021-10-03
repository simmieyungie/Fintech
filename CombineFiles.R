#read in dataset
library(tidyverse)
library(lubridate)
#install.packages("texter")
library(texter)
library(jsonlite)

#Combine google and apple datasets
Appstore <- read.csv("Data/AppStoreReviews.csv")


fromJSON(toJSON(Appstore$developerResponse), flatten = T)

(Appstore$developerResponse)
json_file <- fromJSON(Appstore$developerResponse, flatten = T)

json_file <- lapply(Appstore$developerResponse, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})


list(Appstore$developerResponse)

(Appstore$developerResponse) %>%
  unlist()

json_file
sample.df$json_col<- as.character(sample.df$json_col)
json_obj<- paste(sample.df$json_col, collapse = "")
json_obj<- stri_replace_all_fixed(json_obj, "][", ",")

library(stringi)

test <- Appstore %>%
  mutate(developerResponse = as.character(developerResponse)) %>%
  mutate(developerResponse = paste(developerResponse, collapse = "")) %>%
  mutate(developerResponse = stri_replace_all_fixed(developerResponse, "][", ","))

library(jsonlite)
test <- Appstore %>%
  mutate(developerResponse = toJSON(developerResponse)) %>%
  mutate(
    json_parsed = map(developerResponse, ~ fromJSON(., flatten=TRUE))
  ) %>%
  unnest(json_parsed)

#rm(test)
fromJSON(gsub(",", "", paste0('[',toString(head(Appstore$developerResponse)),']')))
