###################################
# extract review data from Yelp*
# data last update: November 16, 2022
# version: 1.0
# author: Sugar
###################################

library(tidyverse)

# read restaurant info from inspection data set
res_data = 
  read_csv("./data/inspection_data.csv") %>% 
  select(dba, boro, building, street, zipcode) %>% 
  mutate(
    across(everything(), as.character)
  ) %>% 
  distinct()

# check missing values
skimr::skim_without_charts(res_data)

# generate yelp url
res_data = 
  res_data %>% 
  replace_na(list(building = " ", zipcode = " "))%>% 
  mutate(
    url = str_c("https://www.yelp.com/search?find_desc=", dba, "&find_loc=", building,"+", street, "+",boro,"+", zipcode) %>% 
      str_replace(" ", "+")
  )

# function of getting review data

# save