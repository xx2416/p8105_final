###################################
# extract review data from Yelp*
# data last update: November 16, 2022
# version: 1.0
# author: Sugar
###################################

library(tidyverse)
library(rvest)
library(httr)

# read restaurant info from inspection data set
res_data = 
  read_csv("./data/inspection_data.csv") %>% 
  select(camis, dba, boro, building, street, zipcode) %>% 
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
      str_replace_all(" ", "+")
  )

# function of getting review data
get_info = function(url){
  html_page = read_html(url)
 
   data = 
    html_page %>% 
    html_elements(".list__09f24__ynIEd") %>% 
    html_text() %>% .[1]
  
  info = 
    html_page %>% 
    html_elements(".css-chan6m") %>% 
    html_text() 
  
  review_num = 
    info[2]
  
  distance = 
    info[4] %>% 
    str_split("Miles", n=2) %>% 
    .[[1]] %>% .[1] %>% 
    as.double()
  
  name = 
    html_page %>%  
    html_elements(".css-1egxyvc") %>% 
    html_text() %>% .[1] %>% 
    str_sub(4)
  
  price =
    html_page %>% 
    html_elements(".css-1s7bx9e") %>%
    html_text() %>% .[1]
    
   # type = 
   #   html_page %>% 
   #   html_elements(".css-q7yb35:nth-child(1) .css-11bijt4") %>% 
   #   html_text() %>% .[1]
  
  
  rating = 
    data %>% 
    str_split(name) %>%
    .[[1]] %>% .[2] %>% 
    str_split(review_num, n = 2) %>% 
    .[[1]] %>% .[1] 
  
    
  tibble(
    name = name,
    distance = distance,
    rating = rating, 
    review_num = review_num,
    price = price,
    data = data
    
  )
}

# get info
res_data_3 =
  res_data %>% slice(1000:1020) %>% 
  mutate(yelp = map(.x = url, ~ get_info(.x))) %>% 
  unnest(yelp)
  
# save