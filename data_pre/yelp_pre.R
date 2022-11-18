###################################
# extract review data from Yelp*
# data last update: November 17, 2022
# version: 2.0
# author: Sugar
###################################

library(tidyverse)
library(rvest)
library(httr)
library(urltools)

# read restaurant info from inspection data set
res_data = 
  read_csv("./data/restaurant_data.csv") %>% 
  mutate(
    across(everything(), as.character)
  ) 

# check missing values
# skimr::skim_without_charts(res_data)

# generate yelp url
res_data = 
  res_data %>% 
  replace_na(list(boro = " ", building = " ", street = " ", zipcode = " "))%>% 
  mutate(
    url = str_c("https://www.yelp.com/search?find_desc=", url_encode(dba), 
                "&find_loc=", url_encode(building),"+", 
                url_encode(street), "+", 
                url_encode(boro),"+", zipcode) %>% 
      str_replace_all("%20", "+")
  )

# function of getting review data
get_info = function(url){
  tryCatch({
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
    
    # rating = 
    #   data %>% 
    #   str_split(name) %>%
    #   .[[1]] %>% .[2] %>% 
    #   str_split(review_num, n = 2) %>% 
    #   .[[1]] %>% .[1] 
    
    rating = 
      html_page %>% 
      html_elements(".css-gutk1c") %>%
      html_text() %>% .[4]
    
    closeAllConnections()
    
    return(tibble(
      name = name,
      distance = distance,
      rating = as.double(rating), 
      review_num = review_num,
      price = price,
      data = ifelse(is.na(name), NA, data)
    ))
  }, 
  error=function(e){
    message(paste("URL does not exist:", url, "\n"))
    return(
      tibble(
        name = NA,
        distance = NA,
        rating = NA, 
        review_num = NA,
        price = NA,
        data = NA
      )
    )
  })
  
}

start_input = readline(prompt="Enter start index: ")
start_index = as.integer(start_input)
end_input = readline(prompt="Enter end index: ")
end_index = as.integer(end_input)

start_time = Sys.time()


# get info
res_data_1 =
  res_data %>% slice(start_index:end_index) %>% 
  mutate(
    yelp = map(.x = url, ~ get_info(.x))
   ) %>% 
  unnest(yelp) 

end_time1 = Sys.time()
running_time_1 = end_time1 - start_time

res_data_2 = 
  res_data_1 %>% 
  drop_na(name, data) %>% 
  filter(
    is.na(rating) | is.na(distance)
  ) %>% 
  select(rowid:url) %>% 
  mutate(
    yelp = map(.x = url, ~ get_info(.x))
  ) %>% 
  unnest(yelp) 

end_time2 = Sys.time()
running_time_2 = end_time2 - start_time

res_data_3 = 
  res_data_2 %>% 
  drop_na(name, data) %>% 
  filter(
    is.na(rating) | is.na(distance)
  ) %>% 
  select(rowid:url) %>% 
  mutate(
    yelp = map(.x = url, ~ get_info(.x))
  ) %>% 
  unnest(yelp) 

end_time3 = Sys.time()
running_time_3 = end_time3 - start_time

res_data_4 = 
  res_data_3 %>% 
  drop_na(name, data) %>% 
  filter(
    is.na(rating) | is.na(distance)
  ) %>% 
  select(rowid:url) %>% 
  mutate(
    yelp = map(.x = url, ~ get_info(.x))
  ) %>% 
  unnest(yelp) 

end_time4 = Sys.time()
running_time_4 = end_time4 - start_time

res_data_5 = 
  res_data_4 %>% 
  drop_na(name, data) %>% 
  filter(
    is.na(rating) | is.na(distance)
  ) %>% 
  select(rowid:url) %>% 
  mutate(
    yelp = map(.x = url, ~ get_info(.x))
  ) %>% 
  unnest(yelp) 

end_time5 = Sys.time()
running_time_5 = end_time5 - start_time

result = res_data_1 %>% 
  rbind(res_data_2) %>%
  rbind(res_data_3) %>%
  rbind(res_data_4) %>%
  drop_na(distance) %>% 
  rbind(res_data_5) %>% 
  drop_na(rating) %>% 
  mutate(
    review_num = str_split(
      review_num, "review", n = 2, simplify = TRUE) %>%
      .[,1] %>% 
      str_sub(2, -2) %>% 
      as.integer()
  )
  
# end_time = Sys.time()
# running_time = end_time - start_time

c(running_time_1, running_time_2, running_time_3, running_time_4, running_time_5)


## timing history (asyn)
# 51 samples / run 2 times => 2.58 min => 35 ratings (32 valid)
# 51 samples / run 3 times => 3.09 min => 40 ratings (39 valid)
# 51 samples / run 4 times => 3.45 min => 42 ratings (41 valid)

## timing in one run
## 51 samples => all results / valid results
# 2 iteration => 2.77 min => 23+6+3 = 32/29 results
# 3 iteration => 3.42 min => 23+6+9+3 = 41/39 results
# 4 iteration => 3.77 min => 23+6+9+2+3 = 43/40 results
# 5 iteration => 4.05 min => 23+6+9+2+3+0 = 43/43 results

# save

file_name = str_c("./data/yelp/yelp_data_", as.character(start_index), "_", as.character(end_index), ".csv")

result %>% 
  select(-data) %>% 
  write_excel_csv(file = file_name)


