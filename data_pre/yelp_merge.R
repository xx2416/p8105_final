# merge review data

library(tidyverse)

file_path = "./data/yelp/"
file_list = list.files("./data/yelp/")

yelp_df = tibble()

for (file in file_list){
  yelp_sub_df = read_csv(str_c(file_path, file))
  yelp_df = yelp_df %>% rbind(yelp_sub_df)
}

yelp_df = 
  yelp_df %>% 
  mutate(
    rowid = as.integer(rowid)
  ) %>% 
  arrange(rowid)

# check duplicate

yelp_df %>% 
  count(rowid) %>% 
  arrange(desc(n))

# export

yelp_df %>% write_excel_csv("./data/yelp_data.csv")

