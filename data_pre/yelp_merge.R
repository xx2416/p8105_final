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
  drop_na(distance) %>% 
  filter(distance < 1.5) %>% 
  arrange(rowid) 

# check duplicate

yelp_df %>% 
  count(rowid) %>% 
  arrange(desc(n))

# export

# yelp_df %>% write_excel_csv("./data/yelp_data.csv")

inspection_df = 
  read_csv("./data/inspection_data.csv") %>% 
  distinct() %>% 
  select(-location_point)

inspection_all_date = 
  inspection_df %>% 
  right_join(yelp_df %>% select(-rowid), keep = FALSE)

inspection_all_date %>% write_excel_csv("./data/inspection_sub_all_date.csv")


inspection_sig_date = 
  inspection_all_date %>% 
  nest(inspection_detail = action:critical_flag)

inspection_sig_date %>% write_excel_csv("./data/inspection_sub_sig_date.csv")

inspection_latest_date = 
  inspection_sig_date %>% 
  group_by(camis) %>% 
  slice(which.max(as.Date(inspection_date))) 

inspection_latest_date %>% write_excel_csv("./data/inspection_sub_latest_date.csv")
  