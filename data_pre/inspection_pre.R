###################################
# import and clean inspection data
# data source: https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j
# data last update: November 17, 2022
# version: 2.0
# author: Sugar
###################################

library(tidyverse)

inspection_df = 
  read_csv("./data/raw/DOHMH_New_York_City_Restaurant_Inspection_Results.csv") %>% 
  janitor::clean_names() %>% 
  drop_na(dba) %>% 
  mutate(
    inspection_date = as.Date(inspection_date, "%m/%d/%Y"),
    grade_date = as.Date(inspection_date, "%m/%d/%Y"),
    record_date = as.Date(record_date, "%m/%d/%Y")
  ) %>% 
  mutate(
    boro = na_if(boro, "0"),
    building = na_if(building, "N/A"),
    street = na_if(street, "N/A")
  ) %>% 
  mutate(
    boro = fct(boro),
    action = fct(action),
    critical_flag = fct(critical_flag),
    grade = fct(grade),
    inspection_type = fct(inspection_type)
  ) %>% 
  arrange(desc(inspection_date))

rest_df = 
  inspection_df %>% 
  select(camis, dba, boro, building, street, zipcode) %>% 
  distinct() %>% 
  mutate(
    across(everything(), as.character)
  ) %>% 
  rowid_to_column()

# export
inspection_df %>% write_csv(file = "./data/inspection_data.csv")
rest_df %>% write_csv(file = "./data/restaurant_data.csv")

# read_csv("./data/inspection_data.csv") %>% View()

###################################
# # Install Git Large file system
#
# $ brew install git-lfs
# $ git lfs install
# $ git lfs track "*.csv"
# $ git add .gitattributes
# 
# Add, Commit, Push
