###################################
# import and clean inspection data
# data source: https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j
# data last update: November 15, 2022
# version: 1.0
# author: Sugar
###################################

library(tidyverse)

inspection_df = 
  read_csv("./data/raw/NYC_Restaurant_Inspection_Results.csv") %>% 
  janitor::clean_names() %>% 
  drop_na(dba) %>% 
  mutate(
    inspection_date = as.Date(inspection_date, "%m/%d/%Y"),
    grade_date = as.Date(inspection_date, "%m/%d/%Y"),
    record_date = as.Date(record_date, "%m/%d/%Y")
  ) %>% 
  mutate(
    boro = fct(boro),
    action = fct(action),
    critical_flag = fct(critical_flag),
    grade = fct(grade),
    inspection_type = fct(inspection_type)
  )

# export
inspection_df %>% write_csv(file = "./data/inspection_data.csv")

# read_csv("./data/inspection_data.csv") %>% View()
