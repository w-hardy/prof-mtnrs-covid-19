set.seed(1956)

library(tidyverse)
library(lubridate)

cms_association_data <- 
  read_csv("data/association_members.csv")

cms_association_data <- 
  cms_association_data %>% 
  mutate(unique_id = 
           as.numeric(as.factor(as.numeric(interaction(candidate_id, dob, drop = TRUE)))),
         country_gb = if_else(country == "GB", TRUE, FALSE),
         sex = case_when(gender == "Female" ~ "Female",
                         gender == "Male" ~ "Male"),
         sex_id = case_when(gender == "Female" ~ 1,
                            gender == "Male" ~ 2),
         mem_mta = 
           if_else(str_detect(`member of`,"Mountain Training Association"), TRUE, FALSE),
         mem_ami =
           if_else(str_detect(`member of`,"AMI"), TRUE, FALSE),
         mem_baiml =
           if_else(str_detect(`member of`,"BAIML"), TRUE, FALSE),
         mem_bmg = 
           if_else(str_detect(`member of`,"BMG"), TRUE, FALSE),
         member_of = paste0(mem_ami, mem_baiml, mem_bmg, mem_mta),
         age = interval(dmy(dob), ymd(20200331))/years(1)) %>% # candidate age at the end of the survey window
  select(unique_id, everything()) %>% 
  select(-c(candidate_id, dob, gender, `member of`))

# check unique ids are unique

table(cms_association_data$unique_id) %>% 
  t() %>% 
  as.data.frame() %>% 
  .$Freq %>% 
  max()

# write data

write_rds(cms_association_data, "data/cms_association_data.rds")
