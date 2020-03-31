library(tidyverse)

association_data <- 
  read_csv("data/association_members.csv")

association_data <- 
  association_data %>% 
  mutate(mem_mta = 
           if_else(str_detect(`member of`,"Mountain Training Association"), TRUE, FALSE),
         mem_ami =
           if_else(str_detect(`member of`,"AMI"), TRUE, FALSE),
         mem_baiml =
           if_else(str_detect(`member of`,"BAIML"), TRUE, FALSE),
         mem_bmg = 
           if_else(str_detect(`member of`,"BMG"), TRUE, FALSE))

association_data <- 
  transform(association_data, 
            unique_id = as.numeric(as.factor(as.numeric(interaction(candidate_id, dob))))) %>% 
  select(-candidate_id, unique_id, everything())


# check unique ids are unique

table(association_data$unique_id)

# write data

write_rds(association_data, "data/association_data.rds")
