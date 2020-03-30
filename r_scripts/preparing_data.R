library(tidyverse)
library(haven)
library(lubridate)

# functions
totalSum <- function(a, b) {
  naSum <- function(a, b) {
    # treat NA as 0 unless both values are NA
    
    if (is.na(a) & is.na(b)) {
      NA
    } else
      if (is.na(a)) {
        a <- 0
      }
    if (is.na(b)) {
      b <- 0
    }
    a + b
  }
  map2(.x = a, .y = b, .f = naSum) %>%
    unlist()
}

positiveNumber <- function(x) {
  return((x ^ 2) ^ .5)
}

inThousands <- function(x) {
  if (is.na(x)) {
    FALSE
  } else
    if ((x < 100) == TRUE) {
      TRUE
    } else
      FALSE
}

# reading data from Qualtrics
raw_survey_data <- read_spss("data/covid-19_29+March+2020_09.42.sav")

# preparing data
survey_data <- 
  raw_survey_data %>% 
  transmute(start_date = ymd_hms(StartDate),
            end_date = ymd_hms(EndDate),
            ip_address = IPAddress,
            progress = Progress,
            response_id = ResponseId,
            age = Q3 + 17,
            sex_id = Q4,
            sex = case_when(Q4 == 1 ~ "Female",
                            Q4 == 2 ~ "Male",
                            Q4 == 3 ~ "Intersex",
                            Q4 == 4 ~ "I prefer not to say"),
            qual_bmg = 
              if_else(Q6_1 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_wmci = 
              if_else(Q6_2 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_mci = 
              if_else(Q6_3 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_iml = 
              if_else(Q6_4 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_wml = 
              if_else(Q6_5 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_ml = 
              if_else(Q6_6 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_rci = 
              if_else(Q6_7 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_rcdi = 
              if_else(Q6_8 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_hml = 
              if_else(Q6_9 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_ll = 
              if_else(Q6_10 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_cwi = 
              if_else(Q6_11 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            qual_cwdi = 
              if_else(Q6_12 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            mem_mta = 
              if_else(Q7_1 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            mem_ami = 
              if_else(Q7_2 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            mem_bmg = 
              if_else(Q7_3 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            mem_baiml = 
              if_else(Q7_4 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            self_employed = 
              if_else(Q47_1 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            owner_ltd = 
              if_else(Q47_2 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            employee = 
              if_else(Q47_3 == 1, true = TRUE, false = FALSE, missing = FALSE ),
            ltd_ownership = (Q43 - 1) * 10,
            primary_emp_code = Q4,
            primary_emp_status = 
              case_when(Q4 == 1 ~ "Self-employed",
                        Q4 == 2 ~ "Owner/shareholder of limited company",
                        Q4 == 3 ~ "Employee"),
            `Employment status` = 
              case_when(self_employed == TRUE & 
                          employee == TRUE & 
                          owner_ltd == TRUE ~ 
                          "Self-employed, employee & owner",
                        self_employed == TRUE & 
                          employee == TRUE & 
                          owner_ltd == FALSE ~ 
                          "Self-employed & employee",
                        self_employed == TRUE & 
                          employee == FALSE & 
                          owner_ltd == TRUE ~ 
                          "Self-employed, & owner",
                        self_employed == FALSE & 
                          employee == TRUE & 
                          owner_ltd == TRUE ~ 
                          "Employee & owner",
                        self_employed == TRUE & 
                          employee == FALSE & 
                          owner_ltd == FALSE ~ 
                          "Self-employed",
                        self_employed == FALSE & 
                          employee == TRUE & 
                          owner_ltd == FALSE ~ 
                          "Employee",
                        self_employed == FALSE & 
                          employee == FALSE & 
                          owner_ltd == TRUE ~ 
                          "Owner"),
            work_uk = if_else(Q9 == 1, true = TRUE, false = FALSE),
            
            self_income_18_19 = Q11_1,
            self_exp_income_change_code = Q46,
            self_exp_income_change = case_when(Q46 == 1 ~ "More than",
                                               Q46 == 2 ~ "About the same",
                                               Q46 == 3 ~ "Less than"),
            self_exp_income = if_else(Q46 == 2, true = self_income_18_19, false = Q52_1),
            self_exp_losses = Q20_1,
            self_expenses = if_else(Q17 == 1, true = TRUE, false = FALSE),
            self_expenses_desc = Q18,
            self_expenses_exp = Q19_1,
            self_alt_career = if_else(Q13 == 1, true = TRUE, false = FALSE),
            self_alt_career_desc = Q14,
            self_alt_career_support_code = Q15,
            self_alt_career_support = case_when(Q15 == 1 ~ "Yes",
                                                Q15 == 2 ~ "Maybe",
                                                Q15 == 3 ~ "No"),
            
            employee_income_18_19 = Q54_1,
            employee_exp_income_change_code = Q55,
            employee_exp_income_change = case_when(Q55 == 1 ~ "More than",
                                                   Q55 == 2 ~ "About the same",
                                                   Q55 == 3 ~ "Less than"),
            employee_exp_income = if_else(Q55 == 2, employee_income_18_19, Q56_1),
            employee_exp_losses = Q58_1,
            employee_expenses = if_else(Q59 == 1, true = TRUE, false = FALSE),
            employee_expenses_desc = Q60,
            employee_expenses_exp = Q61_1,
            employee_alt_career = if_else(Q62 == 1, true = TRUE, false = FALSE),
            employee_alt_career_desc = Q63,
            employee_alt_career_support_code = Q64,
            employee_alt_career_support = case_when(Q64 == 1 ~ "Yes",
                                                    Q64 == 2 ~ "Maybe",
                                                    Q64 == 3 ~ "No"),
            
            company_gross_profit_18_19 = Q49_1,
            company_exp_losses = positiveNumber(Q50_1),
            company_expenses = if_else(Q24 == 1, true = TRUE, false = FALSE),
            company_expenses_desc = Q25,
            company_expenses_exp = Q26_1,
            
            private_daily_rate = Q28_1,
            employee_daily_rate = Q29_1,
            lowest_daily_rate = Q30_1,
            
            total_income_18_19 = 
              totalSum(self_income_18_19, employee_income_18_19),
            total_exp_income =
              totalSum(self_exp_income, employee_exp_income),
            total_exp_losses = 
              totalSum(self_exp_losses, employee_exp_losses),
            total_exp_expenses = 
              totalSum(self_expenses_exp, employee_expenses_exp),
            alternate_career = 
              if_else(self_alt_career == TRUE | employee_alt_career == TRUE, 
                      true = TRUE, false = FALSE)) %>% 
  filter(start_date > ymd_hms("2020-03-25 17:00:00")) # remove "real" preview cases


if (min(psych::describe(survey_data, omit = FALSE)$min, na.rm = TRUE) < 0) {
  stop("Minimum value less than 0")
}


# manual corrections
# values enetred in 1000s
survey_data %>% 
  filter(self_income_18_19 > 0 & self_income_18_19 < 100) %>% 
  select(response_id, self_income_18_19)

survey_data <- 
  survey_data %>% 
  mutate(self_income_18_19 = modify_if(.x = .$self_income_18_19, 
                                       .p = inThousands, .f = ~ .x *1000),
         self_exp_income = modify_if(.x = .$self_exp_income, 
                                     .p = inThousands, .f = ~ .x *1000),
         self_exp_losses = modify_if(.x = .$self_exp_losses, 
                                    .p = inThousands, .f = ~ .x *1000),
         self_expenses_exp = modify_if(.x = .$self_expenses_exp, 
                                      .p = inThousands, .f = ~ .x *1000),
  ) 

survey_data %>% 
  filter(response_id == "R_1LFOvdFPZ1LMOis") %>% 
  .$self_income_18_19

# self_income_18_19 typos
survey_data %>% 
  filter(self_income_18_19 < self_exp_income & self_exp_income_change == "Less than") %>% 
  select(response_id, self_income_18_19, self_exp_income)

survey_data[survey_data$response_id == "R_paWW12a8TtIuGIN", "self_income_18_19"] <- 60000 # looks like a typo
survey_data[survey_data$response_id == "R_1pAJyquZqs0hLRV", "self_income_18_19"] <- 12000 # reported earning 120000 18/19 and to earn more 20/21 @ 14000


write_rds(survey_data, "data/prof_mtnrs_covid_19_survey.rds")

# checks
psych::describe(survey_data)

DataExplorer::plot_histogram(survey_data)



survey_data %>% 
  filter(`Employment status` == "Employee") %>% 
  select(employee_exp_income_change, ends_with("exp_income")) %>% 
  View

cases <- 
  survey_data %>% 
  filter(`Employment status` == "Employee") %>% 
  .$response_id

raw_survey_data %>% 
  filter(ResponseId %in% cases) %>% 
  select(Q46, Q54_1, Q56_1)

raw_survey_data