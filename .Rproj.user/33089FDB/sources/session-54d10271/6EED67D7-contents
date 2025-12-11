library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(finalfit)
library(here)

# Reading the data from the excel file with skippinng first row as it contains unneeded organization 
ISP_block_data <- read_excel(path = here("Project-1-data copy.xlsx"), skip = 1)

# Renaming columns to names shorter, representative and more easy to deal with 
ISP_block_data <- ISP_block_data %>% rename(time_of_peth_post_opt_min = `time of pethidine post operative (min)`,
                          first_dose_peth_post_opt_mg = `first dose of pethidine post operative (mg)`,
                          total_peth_post_opt_mg = `total of pethidine post operative (mg)`,
                          intra_opt_fentanyl_consump_mic = `intra oprative fentanyl consumption (mic)`)

# Removing extra spaces in front and back of the column values 
ISP_block_data$group <- ISP_block_data$group %>% str_trim()

# Removing text present with values of columns that should only include numeric data 
ISP_block_data$first_dose_peth_post_opt_mg <- ISP_block_data$first_dose_peth_post_opt_mg %>% str_remove("mg") %>% str_trim()
ISP_block_data$total_peth_post_opt_mg <- ISP_block_data$total_peth_post_opt_mg %>% str_remove("mg") %>% str_trim()
ISP_block_data$intra_opt_fentanyl_consump_mic <- ISP_block_data$intra_opt_fentanyl_consump_mic %>% str_remove("mic") %>% str_trim()

# Coding columns to be factors and setting up codes as needed
ISP_block_data$sex <- factor(ISP_block_data$sex) %>% fct_recode("Female" = "0", "Male" = "1")
ISP_block_data$group <- factor(ISP_block_data$group) %>% fct_recode(
  "Control group" = "control group", "ISSP Block group" = "ISSP block"
)

# Removing extra spaces in front and back of the column values 
ISP_block_data$group <- ISP_block_data$group %>% str_trim()

# Coding columns to be factors and setting up codes as needed

ISP_block_data <- ISP_block_data %>% mutate(across("patient satisification", ~ fct_recode(as.factor(.x),
                                                                        "satisfied" = "3",
                                                                        "fair" = "2", 
                                                                        "dissatisfied" = "1")))

ISP_block_data$`surgeon satisification` <- ISP_block_data$`surgeon satisification` %>% factor() %>% fct_recode(
  "very good" = "5",
  "good" = "4",
  "fair" = "3",
  "not bad" = "2",
  "dissatisfied" = "1"
)

ISP_block_data <- ISP_block_data %>% mutate(across(c("nausea", "vomiting", "pruritis", "others"), ~ factor(.x)))

# Removing extra spaces in all columns of VAS score and Extracting the important numbers we need as values for the column
ISP_block_data <- ISP_block_data %>% mutate(across(contains("VAS"), ~ as.numeric(parse_number(trimws(as.character(.x))))))

# Setting a condition with case_when function to unify the time values in time column
# Detection of wether word minute or hour are present and depending on that we extract the number 
# number extracted would be either converted to minutes or already in minutes time
ISP_block_data <- ISP_block_data %>% mutate(time_of_peth_post_opt_min = case_when(
  str_detect(time_of_peth_post_opt_min, "min") ~ parse_number(time_of_peth_post_opt_min),
  str_detect(time_of_peth_post_opt_min, "h") ~ parse_number(time_of_peth_post_opt_min) * 60,
  T ~ 0
))

# Converting values of columns to be numeric as needed 
ISP_block_data <- ISP_block_data %>% mutate(across(c("first_dose_peth_post_opt_mg", "total_peth_post_opt_mg", "intra_opt_fentanyl_consump_mic"), 
                                                   ~as.numeric(.x)))

# Taking a look at the data after finishing of cleaning & conversions 
str(ISP_block_data)
