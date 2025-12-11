
# Running the code for the cleaned data to analyze 
source(file = here("Data_Upload_Clean.R"))

library(gtsummary)
library(broom)
library(magrittr)
library(epiR)
library(survival)
library(survminer)
library(finalfit)
library(flextable)

# Making a Summary Table for patients demographics in the study 
# Making flex table is better visualized when exporting your results on word document
ISP_block_data %>% select(Age:group) %>% tbl_summary(by = group,
                                                     label = list(
                                                       weight ~ "Weight(Kg)",
                                                     hight ~"Hight",
                                                     sex ~ "Sex"
                                                     )) %>% add_p() %>% as_flex_table()

# Testing Difference in Number of Patients who needed pethidine post operative between the two groups
# Using Chi-Square Test 
ISP_block_data %$% table(group, `need of pethidine post operative`)
ISP_block_data %$% table(group, `need of pethidine post operative`) %>% chisq.test() %>% tidy()

# Getting Risk Ratio for the outcome if exposure occured 
# Make sure to adjust the levels and mapping to be adjusted as what epi.2by2 function expects
ISP_block_data %$% table(factor(group, levels = c("ISSP Block group","Control group")), 
                         `need of pethidine post operative`) %>% 
  epi.2by2(method = "cohort.count", conf.level = 0.95)


# Making a summary table for hypothesize testing of different important values between intervention and control group
# Remove Zero Values from the columns as right now you're not testing the need of the medication,
# Instead you're testing doses on time difference upon those who needed the medication.
ISP_block_data %>% select(contains("peth"), group, intra_opt_fentanyl_consump_mic) %>% 
  filter(time_of_peth_post_opt_min != 0, first_dose_peth_post_opt_mg != 0,
         total_peth_post_opt_mg != 0, intra_opt_fentanyl_consump_mic != 0) %>% 
  tbl_summary(by = group,
              type = list(time_of_peth_post_opt_min ~ "continuous",
                          first_dose_peth_post_opt_mg ~ "continuous",
                          total_peth_post_opt_mg ~ "continuous",
                          intra_opt_fentanyl_consump_mic ~ "continuous"
              ),
              label = list('need of pethidine post operative'	~ "Needed of Pethidine Post Operative",
                           time_of_peth_post_opt_min	~ "Time of Pethidine Post Operative (min)",
                           first_dose_peth_post_opt_mg ~ "First Dose of Pethidine Post Operative (mg)",
                           total_peth_post_opt_mg ~ "Total of Pethidine Consumed Post Operative (mg)",
                           intra_opt_fentanyl_consump_mic ~ "Intraoperative Fentanyl Consumption (microgram)")) %>% 
  add_p() %>% as_flex_table()


# As the frequency of specific ADRs is very low, We could create a column of wether there is any ADR or not
ISP_block_data <- ISP_block_data %>% mutate(
  Any_ADR = factor(as.integer(nausea == 1 | vomiting == 1 | pruritis == 1),
                                                      levels = c(0, 1),
                                                      labels = c("No", "Yes")))

# Making a summary table for comparison of number of ADRs occurred between the two groups
# If the values are recorded as factors with levels of 0 and 1 
# Convert them to event type as you're looking for just occurrences, 
# This is done by specifiying the type and value needed in tbl_summary
ISP_block_data %>% select(nausea:pruritis, Any_ADR, group) %>% 
  tbl_summary(by = group, label = list( 
    nausea ~ "Nausea", vomiting ~ "Vomiting", pruritis ~ "Pruritis", Any_ADR ~ "Any ADRs"),
    type = list( nausea ~ "dichotomous", vomiting ~ "dichotomous", pruritis ~ "dichotomous",
                 Any_ADR ~ "dichotomous"),
    value = list(nausea ~ 1, vomiting ~ 1, pruritis ~ 1, Any_ADR ~ "Yes")) %>% 
  add_p() %>% as_flex_table()

# Comparing Satisfaction Rates of patients between the Two Groups 
ISP_block_data %>% select(group, `patient satisification`) %>% tbl_summary(by = group) %>% add_p()


# Replacing the zero values in time column in participants who didn't have the event to
# the last time of follow-up to construct proper survival function and kaplan meier curve 
max(ISP_block_data$time_of_peth_post_opt_min)
ISP_block_data <- ISP_block_data %>% mutate(time_of_peth_post_opt_min = ifelse(
  time_of_peth_post_opt_min == 0, 480, time_of_peth_post_opt_min
))
survival_object <- Surv(time = ISP_block_data$time_of_peth_post_opt_min, 
                        ISP_block_data$`need of pethidine post operative`)
surv_fit <- survfit(survival_object ~ group, data = ISP_block_data)

# Making Kaplan Meier Curve
ggsurvplot(surv_fit, data = ISP_block_data, conf.int = T, xlab = "Minutes",
           ylab = "Survival Probability", pval = T, pval.coord = c(210, 0.12))

# Making a summary table for comparing VAS scores between the two groups in different time points 
ISP_block_data %>% select(group, contains("VAS")) %>% 
  tbl_summary(by = group,
              type = list(contains("VAS") ~ "continuous")) %>% add_p() %>% 
  add_q(method = "bonferroni") %>% as_flex_table()


# Adjusting the format of the table, making long format for VAS scores so we could plot the data 
long_format <- ISP_block_data %>% select(ID, contains("VAS"), -`Pre opr VAS score`, group) %>% 
  pivot_longer(cols = matches("VAS"), names_to = "Time", values_to = "VAS") %>% 
  mutate(Time = factor(Time, levels = c("30 min post opt VAS", "1 h post opt VAS", "2h post opt VAS", "4 h post opt VAS",
                                        "6h post opt VAS", "8h post opt VAS", "12h post opt VAS", "18h post opt VAS",
                                        "24h post opt VAS", "36h post opt VAS", "48h post opt VAS"),
                       labels = c("30 min", "1h", "2h", "4h", "6h", "8h", "12h", "18h", "24h", "36h", "48h")))

# Bar Plot Comparing medians of VAS score between the two groups in different time points 
VAS_Summ <- long_format %>% group_by(group, Time) %>% summarise(Median = median(VAS), IQR = IQR(VAS))
VAS_Visual <- VAS_Summ %>% ggplot(aes(Time, Median, fill = group)) + geom_col(position = position_dodge(width = 0.5)) + 
  scale_fill_manual(values = c("#023E8A", "#00B4D8")) + theme_minimal()
VAS_Visual



   