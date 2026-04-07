library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(Amelia)
library(corrplot)
library(lubridate)
library(vip)
library(caret)
yr2021_22 = read.csv("APR TimePeriod 07-01-21 to 06-30-22_V2_final.csv")
yr2022_23 = read.csv("APR TimePeriod 07-01-22 to 06-30-23_V2_final.csv")
yr2023_24 = read.csv("APR TimePeriod 07-01-23 to 06-30-24_V2_final.csv")
yr2024_25 = read.csv("APR TimePeriod 07-01-24 to 06-30-25_V2_final.csv")
yr2025calendar = read.csv("APR TimePeriod 01-01-25 to 12-31-25_V2_final.csv")
totalyr_assessments = read.csv("SLO_Assessments_7-1-21 to 12-31-25.csv")

# Construct new column 'L_FTAwarRP' for 24/25 year and 2025 calendar year in order
# to prepare for merge

yr2024_25$"L_FTAwarCount" = yr2024_25$"L_FTAwarRP"
yr2025calendar$"L_FTAwarCount" = yr2025calendar$"L_FTAwarRP"


# Dropping old column 'L_FTAwarRP'

yr2024_25 = subset(yr2024_25, select = -c(L_FTAwarRP))
yr2025calendar = subset(yr2025calendar, select = -c(L_FTAwarRP))

# Merge data from years 2021 to 2025

yrList <- list(yr2021_22, yr2022_23, yr2023_24,
               yr2024_25, yr2025calendar)

merged_yr2021_to_2025 <- bind_rows(yrList)

# Modifies levels that have extra spaces to become levels without excess spaces
merged_yr2021_to_2025 <- merged_yr2021_to_2025 %>%
  mutate(A_EVLEVEL = case_when(
    A_EVLEVEL == "Medium " ~ "Medium",
    A_EVLEVEL == "High " ~ "High",
    A_EVLEVEL == "Low " ~ "Low",
    A_EVLEVEL == "" ~ NA,
    TRUE ~ A_EVLEVEL 
  ), D_FinalEmpStatus = case_when(
    D_FinalEmpStatus == "" ~ NA,
    TRUE ~ D_FinalEmpStatus),
  D_FinalHouseStatus = case_when(
    D_FinalHouseStatus == "" ~ NA,
    TRUE ~ D_FinalHouseStatus))


# Dropping entry 2215 in totalyr_assessments - no data present what so ever

totalyr_assessments <- totalyr_assessments[-2215,]

# Joining Data Frames

merged_yr2021_to_2025$ID = merged_yr2021_to_2025$A_MNID
inner_merged_yr2021_to_2025 = merge(merged_yr2021_to_2025, totalyr_assessments, 
                                    by = "ID")

# Column Modifications:

# Removing redundant columns:

inner_merged_yr2021_to_2025 = inner_merged_yr2021_to_2025 %>% select(-c("A_MNID", "Score1",
                                                                  "Score2", "Score3"))
# The score predictors will not work with logistic regression, as classifcation requires a categorical
# outcome variable. 
# nvca_assessmentScore is also the same as Scale.Term3, so it's not needed. 
# A_MNID is the same as ID, so it's not needed.

# Removing columns that were suggested to be removed by the mentor:

inner_merged_yr2021_to_2025 = inner_merged_yr2021_to_2025 %>% select(-c("A_EVTID", "D_FinalHouseStatus", "D_FinalEmpStatus", "officer",
                                                                  "C_EvtFinalResult"))

# Housing and Employment Status were suggested to be removed due to lack of data
# as seen through prior graphs.
# EVTID was suggested to be removed due to sorting being done through MNID instead.
# Officer was removed as inter rater reliability was not effective since there are 16
# officers scoring hundreds of people, so it's difficult to get enough data to reliably compare
# C_EvtFinalResult often does not involve the actual scores from the assessments
# the inter-rater reliability rates


# Checking Missing Data

sum(is.na(inner_merged_yr2021_to_2025$L_FTAwarCount))

# Most of the L_FTAwarRP column, which counts the number of 
#times the subject failed to appear in court 
#between the supervision start and end dates is empty.

# These will be imputed with 0s. 

inner_merged_yr2021_to_2025$L_FTAwarCount[is.na(inner_merged_yr2021_to_2025$L_FTAwarCount)] <- 0

# There are less than 10 entries with a missing EV level; 
# this is less than 1% and will be converted to 'NoAssmt'

inner_merged_yr2021_to_2025$A_EVLEVEL[is.na(inner_merged_yr2021_to_2025$A_EVLEVEL)] <- "NoAssmt"
# The start and end dates are currently in char format, 
# so they will be converted to numeric/date format

# Outcome Variable Modification: Logistic Regression requires two outcomes of a factor variable
# but the NCA and FTA variables have 6, so they will be mutated to only have two outcomes

inner_merged_yr2021_to_2025 = inner_merged_yr2021_to_2025 %>% 
  mutate(Scale.Term1 = case_when(
    Scale.Term1 == "FTA-1" ~ "NoFailuretoAppear",
    Scale.Term1 == "FTA-2" ~ "NoFailuretoAppear",
    Scale.Term1 == "FTA-3" ~ "NoFailuretoAppear",
    Scale.Term1 == "FTA-4" ~ "FailuretoAppear",
    Scale.Term1 == "FTA-5" ~ "FailuretoAppear",
    Scale.Term1 == "FTA-6" ~ "FailuretoAppear"
  ),
  Scale.Term2 = case_when(
    Scale.Term2 == "NCA-1" ~ "NoNewCriminalArrest",
    Scale.Term2 == "NCA-2" ~ "NoNewCriminalArrest",
    Scale.Term2 == "NCA-3" ~ "NoNewCriminalArrest",
    Scale.Term2 == "NCA-4" ~ "NewCriminalArrest",
    Scale.Term2 == "NCA-5" ~ "NewCriminalArrest",
    Scale.Term2 == "NCA-6" ~ "NewCriminalArrest"
  ))

inner_merged_yr2021_to_2025$Scale.Term1 = factor(inner_merged_yr2021_to_2025$Scale.Term1)

inner_merged_yr2021_to_2025$Scale.Term2 = factor(inner_merged_yr2021_to_2025$Scale.Term2)


# Updated Visualizations

## Bar Charts for EVLevel, Gender, Race, Employment & Housing Status

ggplot(supervision90days_1st_ep, aes(A_EVLEVEL)) + geom_bar() + 
  ggtitle(label = "Frequency of Different Levels of Probationary Supervision") +
  xlab(label = "Level of Probationary Supervision") + theme_minimal()

ggplot(supervision90days_1st_ep, aes(D_Gender, fill = D_Gender)) + geom_bar() + 
  ggtitle(label = "Gender Gap in Probationary Supervision") + 
  xlab(label = "Gender") + theme_minimal() + 
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female"))

ggplot(supervision90days_1st_ep, aes(D_FinalHouseStatus)) + geom_bar() + 
  ggtitle(label = "Housing Status of Probationers") + 
  xlab(label = "Status") + theme_minimal()

ggplot(supervision90days_1st_ep, aes(D_FinalEmpStatus)) + geom_bar() + 
  ggtitle(label = "Employment Status of Probationers") + 
  xlab(label = "Status") + theme_minimal()

ggplot(supervision90days_1st_ep, aes(D_Race)) + geom_bar() + 
  ggtitle(label = "Racial Representation in Probationary Supervision") + 
  xlab(label = "Race") + theme_minimal()

## Histograms for Age & Days in Jail during Supervision

ggplot(supervision90days_1st_ep, aes(D_AgeAtSup)) + geom_histogram(binwidth = 2, fill = "#FFA500") + 
  ggtitle(label = "Distribution of Age at the start of Probationary Supervision") + 
  xlab(label = "Age") + scale_x_continuous(breaks = scales::breaks_width(5)) +
  theme_minimal() + 
  geom_vline(aes(xintercept = mean(D_AgeAtSup)), color = "cyan", 
             linetype = "dashed", size = 1)

ggplot(supervision90days_1st_ep, aes(B_EvtDaysOnSup)) + geom_histogram(binwidth = 2, fill = "#FFA500") + 
  ggtitle(label = "Distribution of Days on Probationary Supervision") + 
  xlab(label = "Days") + scale_x_continuous(breaks = scales::breaks_width(50)) +
  theme_minimal() + 
  geom_vline(aes(xintercept = mean(B_EvtDaysOnSup)), color = "cyan", 
             linetype = "dashed", size = 1)

ggplot(supervision90days_1st_ep, aes(E_DaysInJailDuringSup)) + geom_histogram(binwidth = 5, fill = "#00FFFF") + 
  ggtitle(label = "Distribution of Days in Jail during Supervision") + xlim(0, 1000) +
  xlab(label = "Days") + ylim(0,40) + 
  scale_x_continuous(breaks = scales::breaks_width(50)) + 
  geom_vline(aes(xintercept = mean(E_DaysInJailDuringSup)), 
             color = "red", linetype = "dashed", size = 0.5) + theme_minimal()

# SLO FTA Risk Score

ggplot(supervision90days_1st_ep, aes(x=Score1,y=after_stat(count) / sum(after_stat(count)))) + geom_bar(fill = "blue") + geom_text(
  aes(label = scales::percent(after_stat(count) / sum(after_stat(count)))),
  stat = "count",    
  vjust = -0.5,      
  size = 2           
) +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Percentage Frequency of FTA Risk Scores",
    x = "Level of FTA Risk",
    y = "Percentage (%)"
  ) +  scale_x_continuous(breaks = seq(1,6,1)) + theme_minimal()

# SLO NCA Risk Score

ggplot(supervision90days_1st_ep, aes(x=Score2,y=after_stat(count) / sum(after_stat(count)))) + geom_bar(fill = "blue") + geom_text(
  aes(label = scales::percent(after_stat(count) / sum(after_stat(count)))),
  stat = "count",    
  vjust = -0.5,       
  size = 2          
) +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Percentage Frequency of NCA Risk Scores",
    x = "Level of NCA Risk",
    y = "Percentage (%)"
  ) +  scale_x_continuous(breaks = seq(1,6,1)) + theme_minimal()


# SLO NVCA Risk Score

supervision90days_1st_ep <- supervision90days_1st_ep %>% 
  mutate(nvca_assessmentScore = case_when(Score3 == 1 ~ "Yes", 
                                          Score3 == 0 ~ "No"))

ggplot(supervision90days_1st_ep, aes(x=nvca_assessmentScore, y=after_stat(count) / sum(after_stat(count)))) + geom_bar(fill = "blue")+ geom_text(
  aes(label = scales::percent(after_stat(count) / sum(after_stat(count)))),
  stat = "count",    
  vjust = -0.5,       
  size = 2         
) +
  scale_y_continuous(labels = scales::percent) + # Format the Y-axis as percentages
  labs(
    title = "Percentage Frequency of NVCA Risk Scores",
    x = "Indication of NVCA",
    y = "Percentage (%)"
  ) +  theme_minimal()

# Logistic Regression 1.0

#library(twinning)

# Keeping consistent results
set.seed(2017)


# Setting up partitions to stratify against NCA and FTA
fta_partition = supervision90days_1st_ep %>% 
  initial_split(0.8, strata = Scale.Term1) # Predicting FTA 

nca_partition = supervision90days_1st_ep %>% 
  initial_split(0.8, strata = Scale.Term2) # Predicting NCA

# Building training and testing for FTA and NCA
fta_training = training(fta_partition)
fta_testing = testing(fta_partition)

nca_training = training(nca_partition)
nca_testing = testing(nca_partition)

# Checking for variables that are too correlated
fta_training %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(type = 'lower', diag = FALSE, method = 'color')

nca_training %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(type = 'lower', diag = FALSE, method = 'color')

# Building the Recipes


fta_recipe <- recipe(Scale.Term1 ~ A_CaseType + A_EVTRACK + A_EVLEVEL + B_EvtSupStDt
                     + B_EvtSupEdDt + B_EvtDaysOnSup + C_NewChargeOnSupYN + D_Gender
                     + D_AgeAtSup + E_DaysInJailDuringSup + L_FTAwarRP, data= fta_training) %>%
  step_dummy(all_nominal_predictors())
#step_interact(terms = ~ starts_with("sex"):fare) %>%
#step_interact(terms = ~ D_Gender:L_FTAwarRP + D_Gender:E_DaysInJailDuringSup)

nca_recipe <- recipe(Scale.Term2 ~ A_CaseType + A_EVTRACK + A_EVLEVEL + B_EvtSupStDt
                     + B_EvtSupEdDt + B_EvtDaysOnSup + C_NewChargeOnSupYN + D_Gender
                     + D_AgeAtSup + E_DaysInJailDuringSup + L_FTAwarRP, data= nca_training) %>%
  step_dummy(all_nominal_predictors())

# Fitting the Models
log_reg <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

fta_logFlow <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(fta_recipe)

nca_logFlow <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(nca_recipe)

fta_logFit <- fit(fta_logFlow, fta_training)

nca_logFit <- fit(nca_logFlow, nca_training)

# Predicting FTA and NCA outcomes on training data

nca_log_predictions <- predict(nca_logFit, new_data = nca_training, type = "prob")
nca_log_predictions <- bind_cols(nca_log_predictions, nca_training)
roc_auc(nca_log_predictions, truth = Scale.Term2, .pred_NewCriminalArrest)

fta_log_predictions <- predict(fta_logFit, new_data = fta_training, type = "prob")
fta_log_predictions <- bind_cols(fta_log_predictions, fta_training)
roc_auc(fta_log_predictions, truth = Scale.Term1, .pred_FailuretoAppear)

# Predicting FTA and NCA outcomes on testing data

fta_logistic_test_results <- augment(fta_logFit, new_data = fta_testing)
roc_auc(fta_logistic_test_results, truth = Scale.Term1, .pred_FailuretoAppear)

nca_logistic_test_results <- augment(nca_logFit, new_data = nca_testing)
roc_auc(nca_logistic_test_results, truth = Scale.Term2, .pred_NewCriminalArrest)

# Confidence Matrices

conf_mat(fta_logistic_test_results, truth = Scale.Term1, estimate = .pred_class)
conf_mat(nca_logistic_test_results, truth = Scale.Term2, estimate = .pred_class)

# ROC Curves

augment(fta_logFit, new_data = fta_testing) %>%
  roc_curve(truth = Scale.Term1, .pred_FailuretoAppear) %>%
  autoplot()

augment(nca_logFit, new_data = nca_testing) %>%
  roc_curve(truth = Scale.Term2, .pred_NewCriminalArrest) %>%
  autoplot()



