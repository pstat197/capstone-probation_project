library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(Amelia)
library(corrplot)
library(lubridate)
library(vip)
library(caret)

# Convert everything to year, month, day and time

generic_time = "12:00:00"

inner_merged_yr2021_to_2025$B_EvtSupStDt <- mdy_hms(paste(inner_merged_yr2021_to_2025$B_EvtSupStDt, generic_time))
inner_merged_yr2021_to_2025$B_EvtSupEdDt <- mdy_hms(paste(inner_merged_yr2021_to_2025$B_EvtSupEdDt, generic_time))

inner_merged_yr2021_to_2025$Date.of.assessment <- mdy_hms(inner_merged_yr2021_to_2025$Date.of.assessment)
# Setup objects for subtraction

inner_merged_yr2021_to_2025$timeDifference = as.numeric(difftime(inner_merged_yr2021_to_2025$B_EvtSupStDt,inner_merged_yr2021_to_2025$Date.of.assessment, units = "days"))

# 30 Days - Keep every row where the assessment occurred within 30 days before the start of the supervision period

supervision30Days_multiEpisode = inner_merged_yr2021_to_2025[inner_merged_yr2021_to_2025$timeDifference >= 0 
                                                             & inner_merged_yr2021_to_2025$timeDifference <= 30, ]
  
# 90 Days - Keep every row where the assessment occurred within 90 days before the start of the supervision period

supervision90Days_multiEpisode = inner_merged_yr2021_to_2025[inner_merged_yr2021_to_2025$timeDifference >= 0 
                                                             & inner_merged_yr2021_to_2025$timeDifference <= 90, ]

# Logistic Regression for 30 and 90 Days, multi-episode
set.seed(2017)


# Setting up partitions to stratify against NCA and FTA
fta_partition90 = supervision90Days_multiEpisode %>% 
  initial_split(0.8, strata = Scale.Term1) # Predicting FTA 

nca_partition90 = supervision90Days_multiEpisode %>% 
  initial_split(0.8, strata = Scale.Term2) # Predicting NCA

fta_partition30 = supervision30Days_multiEpisode %>% 
  initial_split(0.8, strata = Scale.Term1) # Predicting FTA 

nca_partition30 = supervision30Days_multiEpisode %>% 
  initial_split(0.8, strata = Scale.Term2) # Predicting NCA

# Building training and testing for FTA and NCA
fta_training90 = training(fta_partition90)
fta_testing90 = testing(fta_partition90)

nca_training90 = training(nca_partition90)
nca_testing90 = testing(nca_partition90)

fta_training30 = training(fta_partition30)
fta_testing30 = testing(fta_partition30)

nca_training30 = training(nca_partition30)
nca_testing30 = testing(nca_partition30)

# Checking for variables that are too correlated
fta_training30 %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(type = 'lower', diag = FALSE, method = 'color')

nca_training30 %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(type = 'lower', diag = FALSE, method = 'color')

fta_training90 %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(type = 'lower', diag = FALSE, method = 'color')

nca_training90 %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(type = 'lower', diag = FALSE, method = 'color')

# Building the Recipes


fta_recipe30 <- recipe(Scale.Term1 ~ A_CaseType + A_EVTRACK + A_EVLEVEL + B_EvtSupStDt
                     + B_EvtSupEdDt + B_EvtDaysOnSup + C_NewChargeOnSupYN + D_Gender
                     + D_AgeAtSup + E_DaysInJailDuringSup + L_FTAwarCount, data= fta_training30) %>%
  step_dummy(all_nominal_predictors())

fta_recipe90 <- recipe(Scale.Term1 ~ A_CaseType + A_EVTRACK + A_EVLEVEL + B_EvtSupStDt
                       + B_EvtSupEdDt + B_EvtDaysOnSup + C_NewChargeOnSupYN + D_Gender
                       + D_AgeAtSup + E_DaysInJailDuringSup + L_FTAwarCount, data= fta_training90) %>%
  step_dummy(all_nominal_predictors())
#step_interact(terms = ~ starts_with("sex"):fare) %>%
#step_interact(terms = ~ D_Gender:L_FTAwarRP + D_Gender:E_DaysInJailDuringSup)

nca_recipe30 <- recipe(Scale.Term2 ~ A_CaseType + A_EVTRACK + A_EVLEVEL + B_EvtSupStDt
                     + B_EvtSupEdDt + B_EvtDaysOnSup + C_NewChargeOnSupYN + D_Gender
                     + D_AgeAtSup + E_DaysInJailDuringSup + L_FTAwarCount, data= nca_training30) %>%
  step_dummy(all_nominal_predictors())

nca_recipe90 <- recipe(Scale.Term2 ~ A_CaseType + A_EVTRACK + A_EVLEVEL + B_EvtSupStDt
                       + B_EvtSupEdDt + B_EvtDaysOnSup + C_NewChargeOnSupYN + D_Gender
                       + D_AgeAtSup + E_DaysInJailDuringSup + L_FTAwarCount, data= nca_training90) %>%
  step_dummy(all_nominal_predictors())

# Fitting the Models
log_reg <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

fta_logFlow30 <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(fta_recipe30)

fta_logFlow90 <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(fta_recipe90)

nca_logFlow30 <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(nca_recipe30)

nca_logFlow90 <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(nca_recipe90)

fta_logFit30 <- fit(fta_logFlow30, fta_training30)

fta_logFit90 <- fit(fta_logFlow90, fta_training90)

nca_logFit30 <- fit(nca_logFlow30, nca_training30)

nca_logFit90 <- fit(nca_logFlow90, nca_training90)

# Predicting FTA and NCA outcomes on training data

nca_log_predictions30 <- predict(nca_logFit30, new_data = nca_training30, type = "prob")
nca_log_predictions30 <- bind_cols(nca_log_predictions30, nca_training30)
roc_auc(nca_log_predictions30, truth = Scale.Term2, .pred_NewCriminalArrest)

nca_log_predictions90 <- predict(nca_logFit90, new_data = nca_training90, type = "prob")
nca_log_predictions90 <- bind_cols(nca_log_predictions90, nca_training90)
roc_auc(nca_log_predictions90, truth = Scale.Term2, .pred_NewCriminalArrest)

fta_log_predictions30 <- predict(fta_logFit30, new_data = fta_training30, type = "prob")
fta_log_predictions30 <- bind_cols(fta_log_predictions30, fta_training30)
roc_auc(fta_log_predictions30, truth = Scale.Term1, .pred_FailuretoAppear)

fta_log_predictions90 <- predict(fta_logFit90, new_data = fta_training90, type = "prob")
fta_log_predictions90 <- bind_cols(fta_log_predictions90, fta_training90)
roc_auc(fta_log_predictions90, truth = Scale.Term1, .pred_FailuretoAppear)

# Predicting FTA and NCA outcomes on testing data

fta_logistic_test_results30 <- augment(fta_logFit30, new_data = fta_testing30)
roc_auc(fta_logistic_test_results30, truth = Scale.Term1, .pred_FailuretoAppear)

fta_logistic_test_results90 <- augment(fta_logFit90, new_data = fta_testing90)
roc_auc(fta_logistic_test_results90, truth = Scale.Term1, .pred_FailuretoAppear)

nca_logistic_test_results30 <- augment(nca_logFit30, new_data = nca_testing30)
roc_auc(nca_logistic_test_results30, truth = Scale.Term2, .pred_NewCriminalArrest)

nca_logistic_test_results90 <- augment(nca_logFit90, new_data = nca_testing90)
roc_auc(nca_logistic_test_results90, truth = Scale.Term2, .pred_NewCriminalArrest)

# Confidence Matrices

conf_mat(fta_logistic_test_results, truth = Scale.Term1, estimate = .pred_class)
conf_mat(nca_logistic_test_results, truth = Scale.Term2, estimate = .pred_class)

# ROC Curves

augment(fta_logFit30, new_data = fta_testing30) %>%
  roc_curve(truth = Scale.Term1, .pred_FailuretoAppear) %>%
  autoplot()

augment(fta_logFit90, new_data = fta_testing90) %>%
  roc_curve(truth = Scale.Term1, .pred_FailuretoAppear) %>%
  autoplot()

augment(nca_logFit30, new_data = nca_testing30) %>%
  roc_curve(truth = Scale.Term2, .pred_NewCriminalArrest) %>%
  autoplot()

augment(nca_logFit90, new_data = nca_testing90) %>%
  roc_curve(truth = Scale.Term2, .pred_NewCriminalArrest) %>%
  autoplot()
  