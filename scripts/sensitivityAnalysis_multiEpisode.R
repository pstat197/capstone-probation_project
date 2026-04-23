library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(Amelia)
library(corrplot)
library(lubridate)
library(vip)
library(caret)
library(knitr)
library(kableExtra)
library(pROC)

# Convert everything to year, month, day and time

generic_time = "12:00:00"

inner_merged_yr2021_to_2025$B_EvtSupStDt <- mdy_hms(paste(inner_merged_yr2021_to_2025$B_EvtSupStDt, generic_time))
inner_merged_yr2021_to_2025$B_EvtSupEdDt <- mdy_hms(paste(inner_merged_yr2021_to_2025$B_EvtSupEdDt, generic_time))

inner_merged_yr2021_to_2025$Date.of.assessment <- mdy_hms(inner_merged_yr2021_to_2025$Date.of.assessment)
# Setup objects for subtraction

inner_merged_yr2021_to_2025$timeDifference = as.numeric(difftime(inner_merged_yr2021_to_2025$B_EvtSupStDt,inner_merged_yr2021_to_2025$Date.of.assessment, units = "days"))

# 30 Days - Keep every row where the assessment occurred within 30 days before the start of the supervision period

supervision30Days = inner_merged_yr2021_to_2025[inner_merged_yr2021_to_2025$timeDifference >= 0 
                                                             & inner_merged_yr2021_to_2025$timeDifference <= 30, ]
  
# 90 Days - Keep every row where the assessment occurred within 90 days before the start of the supervision period

supervision90Days = inner_merged_yr2021_to_2025[inner_merged_yr2021_to_2025$timeDifference >= 0 
                                                             & inner_merged_yr2021_to_2025$timeDifference <= 90, ]
# Remove additional episodes after the first episode for each subject and duplicates with less severe
# charges

supervision30Days = supervision30Days[-c(44,75,247,257,311,380,529),]

supervision90Days = supervision90Days[-c(9,44,55,75,82,245,247,257,311,351,380,529),]
# Validating Logistic Regression for 30 and 90 Days, single episode
set.seed(2017)

supervision30days_split = initial_split(supervision30Days, prop = 0.8)
# 90 Days
supervision90days_split = initial_split(supervision90Days, prop = 0.8)

# Training the Model

supervision30days_training = training(supervision30days_split)
supervision90days_training = training(supervision90days_split)

# 30 Days - NCA and FTA - Predictors are FTA Score, NCA Score, NVCA Score, and Days on Supervision
glm30fitNCA = glm(NCA_Charge ~ Scale.Term1 + Scale.Term2 + Scale.Term3
               + B_EvtDaysOnSup, data = supervision30days_training, family = "binomial")

glm30fitFTA = glm(FTA_Outcome ~ Scale.Term1 + Scale.Term2 + Scale.Term3
                  + B_EvtDaysOnSup, data = supervision30days_training, family = "binomial")

# 90 Days - NCA and FTA - Predictors are FTA Score, NCA Score, NVCA Score, and Days on Supervision
glm90fitNCA = glm(NCA_Charge ~ Scale.Term1 + Scale.Term2 + Scale.Term3
                  + B_EvtDaysOnSup, data = supervision90days_training, family = "binomial")

glm90fitFTA = glm(FTA_Outcome ~ Scale.Term1 + Scale.Term2 + Scale.Term3
                  + B_EvtDaysOnSup, data = supervision90days_training, family = "binomial")

# Extracting AUC values & confidence intervals for accounting only for days released
glm30fitFTA_auc = as.numeric(auc(roc(supervision30days_training$FTA_Outcome, glm30fitFTA$fitted.values, plot = FALSE)))
glm90fitFTA_auc = as.numeric(auc(roc(supervision90days_training$FTA_Outcome, glm90fitFTA$fitted.values, plot = FALSE))) 
glm30fitNCA_auc = as.numeric(auc(roc(supervision30days_training$NCA_Charge, glm30fitNCA$fitted.values, plot = FALSE)))
glm90fitNCA_auc = as.numeric(auc(roc(supervision90days_training$NCA_Charge, glm90fitNCA$fitted.values, plot = FALSE)))

glm30fitFTA_ci = as.numeric(ci(roc(supervision30days_training$FTA_Outcome, glm30fitFTA$fitted.values, plot = FALSE)))[c(1,3)]
glm90fitFTA_ci = as.numeric(ci(roc(supervision90days_training$FTA_Outcome, glm90fitFTA$fitted.values, plot = FALSE)))[c(1,3)]
glm30fitNCA_ci = as.numeric(ci(roc(supervision30days_training$NCA_Charge, glm30fitNCA$fitted.values, plot = FALSE)))[c(1,3)]
glm90fitNCA_ci = as.numeric(ci(roc(supervision90days_training$NCA_Charge, glm90fitNCA$fitted.values, plot = FALSE)))[c(1,3)]

timePeriods = c("30 Days", "90 Days", "30 Days", "90 Days")
riskPredictors = c("FTA", "FTA", "NCA","NCA")

auc_values_v1 = c(glm30fitFTA_auc, glm90fitFTA_auc, glm30fitNCA_auc, glm90fitNCA_auc)

conf_ints_v1 = c(paste0(glm30fitFTA_ci[1],",",glm30fitFTA_ci[2]), paste0(glm90fitFTA_ci[1],",",glm90fitFTA_ci[2]),
                 paste0(glm30fitNCA_ci[1],",",glm30fitNCA_ci[2]), paste0(glm90fitNCA_ci[1],",",glm90fitNCA_ci[2]))

glmTable = data.frame(riskPredictors, timePeriods, auc_values_v1, conf_ints_v1)

colnames(glmTable) = c("Outcome", "Time Period", "AUC", "95% Confidence Interval")

glmTable %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))

# 30 Days - NCA and FTA - Predictors are FTA Score, NCA Score, NVCA Score, Days on Supervision, 
# and Race


# Example if we're doing more than the given variable mentioned
#glm(NCA_Charge ~ Scale.Term1 + Scale.Term2 + Scale.Term3
  #  + B_EvtDaysOnSup + D_Race, data = supervision30Days, family = "binomial")

# Race + Risk Scores

glm30fitNCA_Race = glm(NCA_Charge ~ Scale.Term1 + Scale.Term2 
                       + Scale.Term3 + D_Race, data = supervision30days_training, family = "binomial")

glm30fitFTA_Race = glm(FTA_Outcome ~ Scale.Term1 + Scale.Term2
                       + Scale.Term3 + D_Race, data = supervision30days_training, family = "binomial")

# 90 Days - NCA and FTA - Predictors are FTA Score, NCA Score, NVCA Score, Days on Supervision, and Race
glm90fitNCA_Race = glm(NCA_Charge ~ Scale.Term1 + Scale.Term2
                       + Scale.Term3 + D_Race, data = supervision90days_training, family = "binomial")

glm90fitFTA_Race = glm(FTA_Outcome ~ Scale.Term1 + Scale.Term2
                       + Scale.Term3 + D_Race, data = supervision90days_training, family = "binomial")

# Extracting AUC values & confidence intervals for accounting for days released and race

supervision30days_training$fitted.fta = glm30fitFTA_Race$fitted.values
supervision30days_training$fitted.nca = glm30fitNCA_Race$fitted.values
supervision90days_training$fitted.fta = glm90fitFTA_Race$fitted.values
supervision90days_training$fitted.nca = glm90fitNCA_Race$fitted.values

# White FTA AUC Values
glm30fitFTA_auc_white = as.numeric(auc(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Race == "White"],
                                           supervision30days_training$fitted.fta[supervision30days_training$D_Race == "White"], plot = FALSE)))
glm90fitFTA_auc_white = as.numeric(auc(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Race == "White"],
                                           supervision90days_training$fitted.fta[supervision90days_training$D_Race == "White"], plot = FALSE)))
# White NCA AUC Values
glm30fitNCA_auc_white = as.numeric(auc(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Race == "White"],
                                           supervision30days_training$fitted.nca[supervision30days_training$D_Race == "White"], plot = FALSE)))

glm90fitNCA_auc_white = as.numeric(auc(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Race == "White"],
                                           supervision90days_training$fitted.nca[supervision90days_training$D_Race == "White"], plot = FALSE)))

# Hispanic FTA AUC Values
glm30fitFTA_auc_hispanic = as.numeric(auc(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Race == "Hispanic"],
                                           supervision30days_training$fitted.fta[supervision30days_training$D_Race == "Hispanic"], plot = FALSE)))
glm90fitFTA_auc_hispanic = as.numeric(auc(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Race == "Hispanic"],
                                           supervision90days_training$fitted.fta[supervision90days_training$D_Race == "Hispanic"], plot = FALSE)))
# Hispanic NCA AUC Values
glm30fitNCA_auc_hispanic = as.numeric(auc(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Race == "Hispanic"],
                                           supervision30days_training$fitted.nca[supervision30days_training$D_Race == "Hispanic"], plot = FALSE)))

glm90fitNCA_auc_hispanic = as.numeric(auc(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Race == "Hispanic"],
                                           supervision90days_training$fitted.nca[supervision90days_training$D_Race == "Hispanic"], plot = FALSE)))


# Vectors of AUC values
auc_values_white = c(glm30fitFTA_auc_white, glm90fitFTA_auc_white, glm30fitNCA_auc_white, glm90fitNCA_auc_white)
auc_values_hispanic = c(glm30fitFTA_auc_hispanic, glm90fitFTA_auc_hispanic, glm30fitNCA_auc_hispanic, glm90fitNCA_auc_hispanic)

# Confidence Intervals for AUC Values: White and Hispanic

glm30fitFTA_ci_white = as.numeric(ci(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Race == "White"],
                                         supervision30days_training$fitted.fta[supervision30days_training$D_Race == "White"], plot = FALSE)))[c(1,3)]
glm90fitFTA_ci_white = as.numeric(ci(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Race == "White"],
                                         supervision90days_training$fitted.fta[supervision90days_training$D_Race == "White"], plot = FALSE)))[c(1,3)]
glm30fitNCA_ci_white = as.numeric(ci(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Race == "White"],
                                         supervision30days_training$fitted.nca[supervision30days_training$D_Race == "White"], plot = FALSE)))[c(1,3)]
glm90fitNCA_ci_white = as.numeric(ci(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Race == "White"],
                                         supervision90days_training$fitted.nca[supervision90days_training$D_Race == "White"], plot = FALSE)))[c(1,3)]
# Hispanic Confidence Intervals

glm30fitFTA_ci_hispanic = as.numeric(ci(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Race == "Hispanic"],
                                         supervision30days_training$fitted.fta[supervision30days_training$D_Race == "Hispanic"], plot = FALSE)))[c(1,3)]
glm90fitFTA_ci_hispanic = as.numeric(ci(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Race == "Hispanic"],
                                         supervision90days_training$fitted.fta[supervision90days_training$D_Race == "Hispanic"], plot = FALSE)))[c(1,3)]
glm30fitNCA_ci_hispanic = as.numeric(ci(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Race == "Hispanic"],
                                         supervision30days_training$fitted.nca[supervision30days_training$D_Race == "Hispanic"], plot = FALSE)))[c(1,3)]
glm90fitNCA_ci_hispanic = as.numeric(ci(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Race == "Hispanic"],
                                         supervision90days_training$fitted.nca[supervision90days_training$D_Race == "Hispanic"], plot = FALSE)))[c(1,3)]

# Confidence Interval Vectors
conf_ints_white = c(paste0(glm30fitFTA_ci_white[1],",",glm30fitFTA_ci_white[2]), paste0(glm90fitFTA_ci_white[1],",",glm90fitFTA_ci_white[2]),
                 paste0(glm30fitNCA_ci_white[1],",",glm30fitNCA_ci_white[2]), paste0(glm90fitNCA_ci_white[1],",",glm90fitNCA_ci_white[2]))

conf_ints_hispanic = c(paste0(glm30fitFTA_ci_hispanic[1],",",glm30fitFTA_ci_hispanic[2]), paste0(glm90fitFTA_ci_hispanic[1],",",glm90fitFTA_ci_hispanic[2]),
                    paste0(glm30fitNCA_ci_hispanic[1],",",glm30fitNCA_ci_hispanic[2]), paste0(glm90fitNCA_ci_hispanic[1],",",glm90fitNCA_ci_hispanic[2]))

glmTablev2 = data.frame(riskPredictors, timePeriods, auc_values_white, auc_values_hispanic, conf_ints_white, conf_ints_hispanic)

colnames(glmTablev2) = c("Outcome", "Time Period", "AUC (White)", "AUC (Hispanic)",
                         "95% Confidence Interval (White)", "95% Confidence Interval (Hispanic)")

glmTablev2 %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))

# AUC by Gender

glm30fitNCA_Gender = glm(NCA_Charge ~ Scale.Term1 + Scale.Term2
                         + Scale.Term3 + D_Gender, data = supervision30days_training, family = "binomial")

glm30fitFTA_Gender = glm(FTA_Outcome ~ Scale.Term1 + Scale.Term2
                         + Scale.Term3 + D_Gender, data = supervision30days_training, family = "binomial")

# 90 Days - NCA and FTA - Predictors are FTA Score, NCA Score, NVCA Score, Days on Supervision, and Race
glm90fitNCA_Gender = glm(NCA_Charge ~ Scale.Term1 + Scale.Term2
                         + Scale.Term3 + D_Gender, data = supervision90days_training, family = "binomial")

glm90fitFTA_Gender = glm(FTA_Outcome ~ Scale.Term1 + Scale.Term2
                         + Scale.Term3 + D_Gender, data = supervision90days_training, family = "binomial")

# Extracting AUC values & confidence intervals for accounting for days released and race

supervision30days_training$fitted.fta.gender = glm30fitFTA_Gender$fitted.values
supervision30days_training$fitted.nca.gender = glm30fitNCA_Gender$fitted.values
supervision90days_training$fitted.fta.gender = glm90fitFTA_Gender$fitted.values
supervision90days_training$fitted.nca.gender = glm90fitNCA_Gender$fitted.values

# Men FTA AUC Values
glm30fitFTA_auc_men = as.numeric(auc(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Gender == "M"],
                                           supervision30days_training$fitted.fta[supervision30days_training$D_Gender == "M"], plot = FALSE)))
glm90fitFTA_auc_men = as.numeric(auc(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Gender == "M"],
                                           supervision90days_training$fitted.fta[supervision90days_training$D_Gender == "M"], plot = FALSE)))
# Men NCA AUC Values
glm30fitNCA_auc_men = as.numeric(auc(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Gender == "M"],
                                           supervision30days_training$fitted.nca[supervision30days_training$D_Gender == "M"], plot = FALSE)))

glm90fitNCA_auc_men = as.numeric(auc(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Gender == "M"],
                                           supervision90days_training$fitted.nca[supervision90days_training$D_Gender == "M"], plot = FALSE)))

# Women FTA AUC Values
glm30fitFTA_auc_women = as.numeric(auc(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Gender == "F"],
                                              supervision30days_training$fitted.fta[supervision30days_training$D_Gender == "F"], plot = FALSE)))
glm90fitFTA_auc_women = as.numeric(auc(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Gender == "F"],
                                              supervision90days_training$fitted.fta[supervision90days_training$D_Gender == "F"], plot = FALSE)))
# Women NCA AUC Values
glm30fitNCA_auc_women = as.numeric(auc(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Gender == "F"],
                                              supervision30days_training$fitted.nca[supervision30days_training$D_Gender == "F"], plot = FALSE)))

glm90fitNCA_auc_women = as.numeric(auc(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Gender == "F"],
                                              supervision90days_training$fitted.nca[supervision90days_training$D_Gender == "F"], plot = FALSE)))


# Vectors of AUC values
auc_values_men = c(glm30fitFTA_auc_men, glm90fitFTA_auc_men, glm30fitNCA_auc_men, glm90fitNCA_auc_men)
auc_values_women = c(glm30fitFTA_auc_women, glm90fitFTA_auc_women, glm30fitNCA_auc_women, glm90fitNCA_auc_women)

# Confidence Intervals for AUC Values: White and Hispanic

glm30fitFTA_ci_men = as.numeric(ci(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Gender == "M"],
                                         supervision30days_training$fitted.fta[supervision30days_training$D_Gender == "M"], plot = FALSE)))[c(1,3)]
glm90fitFTA_ci_men = as.numeric(ci(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Gender == "M"],
                                         supervision90days_training$fitted.fta[supervision90days_training$D_Gender == "M"], plot = FALSE)))[c(1,3)]
glm30fitNCA_ci_men = as.numeric(ci(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Gender == "M"],
                                         supervision30days_training$fitted.nca[supervision30days_training$D_Gender == "M"], plot = FALSE)))[c(1,3)]
glm90fitNCA_ci_men = as.numeric(ci(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Gender == "M"],
                                         supervision90days_training$fitted.nca[supervision90days_training$D_Gender == "M"], plot = FALSE)))[c(1,3)]
# Hispanic Confidence Intervals

glm30fitFTA_ci_women = as.numeric(ci(roc(supervision30days_training$FTA_Outcome[supervision30days_training$D_Gender == "F"],
                                            supervision30days_training$fitted.fta[supervision30days_training$D_Gender == "F"], plot = FALSE)))[c(1,3)]
glm90fitFTA_ci_women = as.numeric(ci(roc(supervision90days_training$FTA_Outcome[supervision90days_training$D_Gender == "F"],
                                            supervision90days_training$fitted.fta[supervision90days_training$D_Gender == "F"], plot = FALSE)))[c(1,3)]
glm30fitNCA_ci_women = as.numeric(ci(roc(supervision30days_training$NCA_Charge[supervision30days_training$D_Gender == "F"],
                                            supervision30days_training$fitted.nca[supervision30days_training$D_Gender == "F"], plot = FALSE)))[c(1,3)]
glm90fitNCA_ci_women = as.numeric(ci(roc(supervision90days_training$NCA_Charge[supervision90days_training$D_Gender == "F"],
                                            supervision90days_training$fitted.nca[supervision90days_training$D_Gender == "F"], plot = FALSE)))[c(1,3)]

# Confidence Interval Vectors
conf_ints_men = c(paste0(glm30fitFTA_ci_men[1],",",glm30fitFTA_ci_men[2]), paste0(glm90fitFTA_ci_men[1],",",glm90fitFTA_ci_men[2]),
                    paste0(glm30fitNCA_ci_men[1],",",glm30fitNCA_ci_men[2]), paste0(glm90fitNCA_ci_men[1],",",glm90fitNCA_ci_men[2]))

conf_ints_women = c(paste0(glm30fitFTA_ci_women[1],",",glm30fitFTA_ci_women[2]), paste0(glm90fitFTA_ci_women[1],",",glm90fitFTA_ci_women[2]),
                       paste0(glm30fitNCA_ci_women[1],",",glm30fitNCA_ci_women[2]), paste0(glm90fitNCA_ci_women[1],",",glm90fitNCA_ci_women[2]))

glmTablev3 = data.frame(riskPredictors, timePeriods, auc_values_men, auc_values_women, conf_ints_men, conf_ints_women)

colnames(glmTablev3) = c("Outcome", "Time Period", "AUC (Men)", "AUC (Women)",
                         "95% Confidence Interval (Men)", "95% Confidence Interval (Women)")

glmTablev3 %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))



