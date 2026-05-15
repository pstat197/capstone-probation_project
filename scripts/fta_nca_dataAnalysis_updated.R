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
library(modelr)
library(yardstick)

## Preprocessing and Cleaning
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


## Assembling the main dataset
# Joining Data Frames

merged_yr2021_to_2025$ID = merged_yr2021_to_2025$A_MNID
inner_merged_yr2021_to_2025 = merge(merged_yr2021_to_2025, totalyr_assessments, 
                                    by = "ID")

# Column Modifications:

# Removing redundant columns:

inner_merged_yr2021_to_2025 = inner_merged_yr2021_to_2025 %>% select(-c("A_MNID", "Scale.Term1",
                                                                        "Scale.Term2", "Scale.Term3"))

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

## Construct outcome variables for FTA, NCA, and NVCA

# New FTA Outcome based on prior warrants
inner_merged_yr2021_to_2025$FTA_Outcome = ifelse(inner_merged_yr2021_to_2025$L_FTAwarCount > 0, 1, 0)

# New Charge/NCA outcome will just be C_NewChargeOnSupYN but with 1s and 0s

inner_merged_yr2021_to_2025$NCA_Charge = ifelse(inner_merged_yr2021_to_2025$C_NewChargeOnSupYN == "Yes", 1, 0)

inner_merged_yr2021_to_2025$FTA_Outcome = factor(inner_merged_yr2021_to_2025$FTA_Outcome)
inner_merged_yr2021_to_2025$NCA_Charge = factor(inner_merged_yr2021_to_2025$NCA_Charge)

inner_merged_yr2021_to_2025$D_Race = factor(inner_merged_yr2021_to_2025$D_Race)
inner_merged_yr2021_to_2025$D_Gender = factor(inner_merged_yr2021_to_2025$D_Gender)


## Analysis Section:

# The start and end dates are currently in char format, 
# so they will be converted to numeric/date format

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
## Validating Logistic Regression for 30 and 90 Days, single episode
set.seed(2017)

supervision30days_split = initial_split(supervision30Days, prop = 0.8)
# 90 Days
supervision90days_split = initial_split(supervision90Days, prop = 0.8)

# Training the Model

supervision30days_training = training(supervision30days_split)
supervision90days_training = training(supervision90days_split)

# 30 Days - NCA and FTA - Predictors are just the FTA and NCA evaluation scores

glm30fitNCA = glm(NCA_Charge ~ Score2, data = supervision30days_training, family = "binomial")

glm30fitFTA = glm(FTA_Outcome ~ Score1, data = supervision30days_training, family = "binomial")

# 90 Days - NCA and FTA - Predictors are just the FTA and NCA evaluation scores
glm90fitNCA = glm(NCA_Charge ~ Score2, data = supervision90days_training, family = "binomial")

glm90fitFTA = glm(FTA_Outcome ~ Score1, data = supervision90days_training, family = "binomial")


# Testing Sets
supervision30days_testing = testing(supervision30days_split)
supervision90days_testing = testing(supervision90days_split)

## Predictions and Results

# FTA
supervision30days_testing$predictions.fta <- predict(glm30fitFTA, 
                                                          newdata = supervision30days_testing,
                                                          type = "response")
supervision90days_testing$predictions.fta <- predict(glm90fitFTA, 
                                                          newdata = supervision90days_testing,
                                                          type = "response")
# NCA
supervision30days_testing$predictions.nca<- predict(glm30fitNCA, 
                                                          newdata = supervision30days_testing,
                                                          type = "response")
supervision90days_testing$predictions.nca <- predict(glm90fitNCA, 
                                                          newdata = supervision90days_testing,
                                                          type = "response")

## Building table to display results

# Defactoring variables to create ROC objects

## FTA
supervision30days_testing$FTA_Outcome = as.numeric(supervision30days_testing$FTA_Outcome) - 1
supervision90days_testing$FTA_Outcome = as.numeric(supervision90days_testing$FTA_Outcome) - 1

## NCA
supervision30days_testing$NCA_Charge = as.numeric(supervision30days_testing$NCA_Charge) - 1
supervision90days_testing$NCA_Charge = as.numeric(supervision90days_testing$NCA_Charge) - 1

# FTA
fta_30d_white <- roc(response = supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Race == "White"],
                       predictor = supervision30days_testing$predictions.fta[supervision30days_testing$D_Race == "White"], levels = c(0,1),
                       direction = "<"
)

fta_90d_white <- roc(response = supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Race == "White"],
                     predictor = supervision90days_testing$predictions.fta[supervision90days_testing$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

fta_30d_hispanic <- roc(response = supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Race == "Hispanic"],
                     predictor = supervision30days_testing$predictions.fta[supervision30days_testing$D_Race == "Hispanic"], levels = c(0,1),
                     direction = "<"
)

fta_90d_hispanic <- roc(response = supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Race == "Hispanic"],
                     predictor = supervision90days_testing$predictions.fta[supervision90days_testing$D_Race == "Hispanic"], levels = c(0,1),
                     direction = "<"
)

fta_30d_men <- roc(response = supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Gender == "M"],
                     predictor = supervision30days_testing$predictions.fta[supervision30days_testing$D_Gender == "M"], levels = c(0,1),
                     direction = "<"
)

fta_90d_men <- roc(response = supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Gender == "M"],
                     predictor = supervision90days_testing$predictions.fta[supervision90days_testing$D_Gender == "M"], levels = c(0,1),
                     direction = "<"
)

fta_30d_women <- roc(response = supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Gender == "F"],
                     predictor = supervision30days_testing$predictions.fta[supervision30days_testing$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

fta_90d_women <- roc(response = supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Gender == "F"],
                     predictor = supervision90days_testing$predictions.fta[supervision90days_testing$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)


# NCA

nca_30d_white <- roc(response = supervision30days_testing$NCA_Charge[supervision30days_testing$D_Race == "White"],
                     predictor = supervision30days_testing$predictions.nca[supervision30days_testing$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

nca_90d_white <- roc(response = supervision90days_testing$NCA_Charge[supervision90days_testing$D_Race == "White"],
                     predictor = supervision90days_testing$predictions.nca[supervision90days_testing$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

nca_30d_hispanic <- roc(response = supervision30days_testing$NCA_Charge[supervision30days_testing$D_Race == "Hispanic"],
                        predictor = supervision30days_testing$predictions.nca[supervision30days_testing$D_Race == "Hispanic"], levels = c(0,1),
                        direction = "<"
)

nca_90d_hispanic <- roc(response = supervision90days_testing$NCA_Charge[supervision90days_testing$D_Race == "Hispanic"],
                        predictor = supervision90days_testing$predictions.nca[supervision90days_testing$D_Race == "Hispanic"], levels = c(0,1),
                        direction = "<"
)

nca_30d_men <- roc(response = supervision30days_testing$NCA_Charge[supervision30days_testing$D_Gender == "M"],
                   predictor = supervision30days_testing$predictions.nca[supervision30days_testing$D_Gender == "M"], levels = c(0,1),
                   direction = "<"
)

nca_90d_men <- roc(response = supervision90days_testing$NCA_Charge[supervision90days_testing$D_Gender == "M"],
                   predictor = supervision90days_testing$predictions.nca[supervision90days_testing$D_Gender == "M"], levels = c(0,1),
                   direction = "<"
)

nca_30d_women <- roc(response = supervision30days_testing$NCA_Charge[supervision30days_testing$D_Gender == "F"],
                     predictor = supervision30days_testing$predictions.nca[supervision30days_testing$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

nca_90d_women <- roc(response = supervision90days_testing$NCA_Charge[supervision90days_testing$D_Gender == "F"],
                     predictor = supervision90days_testing$predictions.nca[supervision90days_testing$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

fta_nca_table <- tibble(Outcome = c("FTA", "FTA", "FTA", "FTA", "NCA", "NCA", "NCA", "NCA"),
  Demographic = c("Men", "Women", "White", "Hispanic", "Men", "Women", "White", "Hispanic"),
  One_Month = c(as.numeric(auc(fta_30d_men)),
          as.numeric(auc(fta_30d_women)),
          as.numeric(auc(fta_30d_white)),
          as.numeric(auc(fta_30d_hispanic)),
          as.numeric(auc(nca_30d_men)),
          as.numeric(auc(nca_30d_women)),
          as.numeric(auc(nca_30d_white)),
          as.numeric(auc(nca_30d_hispanic))
  ),
  Three_Months = c(as.numeric(auc(fta_90d_men)),
                   as.numeric(auc(fta_90d_women)),
                   as.numeric(auc(fta_90d_white)),
                   as.numeric(auc(fta_90d_hispanic)),
                   as.numeric(auc(nca_90d_men)),
                   as.numeric(auc(nca_90d_women)),
                   as.numeric(auc(nca_90d_white)),
                   as.numeric(auc(nca_90d_hispanic))
  ))

fta_nca_cleanTable = fta_nca_table %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))

fta_nca_cleanTable

## Statistical Significance of Electronic Monitoring

# 30 Days - NCA and FTA - Predictors are just the FTA and NCA evaluation scores and electronic monitoring

glm30fitNCA_em = glm(NCA_Charge ~ Score2 + A_EVTRACK, data = supervision30days_training, family = "binomial")

glm30fitFTA_em = glm(FTA_Outcome ~ Score1 + A_EVTRACK, data = supervision30days_training, family = "binomial")

# 90 Days - NCA and FTA - Predictors are just the FTA and NCA evaluation scores and electronic monitoring
glm90fitNCA_em = glm(NCA_Charge ~ Score2 + A_EVTRACK, data = supervision90days_training, family = "binomial")

glm90fitFTA_em = glm(FTA_Outcome ~ Score1 + A_EVTRACK, data = supervision90days_training, family = "binomial")

# FTA
supervision30days_testing$predictions.fta_em <- predict(glm30fitFTA_em, 
                                                     newdata = supervision30days_testing,
                                                     type = "response")
supervision90days_testing$predictions.fta_em <- predict(glm90fitFTA_em, 
                                                     newdata = supervision90days_testing,
                                                     type = "response")
# NCA
supervision30days_testing$predictions.nca_em<- predict(glm30fitNCA_em, 
                                                    newdata = supervision30days_testing,
                                                    type = "response")
supervision90days_testing$predictions.nca_em <- predict(glm90fitNCA_em, 
                                                     newdata = supervision90days_testing,
                                                     type = "response")


## ROC Objects - No GLM

## FTA
supervision30Days$FTA_Outcome = as.numeric(supervision30Days$FTA_Outcome) - 1
supervision90Days$FTA_Outcome = as.numeric(supervision90Days$FTA_Outcome) - 1

## NCA
supervision30Days$NCA_Charge = as.numeric(supervision30Days$NCA_Charge) - 1
supervision90Days$NCA_Charge = as.numeric(supervision90Days$NCA_Charge) - 1

fta_30d_white_noGLM <- roc(response = supervision30Days$FTA_Outcome[supervision30Days$D_Race == "White"],
                     predictor = supervision30Days$Score1[supervision30Days$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

fta_30d_hispanic_noGLM <- roc(response = supervision30Days$FTA_Outcome[supervision30Days$D_Race == "Hispanic"],
                      predictor = supervision30Days$Score1[supervision30Days$D_Race == "Hispanic"], levels = c(0,1),
                      direction = "<"
)

fta_30d_women_noGLM <- roc(response = supervision30Days$FTA_Outcome[supervision30Days$D_Gender == "F"],
                     predictor = supervision30Days$Score1[supervision30Days$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

fta_30d_men_noGLM <- roc(response = supervision30Days$FTA_Outcome[supervision30Days$D_Gender == "M"],
                     predictor = supervision30Days$Score1[supervision30Days$D_Gender == "M"], levels = c(0,1),
                     direction = "<"
)

fta_90d_white_noGLM <- roc(response = supervision90Days$FTA_Outcome[supervision90Days$D_Race == "White"],
                     predictor = supervision90Days$Score1[supervision90Days$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

fta_90d_hispanic_noGLM <- roc(response = supervision90Days$FTA_Outcome[supervision90Days$D_Race == "Hispanic"],
                        predictor = supervision90Days$Score1[supervision90Days$D_Race == "Hispanic"], levels = c(0,1),
                        direction = "<"
)

fta_90d_women_noGLM <- roc(response = supervision90Days$FTA_Outcome[supervision90Days$D_Gender == "F"],
                     predictor = supervision90Days$Score1[supervision90Days$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

fta_90d_men_noGLM <- roc(response = supervision90Days$FTA_Outcome[supervision90Days$D_Gender == "M"],
                   predictor = supervision90Days$Score1[supervision90Days$D_Gender == "M"], levels = c(0,1),
                   direction = "<"
)


# NCA

nca_30d_white_noGLM <- roc(response = supervision30Days$NCA_Charge[supervision30Days$D_Race == "White"],
                     predictor = supervision30Days$Score2[supervision30Days$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

nca_30d_hispanic_noGLM <- roc(response = supervision30Days$NCA_Charge[supervision30Days$D_Race == "Hispanic"],
                     predictor = supervision30Days$Score2[supervision30Days$D_Race == "Hispanic"], levels = c(0,1),
                     direction = "<"
)

nca_30d_men_noGLM <- roc(response = supervision30Days$NCA_Charge[supervision30Days$D_Gender == "M"],
                     predictor = supervision30Days$Score2[supervision30Days$D_Gender == "M"], levels = c(0,1),
                     direction = "<"
)
nca_30d_women_noGLM <- roc(response = supervision30Days$NCA_Charge[supervision30Days$D_Gender == "F"],
                     predictor = supervision30Days$Score2[supervision30Days$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

nca_90d_women_noGLM <- roc(response = supervision90Days$NCA_Charge[supervision90Days$D_Gender == "F"],
                     predictor = supervision90Days$Score2[supervision90Days$D_Gender == "F"], levels = c(0,1),
                     direction = "<"
)

nca_90d_men_noGLM <- roc(response = supervision90Days$NCA_Charge[supervision90Days$D_Gender == "M"],
                     predictor = supervision90Days$Score2[supervision90Days$D_Gender == "M"], levels = c(0,1),
                     direction = "<"
)

nca_90d_white_noGLM <- roc(response = supervision90Days$NCA_Charge[supervision90Days$D_Race == "White"],
                     predictor = supervision90Days$Score2[supervision90Days$D_Race == "White"], levels = c(0,1),
                     direction = "<"
)

nca_90d_hispanic_noGLM <- roc(response = supervision90Days$NCA_Charge[supervision90Days$D_Race == "Hispanic"],
                     predictor = supervision90Days$Score2[supervision90Days$D_Race == "Hispanic"], levels = c(0,1),
                     direction = "<"
)

fta_nca_noGLM_table <- tibble(Outcome = c("FTA", "FTA", "FTA", "FTA", "NCA", "NCA", "NCA", "NCA"),
                        Demographic = c("Men", "Women", "White", "Hispanic", "Men", "Women", "White", "Hispanic"),
                        One_Month = c(as.numeric(auc(fta_30d_men_noGLM)),
                                      as.numeric(auc(fta_30d_women_noGLM)),
                                      as.numeric(auc(fta_30d_white_noGLM)),
                                      as.numeric(auc(fta_30d_hispanic_noGLM)),
                                      as.numeric(auc(nca_30d_men_noGLM)),
                                      as.numeric(auc(nca_30d_women_noGLM)),
                                      as.numeric(auc(nca_30d_white_noGLM)),
                                      as.numeric(auc(nca_30d_hispanic_noGLM))
                        ),
                        Three_Months = c(as.numeric(auc(fta_90d_men_noGLM)),
                                         as.numeric(auc(fta_90d_women_noGLM)),
                                         as.numeric(auc(fta_90d_white_noGLM)),
                                         as.numeric(auc(fta_90d_hispanic_noGLM)),
                                         as.numeric(auc(nca_90d_men_noGLM)),
                                         as.numeric(auc(nca_90d_women_noGLM)),
                                         as.numeric(auc(nca_90d_white_noGLM)),
                                         as.numeric(auc(nca_90d_hispanic_noGLM))
                        ))

fta_nca_noGLM_cleanTable = fta_nca_noGLM_table %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))

fta_nca_noGLM_cleanTable

fta_noGLM_table = tibble(Outcome = "FTA",
                         Demographic = c("Men", "Women", "White", "Hispanic"),
                                                One_Month = c(round(as.numeric(auc(fta_30d_men_noGLM)), digits = 3),
                                                              round(as.numeric(auc(fta_30d_women_noGLM)), digits = 3),
                                                              round(as.numeric(auc(fta_30d_white_noGLM)), digits = 3),
                                                              round(as.numeric(auc(fta_30d_hispanic_noGLM)), digits = 3)
                                                ),
                                                Three_Months = c(round(as.numeric(auc(fta_90d_men_noGLM)), digits = 3),
                                                                 round(as.numeric(auc(fta_90d_women_noGLM)), digits =  3),
                                                                 round(as.numeric(auc(fta_90d_white_noGLM)), digits = 3),
                                                                 round(as.numeric(auc(fta_90d_hispanic_noGLM)), digits = 3)
                                                ))

fta_noGLM_cleanTable = fta_noGLM_table %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))

fta_noGLM_cleanTable

nca_noGLM_table = tibble(Outcome = "NCA",
                         Demographic = c("Men", "Women", "White", "Hispanic"),
                         One_Month = c(round(as.numeric(auc(nca_30d_men_noGLM)), digits = 3),
                                       round(as.numeric(auc(nca_30d_women_noGLM)), digits = 3),
                                       round(as.numeric(auc(nca_30d_white_noGLM)), digits = 3),
                                       round(as.numeric(auc(nca_30d_hispanic_noGLM)), digits = 3)
                         ),
                         Three_Months = c(round(as.numeric(auc(nca_90d_men_noGLM)), digits = 3),
                                          round(as.numeric(auc(nca_90d_women_noGLM)), digits =  3),
                                          round(as.numeric(auc(nca_90d_white_noGLM)), digits = 3),
                                          round(as.numeric(auc(nca_90d_hispanic_noGLM)), digits = 3)
                         ))

nca_noGLM_cleanTable = nca_noGLM_table %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))

nca_noGLM_cleanTable
