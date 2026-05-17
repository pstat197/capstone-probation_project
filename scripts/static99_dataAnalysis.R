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

# Static 99 - an actuarial risk prediction instrument designed to estimate the 
# probability of sexual and violent reconviction for adult males 
# who have already been charged with or convicted of at least one 
# sexual offense against a child or a non-consenting adult.


## Preprocessing and Cleaning
supervisionEntries = read.csv("Copy of Adult_supervision_event_w_group_assignments(1).csv")
static99Assessments = read.csv("SLO_Assessments_Static99_1-1-2015 to 12-31-25(1).csv")
generalRecidivismData = read.csv("slo_recidivism_for_SupEvents_after_7-1-2021_sent.csv")

## Merging 

static99Entries = inner_join(static99Assessments, supervisionEntries, by = "MNID",
                             relationship = "many-to-many")

static99RecidivismEntries = inner_join(static99Entries, generalRecidivismData, 
                                       by = "MNID", relationship = "many-to-many")

## Keep Essential Columns - MNID, DATECREATED, RESULT, GENDER, RACE, VIODATE DURING 
## & AFTER SUP, & VIOCHARGE DURING/AFTER SUP

static99RecidivismEntries = static99RecidivismEntries %>% 
                              select(MNID, DATECREATED, RESULT, GENDER, RACE,
                                     vioDate_During_Sup, vioDate_after_Sup,
                                     vio_Charge_During_Sup, vio_Charge_after_Sup)

penalCodes = c("PC290", "PC187","PC207", "PC209", "PC220", "PC2361B",
               "PC2361C", "PC2434", "PC261", "PC2641", "PC266", "PC266HB",
               "PC266IB", "PC266J", "PC267", "PC269", "PC272", "PC285",
               "PC286", "PC287", "PC288", "PC288A", "PC288B", "PC288B",
               "PC288C", "PC2882", "PC2883", "PC2884", "PC2885", "PC2885",
               "PC2887", "PC289", "PC3111", "PC3112", "PC3112", "PC3113",
               "PC3114", "PC31110", "PC31111", "PC6476", "PC314", "PC64721",
               "PC653FC", "PC66761")

##  Setting up dates

static99_endDate = as.Date("2025-12-31")

# Converting DATECREATED, vioDate_duringSup, and vioDate_afterSup to date format
static99RecidivismEntries$DATECREATED <- mdy_hms(static99RecidivismEntries$DATECREATED)
static99RecidivismEntries$vioDate_During_Sup <- mdy(static99RecidivismEntries$vioDate_During_Sup)
static99RecidivismEntries$vioDate_after_Sup <- mdy(static99RecidivismEntries$vioDate_after_Sup)

static99RecidivismEntries$assessment_end_diff = as.numeric(time_length(difftime(static99_endDate,
                                                                      static99RecidivismEntries$DATECREATED),"years"))
static99RecidivismEntries = static99RecidivismEntries %>% 
  filter(assessment_end_diff >= 3, 
         vio_Charge_During_Sup %in% penalCodes| vio_Charge_after_Sup %in% penalCodes)

# Apparently none of the charges appear in either variable?

## Constructing Static 99 Recidivism Outcome Variable - 1 if the charge matches with penalCodes
# and their assessment date falls within 3 years of the date in vioDateDuringSup OR
# vioDateAfterSup, where the assessment date is DATECREATED

# Difference1 - |Assessment date - vioDateDuringSup| 

static99RecidivismEntries$diff1 = abs(as.numeric(time_length(difftime(static99RecidivismEntries$DATECREATED,
                                                            static99RecidivismEntries$vioDate_During_Sup),"years")))
# Difference2 - |Assessment date - vioDateafterSup|

static99RecidivismEntries$diff2 = abs(as.numeric(time_length(difftime(static99RecidivismEntries$DATECREATED,
                                                            static99RecidivismEntries$vioDate_after_Sup),"years")))

# Setting up the Domestic Violence Outcome
static99RecidivismEntries = static99RecidivismEntries %>% 
  mutate(static99_outcome = case_when(
    diff1 <= 5 | diff2 <= 5 ~ 1,
    .default = 0
  ))


