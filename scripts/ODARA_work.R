library(stringr)
library(lubridate)
library(dplyr)
library(yardstick)
library(naniar)
library(tidyverse)
library(tidymodels)
library(caret)
library(knitr)
library(kableExtra)
library(pROC)

# Reading in Data
odara_longData = read.csv("SLO_Assessments_ODARA_1-1-2015 to 12-31-25.csv")
general_recidivism = read.csv("slo_recidivism_for_SupEvents_after_7-1-2021_sent.csv")

# Bind to ODARA:

odara_longMerge = merge(general_recidivism, odara_longData, by = "MNID")

# Extract ODARA Assessment result as just the number

odara_longMerge <- odara_longMerge %>% 
  mutate(RESULT = as.numeric(str_extract(RESULT, pattern = "\\d+")))

# Modify vio_Charge_During_Sup so if the charge matches the following set of penal
# codes, then it will be classified as a YES for reoffending domestic violence, 
# otherwise treat it as a NO


# Violent Charges (HOLD off for Now): "PC69", "PC1361C1", "PC148B", "PC187A", "PC664-PC187A", "PC192B",
#"PC206", "PC207A", "PC664-PC211", "PC211", "PC21710", "PC21810", 
#"PC240", "PC242", "PC2434A","PC2434E1", "PC245A4-F", "PC245A1-F", 
#"PC245A1", "PC245A4", "PC261A2", "PC2615D", "PC2615C", "PC273AA",
#"PC273AA-F", "PC273AB","PC2736D", "PC368B1-F", "PC368B1", "PC368C",
#"PC417A1-M", "PC417A1", "PC4226a", "PC451C", "PC451D", "PC455",
#"PC6469B", "PC6469A", "PC18740", 
dV_general_violence_charges = c("PC243C2", "PC243D", "PC243D-F", "PC2434A",
                     "PC243B", "PC243E1",  "PC166C1", "PC166C4", "PC166C1A", 
                    "PC166C4-F","PC2735F1", "PC2735", "PC2735A", "PC2735A-F", 
                    "PC2735F1-F", "PC2735F2-F", "PC2735F2", "PC2736A", "PC273B")

# Converting DATECREATED, vioDate_duringSup, and vioDate_afterSup to date format
odara_longMerge$DATECREATED <- mdy_hms(odara_longMerge$DATECREATED)
odara_longMerge$vioDate_During_Sup <- mdy(odara_longMerge$vioDate_During_Sup)
odara_longMerge$vioDate_after_Sup <- mdy(odara_longMerge$vioDate_after_Sup)

# First, IDs where the time between the end of the recording period (12/31/25) and
# the assessment date is less than 3 years will be excluded as their supervision period
# has not finished & IDs with no ODARA charges (see above) will be excluded
# If they have an end date with NA, then the supervision period is not over so they
# should be excluded

odara_endDate = as.Date("2025-12-31")

odara_longMerge$assessment_end_diff = as.numeric(time_length(difftime(odara_endDate,
                                                                      odara_longMerge$DATECREATED),"years"))
odara_longMerge = odara_longMerge %>% 
  filter(assessment_end_diff >= 1, 
         vio_Charge_During_Sup %in% dV_general_violence_charges | vio_Charge_after_Sup %in% dV_general_violence_charges)

# Domestic Violence aka ODARA Truth is a 1 if the charge matches with the above vector
# and their assessment date falls within 3 years of the date in vioDateDuringSup OR
# vioDateAfterSup, where the assessment date is DATECREATED

# Difference1 - |Assessment date - vioDateDuringSup| 

odara_longMerge$diff1 = abs(as.numeric(time_length(difftime(odara_longMerge$DATECREATED,
                                                        odara_longMerge$vioDate_During_Sup),"years")))
# Difference2 - |Assessment date - vioDateafterSup|

odara_longMerge$diff2 = abs(as.numeric(time_length(difftime(odara_longMerge$DATECREATED,
                                                            odara_longMerge$vioDate_after_Sup),"years")))

# Setting up the Domestic Violence Outcome
odara_longMerge = odara_longMerge %>% 
  mutate(dV_Outcome = case_when(
    diff1 <= 3 | diff2 <= 3 ~ 1,
    .default = 0
  ))

# Factoring Variables for Logistic Regression

odara_longMerge$GENDER = factor(odara_longMerge$GENDER)
odara_longMerge$RACE = factor(odara_longMerge$RACE)
odara_longMerge$dV_Outcome = factor(odara_longMerge$dV_Outcome)
## ODARA Assessment Score
#odara_longMerge$RESULT = factor(odara_longMerge$RESULT)

# TODO: Figure out what to do with duplicates - Even if you have multiple supervisions, 
# they only have 1 in regards to the dataset

# Maybe keep the one that has a 1

# Logic: if ID is the same, keep first entry with a 1 (exact charge doesnt matter
# as they are all dV charges)

odara_longMerge = odara_longMerge %>% 
  group_by(MNID, DATECREATED) %>% 
  distinct(DATECREATED, .keep_all = TRUE)
  #arrange(desc(dV_Outcome)) %>% 
  #distinct(MNID, .keep_all = TRUE)

# set seed 469464
# 2nd set seed 2478
# 3rd set seed 2
set.seed(2)
#odara_longMerge = odara_longMerge[sample(1:nrow(odara_longMerge)),]

# Drop instances of racial groups that are too underrepresented to predict domestic
# violence outcomes on

odara_longMerge = odara_longMerge %>% 
  filter(RACE %in% c("W: White", "H:  Hispanic"))

# Setting up training and testing

## ODARA No GLM results:

# Race
odara_white_noGLM = roc(response = odara_longMerge$dV_Outcome[odara_longMerge$RACE == "W: White"], 
                  predictor = odara_longMerge$RESULT[odara_longMerge$RACE == "W: White"], direction = "<")

odara_hispanic_noGLM = roc(response = odara_longMerge$dV_Outcome[odara_longMerge$RACE == "H:  Hispanic"], 
                     predictor = odara_longMerge$RESULT[odara_longMerge$RACE == "H:  Hispanic"], direction = "<")

# Gender
odara_men_noGLM = roc(response = odara_longMerge$dV_Outcome[odara_longMerge$GENDER == "M"], 
                predictor = odara_longMerge$RESULT[odara_longMerge$GENDER == "M"], direction = "<")

odara_women_noGLM = roc(response = odara_longMerge$dV_Outcome[odara_longMerge$GENDER == "F"], 
                  predictor = odara_longMerge$RESULT[odara_longMerge$GENDER == "F"], direction = "<")

# ODARA Demographics Curves

plot(odara_men_noGLM, 
     col = "blue", main = "ODARA ROC Curves for Demographics", print.auc = TRUE,
     print.auc.x = 1.9,
     print.auc.y = 0.9)

plot(odara_women_noGLM, 
     col = "red", print.auc = TRUE, add = TRUE,
     print.auc.x = 1.9,
     print.auc.y = 0.7)

plot(odara_white_noGLM, 
     col = "green", print.auc = TRUE, add = TRUE,
     print.auc.x = 1.9,
     print.auc.y = 0.5)

plot(odara_hispanic_noGLM, 
     col = "orange", lty = 2, print.auc = TRUE, add = TRUE,
     print.auc.x = 1.9,
     print.auc.y = 0.3)
legend("bottomleft", legend=c("Men", "Women", "White", "Hispanic"), 
       col=c("blue", "red", "green", "orange"),
       pch=c(19, 18))

## Logistic Regression
#odara_split = initial_split(odara_longMerge, prop = 0.8)
#odara_training = training(odara_split)
#odara_testing = testing(odara_split)

# Fitting models
#odara_model = glm(dV_Outcome ~ RESULT, data = odara_training, family = "binomial")



# Predicting on Test Set
#odara_testing$predictions <- predict(odara_model, newdata = odara_testing, type = "response")


## AUC Values

# Race

#odara_white = roc(response = odara_testing$dV_Outcome[odara_testing$RACE == "W: White"], 
        # predictor = odara_testing$predictions[odara_testing$RACE == "W: White"], direction = "<")

#odara_hispanic = roc(response = odara_testing$dV_Outcome[odara_testing$RACE == "H:  Hispanic"], 
               #   predictor = odara_testing$predictions[odara_testing$RACE == "H:  Hispanic"], direction = "<")

# Gender

#odara_men = roc(response = odara_testing$dV_Outcome[odara_testing$GENDER == "M"], 
                #  predictor = odara_testing$predictions[odara_testing$GENDER == "M"], direction = "<")

#odara_women = roc(response = odara_testing$dV_Outcome[odara_testing$GENDER == "F"], 
                 # predictor = odara_testing$predictions[odara_testing$GENDER == "F"], direction = "<")


#odara_table <- tibble(Outcome = "ODARA",
                       # Demographic = c("Men", "Women", "White", "Hispanic"),
                       # AUC = c(as.numeric(auc(odara_men)),
                                   #   as.numeric(auc(odara_women)),
                                    #  as.numeric(auc(odara_white)),
                                    #  as.numeric(auc(odara_hispanic))),
                       # CI_Lower = c(as.numeric(ci.auc(odara_men)[1]),
                                    # as.numeric(ci.auc(odara_women)[1]),
                                    # as.numeric(ci.auc(odara_white)[1]),
                                    # as.numeric(ci.auc(odara_hispanic)[1])
 
                      #  ),
                      #  CI_Upper = c(as.numeric(ci.auc(odara_men)[3]),
                                  # as.numeric(ci.auc(odara_women)[3]),
                                  # as.numeric(ci.auc(odara_white)[3]),
                                  # as.numeric(ci.auc(odara_hispanic)[3])
                                   
                   #   ))



#odara_cleanTable = odara_table %>% 
  #kbl() %>% 
  #kable_minimal(c("striped", "hover"))

#odara_cleanTable

# Overall ODARA Curve

#plot(roc(odara_testing$dV_Outcome, 
      #   odara_testing$predictions), 
         # col = "blue", main = "ROC Curve", print.auc = TRUE)

# ODARA Demographics Curves

#plot(odara_men, 
     #col = "blue", main = "ODARA ROC Curves for Demographics", print.auc = TRUE,
     #print.auc.x = 1.9,
     #print.auc.y = 0.9)

#plot(odara_women, 
    # col = "red", print.auc = TRUE, add = TRUE,
     #print.auc.x = 1.9,
    # print.auc.y = 0.7)

#plot(odara_white, 
     #col = "green", print.auc = TRUE, add = TRUE,
    # print.auc.x = 1.9,
     #print.auc.y = 0.5)

#plot(odara_hispanic, 
    # col = "orange", lty = 2, print.auc = TRUE, add = TRUE,
     #print.auc.x = 1.9,
     #print.auc.y = 0.3)
### White AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$RACE == "W: White"], 
         #odara_testing$race.predictions[odara_testing$RACE == "W: White"]), 
    # col = "blue", main = "ROC Curve", print.auc = TRUE)

### Hispanic AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$RACE == "H:  Hispanic"], 
        # odara_testing$race.predictions[odara_testing$RACE == "H:  Hispanic"]), 
     #col = "blue", main = "ROC Curve", print.auc = TRUE)

## Gender


#plot(roc(odara_testing$dV_Outcome, 
        # odara_testing$gender.predictions), 
    # col = "blue", main = "ROC Curve", print.auc = TRUE)

### Men AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$GENDER == "M"], 
        # odara_testing$gender.predictions[odara_testing$GENDER == "M"]), 
     #col = "blue", main = "ROC Curve", print.auc = TRUE)

### Women AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$GENDER == "F"], 
         #odara_testing$gender.predictions[odara_testing$GENDER == "F"]), 
     #col = "blue", main = "ROC Curve", print.auc = TRUE)



#auc_odara_race <- odara_testing %>%
 # group_by(RACE) %>%
  #summarize(auc_value = as.numeric(auc(response = dV_Outcome, predictor = race.predictions)))

#auc_odara_race <- odara_testing %>%
  #group_by(RACE) %>%
  #summarize(auc_value = as.numeric(auc(response = dV_Outcome, predictor = race.predictions)))

#auc_odara_gender <- odara_testing %>%
  #group_by(GENDER) %>%
  #summarize(auc_value = as.numeric(auc(response = dV_Outcome, predictor = gender.predictions)))


# DeLong Tests

## Race

#roc.test(roc(odara_testing$dV_Outcome[odara_testing$RACE == "W: White"],
             #odara_testing$race.predictions[odara_testing$RACE == "W: White"]), 
        # roc(odara_testing$dV_Outcome[odara_testing$RACE == "H:  Hispanic"],
            # odara_testing$race.predictions[odara_testing$RACE == "H:  Hispanic"]))

#roc.test(roc(odara_testing$dV_Outcome[odara_testing$GENDER == "M"],
            # odara_testing$gender.predictions[odara_testing$GENDER == "M"]), 
         #roc(odara_testing$dV_Outcome[odara_testing$GENDER == "F"],
            # odara_testing$gender.predictions[odara_testing$GENDER == "F"]))
# Confidence Intervals

## Race
#ci_odara_race <- odara_testing %>%
  #group_by(RACE) %>%
  #reframe(ci_seq = as.numeric(ci(response = dV_Outcome, predictor = race.predictions))[c(1,3)])

#ci_odara_gender <- odara_testing %>%
  #group_by(GENDER) %>%
  #reframe(ci_seq = as.numeric(ci(response = dV_Outcome, predictor = gender.predictions))[c(1,3)])

# Formatting
#ci_odara_race = ci_odara_race %>% 
  #pivot_wider(
    #names_from = RACE,
    #values_from = ci_seq
  #)

#ci_odara_gender = ci_odara_gender %>% 
 # pivot_wider(
   # names_from = GENDER,
    #values_from = ci_seq
  #)

# ODARA

## Race

#plot(roc(odara_testing$dV_Outcome, 
         #odara_testing$race.predictions), 
    # col = "blue", 
     #main = "Accuracy Curve of Race on recidivism of Domestic Violence",
     #print.auc = TRUE)

### White AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$RACE == "W: White"], 
        # odara_testing$race.predictions[odara_testing$RACE == "W: White"]), 
    # col = "blue", main = "ROC Curve", print.auc = TRUE)

### Hispanic AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$RACE == "H:  Hispanic"], 
        # odara_testing$race.predictions[odara_testing$RACE == "H:  Hispanic"]), 
     #col = "blue", main = "ROC Curve", print.auc = TRUE)

## Gender


#plot(roc(odara_testing$dV_Outcome, 
         #odara_testing$gender.predictions), 
     #col = "blue", main = "ROC Curve", print.auc = TRUE)

### Men AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$GENDER == "M"], 
         #odara_testing$gender.predictions[odara_testing$GENDER == "M"]), 
    # col = "blue", main = "ROC Curve", print.auc = TRUE)

### Women AUC
#plot(roc(odara_testing$dV_Outcome[odara_testing$GENDER == "F"], 
         #odara_testing$gender.predictions[odara_testing$GENDER == "F"]), 
     #col = "blue", main = "ROC Curve", print.auc = TRUE)
