# Splitting Data:

library(modelr)
library(yardstick)

# Testing Sets
supervision30days_testing = testing(supervision30days_split)
supervision90days_testing = testing(supervision90days_split)

# Predictions and Results

# Race
## FTA
supervision30days_testing$predictions.fta.race <- predict(glm30fitFTA_Race, 
                                                 newdata = supervision30days_testing,
                                                 type = "response")
supervision90days_testing$predictions.fta.race <- predict(glm90fitFTA_Race, 
                                                 newdata = supervision90days_testing,
                                                 type = "response")
## NCA
supervision30days_testing$predictions.nca.race <- predict(glm30fitNCA_Race, 
                                                 newdata = supervision30days_testing,
                                                 type = "response")
supervision90days_testing$predictions.nca.race <- predict(glm90fitNCA_Race, 
                                                 newdata = supervision90days_testing,
                                                 type = "response")
# Gender
## FTA
supervision30days_testing$predictions.fta.gender <- predict(glm30fitFTA_Gender, 
                                                          newdata = supervision30days_testing,
                                                          type = "response")
supervision90days_testing$predictions.fta.gender <- predict(glm90fitFTA_Gender, 
                                                          newdata = supervision90days_testing,
                                                          type = "response")
## NCA
supervision30days_testing$predictions.nca.gender <- predict(glm30fitNCA_Gender, 
                                                          newdata = supervision30days_testing,
                                                          type = "response")
supervision90days_testing$predictions.nca.gender <- predict(glm90fitNCA_Gender, 
                                                          newdata = supervision90days_testing,
                                                          type = "response")

# Calculating AUC values
## FTA
auc_fta_race_30d <- supervision30days_testing %>%
  group_by(D_Race) %>%
  summarize(auc_value = if(n_distinct(FTA_Outcome) < 2) 0.5 else as.numeric(auc(response = FTA_Outcome, predictor = predictions.fta.race)))
auc_fta_race_90d <- supervision90days_testing %>%
  group_by(D_Race) %>%
  summarize(auc_value = if(n_distinct(FTA_Outcome) < 2) 0.5 else as.numeric(auc(response = FTA_Outcome, predictor = predictions.fta.race)))
auc_fta_gender_30d <- supervision30days_testing %>%
  group_by(D_Gender) %>%
  summarize(auc_value = as.numeric(auc(response = FTA_Outcome, predictor = predictions.fta.gender)))
auc_fta_gender_90d <- supervision90days_testing %>%
  group_by(D_Gender) %>%
  summarize(auc_value = as.numeric(auc(response = FTA_Outcome, predictor = predictions.fta.gender)))
## NCA
auc_nca_race_30d <- supervision30days_testing %>%
  group_by(D_Race) %>%
  summarize(auc_value = if(n_distinct(NCA_Charge) < 2) 0.5 else as.numeric(auc(response = NCA_Charge, predictor = predictions.nca.race)))
auc_nca_race_90d <- supervision90days_testing %>%
  group_by(D_Race) %>%
  summarize(auc_value = if(n_distinct(NCA_Charge) < 2) 0.5 else as.numeric(auc(response = NCA_Charge, predictor = predictions.nca.race)))
auc_nca_gender_30d <- supervision30days_testing %>%
  group_by(D_Gender) %>%
  summarize(auc_value = as.numeric(auc(response = NCA_Charge, predictor = predictions.nca.gender)))
auc_nca_gender_90d <- supervision90days_testing %>%
  group_by(D_Gender) %>%
  summarize(auc_value = as.numeric(auc(response = NCA_Charge, predictor = predictions.nca.gender)))

# Confidence Intervals
## FTA
ci_fta_race_30d <- supervision30days_testing %>%
  group_by(D_Race) %>%
  reframe(ci_seq = if(n_distinct(FTA_Outcome) < 2) c(0.5,0.5) else as.numeric(ci(response = FTA_Outcome, predictor = predictions.fta.race))[c(1,3)])
ci_fta_race_90d <- supervision90days_testing %>%
  group_by(D_Race) %>%
  reframe(ci_seq = if(n_distinct(FTA_Outcome) < 2) c(0.5,0.5) else as.numeric(ci(response = FTA_Outcome, predictor = predictions.fta.race))[c(1,3)])
ci_fta_gender_30d <- supervision30days_testing %>%
  group_by(D_Gender) %>%
  reframe(ci_seq = as.numeric(ci(response = FTA_Outcome, predictor = predictions.fta.gender))[c(1,3)])
ci_fta_gender_90d <- supervision90days_testing %>%
  group_by(D_Gender) %>%
  reframe(ci_seq = as.numeric(ci(response = FTA_Outcome, predictor = predictions.fta.gender))[c(1,3)])
## NCA
ci_nca_race_30d <- supervision30days_testing %>%
  group_by(D_Race) %>%
  reframe(ci_seq = if(n_distinct(NCA_Charge) < 2) c(0.5,0.5) else as.numeric(ci(response = NCA_Charge, predictor = predictions.nca.race))[c(1,3)])
ci_nca_race_90d <- supervision90days_testing %>%
  group_by(D_Race) %>%
  reframe(ci_seq = if(n_distinct(NCA_Charge) < 2) c(0.5,0.5) else as.numeric(ci(response = NCA_Charge, predictor = predictions.nca.race))[c(1,3)])
ci_nca_gender_30d <- supervision30days_testing %>%
  group_by(D_Gender) %>%
  reframe(ci_seq = as.numeric(ci(response = NCA_Charge, predictor = predictions.nca.gender))[c(1,3)])
ci_nca_gender_90d <- supervision90days_testing %>%
  group_by(D_Gender) %>%
  reframe(ci_seq = as.numeric(ci(response = NCA_Charge, predictor = predictions.nca.gender))[c(1,3)])

# Pivot Wider to make it easier to read

## FTA
ci_fta_race_30d = ci_fta_race_30d %>% 
  pivot_wider(
    names_from = D_Race,
    values_from = ci_seq
  )
ci_fta_race_90d = ci_fta_race_90d %>% 
  pivot_wider(
    names_from = D_Race,
    values_from = ci_seq
  )
ci_fta_gender_30d = ci_fta_gender_30d %>% 
  pivot_wider(
    names_from = D_Gender,
    values_from = ci_seq
  )
ci_fta_gender_90d = ci_fta_gender_90d %>% 
  pivot_wider(
    names_from = D_Gender,
    values_from = ci_seq
  )
## NCA
ci_nca_race_30d = ci_nca_race_30d %>% 
  pivot_wider(
    names_from = D_Race,
    values_from = ci_seq
  )
ci_nca_race_90d = ci_nca_race_90d %>% 
  pivot_wider(
    names_from = D_Race,
    values_from = ci_seq
  )
ci_nca_gender_30d = ci_nca_gender_30d %>% 
  pivot_wider(
    names_from = D_Gender,
    values_from = ci_seq
  )
ci_nca_gender_90d = ci_nca_gender_90d %>% 
  pivot_wider(
    names_from = D_Gender,
    values_from = ci_seq
  )









# race_ci_func = function(race_set){
#   race_set = race_set %>% 
#     pivot_wider(
#       names_from = D_Race,
#       values_from = ci_seq
#     )
# }
# 
# gender_ci_func = function(gender_set){
#   gender_set = gender_set %>% 
#     pivot_wider(
#       names_from = D_Gender,
#       values_from = ci_seq
#     )
# }
# 
# conf_ints_race = lapply()

# class_metrics = metric_set(yardstick::sensitivity, yardstick::specificity, accuracy, roc_auc)
# # Race
# fta_30_day_race_results = supervision30days_testing %>%
#   add_predictions(glm30fitFTA_Race, type = 'response') %>% 
#   mutate(pred.fta = factor(pred > 0.5, labels = c("No FTA", "FTA")),
#          FTA_Outcome = factor(FTA_Outcome, labels = c("No FTA", "FTA"))) %>% 
#   class_metrics(
#     truth = FTA_Outcome,
#     estimate = pred.fta,
#     pred,
#     event_level = "second"
#   )
# 
# fta_90_day_race_results = supervision90days_testing %>%
#   add_predictions(glm90fitFTA_Race, type = 'response') %>%
#   mutate(pred.fta = factor(pred > 0.5, labels = c("No FTA", "FTA")),
#          FTA_Outcome = factor(FTA_Outcome, labels = c("No FTA", "FTA"))) %>% 
#   class_metrics(
#     truth = FTA_Outcome,
#     estimate = pred.fta,
#     pred,
#     event_level = "second"
#   )
# 
# nca_30_day_race_results = supervision30days_testing %>%
#   add_predictions(glm30fitNCA_Race, type = 'response') %>% 
#   mutate(pred.nca = factor(pred > 0.5, labels = c("No NCA", "NCA")),
#          NCA_Charge = factor(NCA_Charge, labels = c("No NCA", "NCA"))) %>% 
#   class_metrics(
#     truth = NCA_Charge,
#     estimate = pred.nca,
#     pred,
#     event_level = "second"
#   )
# 
# nca_90_day_race_results = supervision90days_testing %>%
#   add_predictions(glm90fitNCA_Race, type = 'response') %>% 
#   mutate(pred.nca = factor(pred > 0.5, labels = c("No NCA", "NCA")),
#          NCA_Charge = factor(NCA_Charge, labels = c("No NCA", "NCA"))) %>% 
#   class_metrics(
#     truth = NCA_Charge,
#     estimate = pred.nca,
#     pred,
#     event_level = "second"
#   )
# 
# # Gender
# fta_30_day_gender_results = supervision30days_testing %>%
#   add_predictions(glm30fitFTA_Gender, type = 'response') %>% 
#   mutate(pred.fta = factor(pred > 0.5, labels = c("No FTA", "FTA")),
#          FTA_Outcome = factor(FTA_Outcome, labels = c("No FTA", "FTA"))) %>% 
#   class_metrics(
#     truth = FTA_Outcome,
#     estimate = pred.fta,
#     pred,
#     event_level = "second"
#   )
# 
# fta_90_day_gender_results = supervision90days_testing %>%
#   add_predictions(glm90fitFTA_Gender, type = 'response') %>% 
#   mutate(pred.fta = factor(pred > 0.5, labels = c("No FTA", "FTA")),
#          FTA_Outcome = factor(FTA_Outcome, labels = c("No FTA", "FTA"))) %>% 
#   class_metrics(
#     truth = FTA_Outcome,
#     estimate = pred.fta,
#     pred,
#     event_level = "second"
#   )
# 
# nca_30_day_gender_results = supervision30days_testing %>%
#   add_predictions(glm30fitNCA_Gender, type = 'response') %>% 
#   mutate(pred.nca = factor(pred > 0.5, labels = c("No NCA", "NCA")),
#          NCA_Charge = factor(NCA_Charge, labels = c("No NCA", "NCA"))) %>% 
#   class_metrics(
#     truth = NCA_Charge,
#     estimate = pred.nca,
#     pred,
#     event_level = "second"
#   )
# 
# nca_90_day_gender_results = supervision90days_testing %>%
#   add_predictions(glm90fitNCA_Gender, type = 'response') %>% 
#   mutate(pred.nca = factor(pred > 0.5, labels = c("No NCA", "NCA")),
#          NCA_Charge = factor(NCA_Charge, labels = c("No NCA", "NCA"))) %>% 
#   class_metrics(
#     truth = NCA_Charge,
#     estimate = pred.nca,
#     pred,
#     event_level = "second"
#   )
