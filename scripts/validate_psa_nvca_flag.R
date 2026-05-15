library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(pROC)
library(ggplot2)

# ------------------------------------------------------------
# 1. Read PSA assessments
# ------------------------------------------------------------

psa_raw <- read_csv(
  "~/pstat-197-data.git/SLO_Assessments_7-1-21 to 12-31-25.csv",
  show_col_types = FALSE
)

psa_clean <- psa_raw %>%
  mutate(
    MNID = ID,
    assessment_date = as_date(mdy_hms(`Date of assessment`))
  ) %>%
  mutate(
    nvca_term = case_when(
      str_detect(`Scale Term1`, "NVCA") ~ `Scale Term1`,
      str_detect(`Scale Term2`, "NVCA") ~ `Scale Term2`,
      str_detect(`Scale Term3`, "NVCA") ~ `Scale Term3`,
      TRUE ~ NA_character_
    ),
    nvca_score = case_when(
      str_detect(`Scale Term1`, "NVCA") ~ Score1,
      str_detect(`Scale Term2`, "NVCA") ~ Score2,
      str_detect(`Scale Term3`, "NVCA") ~ Score3,
      TRUE ~ NA_real_
    ),
    nvca_flag = case_when(
      str_detect(nvca_term, "Yes") ~ 1,
      str_detect(nvca_term, "No") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(MNID, assessment_date, nvca_term, nvca_score, nvca_flag)


# ------------------------------------------------------------
# 2. Read event-level violent rearrest outcome
# ------------------------------------------------------------

violent_evt <- read_csv(
  "data/processed/violent_rearrest_during_sup_by_evtid.csv",
  show_col_types = FALSE
) %>%
  mutate(
    EVTID = as.numeric(EVTID),
    MNID = as.character(MNID),
    sup_start = as_date(sup_start),
    sup_end = as_date(sup_end)
  ) %>%
  filter(!is.na(EVTID), !is.na(MNID)) %>%
  filter(!is.na(sup_start), !is.na(sup_end))

violent_evt %>%
  count(violent_rearrest_during_sup)


# ------------------------------------------------------------
# 3. Link each event to the most recent PSA assessment
#    on or before supervision start
# ------------------------------------------------------------

psa_event_linked <- violent_evt %>%
  left_join(psa_clean, by = "MNID", relationship = "many-to-many") %>%
  filter(!is.na(assessment_date)) %>%
  filter(assessment_date <= sup_start) %>%
  mutate(
    days_between_assessment_and_sup = as.numeric(sup_start - assessment_date)
  ) %>%
  group_by(EVTID, MNID) %>%
  slice_min(days_between_assessment_and_sup, n = 1, with_ties = FALSE) %>%
  ungroup()


# ------------------------------------------------------------
# 4. Final analytic dataset for NVCA validation
# ------------------------------------------------------------

nvca_validation <- psa_event_linked %>%
  filter(!is.na(nvca_flag)) %>%
  mutate(
    days_released = as.numeric(sup_end - sup_start)
  ) %>%
  select(
    EVTID,
    MNID,
    sup_start,
    sup_end,
    assessment_date,
    days_between_assessment_and_sup,
    days_released,
    nvca_term,
    nvca_score,
    nvca_flag,
    violent_rearrest_during_sup,
    n_charge_rows,
    n_violent_charge_rows
  )


# ------------------------------------------------------------
# 5. Sample flow
# ------------------------------------------------------------

sample_flow <- tibble::tibble(
  step = c(
    "Violence outcome events",
    "Linked to PSA assessment before supervision start",
    "Final NVCA validation sample"
  ),
  n_events = c(
    nrow(violent_evt),
    nrow(psa_event_linked),
    nrow(nvca_validation)
  ),
  violent_rearrests = c(
    sum(violent_evt$violent_rearrest_during_sup, na.rm = TRUE),
    sum(psa_event_linked$violent_rearrest_during_sup, na.rm = TRUE),
    sum(nvca_validation$violent_rearrest_during_sup, na.rm = TRUE)
  )
)

sample_flow


# ------------------------------------------------------------
# 6. Main NVCA validation table
# ------------------------------------------------------------

nvca_results <- nvca_validation %>%
  group_by(nvca_flag) %>%
  summarise(
    total_events = n(),
    violent_rearrests = sum(violent_rearrest_during_sup, na.rm = TRUE),
    violent_rearrest_rate = round(
      100 * mean(violent_rearrest_during_sup, na.rm = TRUE),
      1
    ),
    .groups = "drop"
  ) %>%
  mutate(
    nvca_group = ifelse(
      nvca_flag == 1,
      "NVCA-Yes / Flag",
      "NVCA-No / No Flag"
    ),
    label = paste0(
      violent_rearrests, "/", total_events,
      " (", violent_rearrest_rate, "%)"
    )
  ) %>%
  select(nvca_group, total_events, violent_rearrests, violent_rearrest_rate, label)

nvca_results


# ------------------------------------------------------------
# 7. Confusion table
# ------------------------------------------------------------

confusion_table <- nvca_validation %>%
  count(nvca_flag, violent_rearrest_during_sup) %>%
  mutate(
    nvca_group = ifelse(
      nvca_flag == 1,
      "NVCA-Yes / Flag",
      "NVCA-No / No Flag"
    ),
    outcome = ifelse(
      violent_rearrest_during_sup == 1,
      "Violent rearrest",
      "No violent rearrest"
    )
  ) %>%
  select(nvca_group, outcome, n)

confusion_table


# ------------------------------------------------------------
# 8. AUC for binary NVCA flag
# ------------------------------------------------------------

roc_obj <- roc(
  response = nvca_validation$violent_rearrest_during_sup,
  predictor = nvca_validation$nvca_flag,
  quiet = TRUE
)

nvca_auc <- auc(roc_obj)
nvca_ci <- ci.auc(roc_obj)

nvca_auc_summary <- tibble::tibble(
  auc = as.numeric(nvca_auc),
  ci_lower = as.numeric(nvca_ci[1]),
  ci_mid = as.numeric(nvca_ci[2]),
  ci_upper = as.numeric(nvca_ci[3])
)

nvca_auc_summary


# ------------------------------------------------------------
# 9. Logistic regression controlling for days released
# ------------------------------------------------------------

nvca_logit <- glm(
  violent_rearrest_during_sup ~ nvca_flag + days_released,
  data = nvca_validation,
  family = binomial()
)

summary(nvca_logit)

logit_coef <- as.data.frame(summary(nvca_logit)$coefficients) %>%
  tibble::rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  ) %>%
  mutate(
    odds_ratio = exp(estimate),
    ci_lower = exp(estimate - 1.96 * std_error),
    ci_upper = exp(estimate + 1.96 * std_error)
  )

logit_coef


# ------------------------------------------------------------
# 10. Save aggregate outputs
#    Note: avoid committing row-level data if repo is public.
# ------------------------------------------------------------

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

write_csv(
  nvca_results,
  "data/processed/nvca_results_by_flag.csv"
)

write_csv(
  confusion_table,
  "data/processed/nvca_confusion_table.csv"
)

write_csv(
  sample_flow,
  "data/processed/nvca_sample_flow.csv"
)

write_csv(
  nvca_auc_summary,
  "data/processed/nvca_auc_summary.csv"
)

write_csv(
  logit_coef,
  "data/processed/nvca_logistic_regression_summary.csv"
)

# Optional local-only file. Do not commit if data is sensitive.
write_csv(
  nvca_validation,
  "data/processed/nvca_validation_dataset_LOCAL_ONLY.csv"
)


# ------------------------------------------------------------
# 11. Poster chart: violent rearrest rate by NVCA flag
# ------------------------------------------------------------

p_nvca_rate <- ggplot(
  nvca_results,
  aes(x = nvca_group, y = violent_rearrest_rate)
) +
  geom_col() +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 4.5
  ) +
  labs(
    title = "Violent Rearrest Rate by PSA Violence Flag",
    subtitle = paste0("Linked NVCA validation sample: n = ", nrow(nvca_validation), " events"),
    x = "",
    y = "Violent rearrest rate (%)"
  ) +
  theme_minimal()

p_nvca_rate

ggsave(
  "figures/nvca_violent_rearrest_rate_by_flag.png",
  p_nvca_rate,
  width = 7,
  height = 5
)


# ------------------------------------------------------------
# 12. ROC curve / AUC plot for PSA NVCA flag
# ------------------------------------------------------------

auc_label <- paste0(
  "AUC = ", round(as.numeric(nvca_auc), 3),
  "\n95% CI: ",
  round(as.numeric(nvca_ci[1]), 3), "–",
  round(as.numeric(nvca_ci[3]), 3)
)

p_nvca_roc <- ggroc(roc_obj, legacy.axes = TRUE) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 0.65,
    y = 0.20,
    label = auc_label,
    size = 5
  ) +
  labs(
    title = "ROC Curve for PSA Violence Flag",
    subtitle = "Predicting violent rearrest during pretrial supervision",
    x = "False positive rate",
    y = "True positive rate"
  ) +
  theme_minimal()

p_nvca_roc

ggsave(
  "figures/nvca_roc_curve.png",
  p_nvca_roc,
  width = 7,
  height = 5
)


# ------------------------------------------------------------
# 13. Confusion matrix chart
# ------------------------------------------------------------

confusion_plot_data <- confusion_table %>%
  mutate(
    nvca_group = factor(
      nvca_group,
      levels = c("NVCA-No / No Flag", "NVCA-Yes / Flag")
    ),
    outcome = factor(
      outcome,
      levels = c("No violent rearrest", "Violent rearrest")
    )
  )

p_confusion <- ggplot(
  confusion_plot_data,
  aes(x = nvca_group, y = outcome, fill = n)
) +
  geom_tile() +
  geom_text(aes(label = n), size = 6) +
  labs(
    title = "PSA Violence Flag vs. Violent Rearrest Outcome",
    x = "PSA Violence Flag",
    y = "Outcome"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p_confusion

ggsave(
  "figures/nvca_confusion_matrix.png",
  p_confusion,
  width = 7,
  height = 5
)