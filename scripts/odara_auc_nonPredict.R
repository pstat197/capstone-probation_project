# Defactoring
odara_longMerge$dV_Outcome = as.numeric(odara_longMerge$dV_Outcome) - 1


# RACE Rocs
roc_odara_white <- roc(response = odara_longMerge$dV_Outcome[odara_longMerge$RACE == "W: White"],
                      predictor = odara_longMerge$RESULT[odara_longMerge$RACE == "W: White"], levels = c(0,1),
                      direction = "<"
)

roc_odara_hispanic <- roc(response = odara_longMerge$dV_Outcome[odara_longMerge$RACE == "H:  Hispanic"],
                       predictor = odara_longMerge$RESULT[odara_longMerge$RACE == "H:  Hispanic"], levels = c(0,1),
                       direction = "<"
)

# Gender ROCs
roc_odara_women <- roc(response = odara_longMerge$dV_Outcome[odara_longMerge$GENDER == "F"],
                          predictor = odara_longMerge$RESULT[odara_longMerge$GENDER == "F"], levels = c(0,1),
                          direction = "<"
)

roc_odara_men <- roc(response = odara_longMerge$dV_Outcome[odara_longMerge$GENDER == "M"],
                          predictor = odara_longMerge$RESULT[odara_longMerge$GENDER == "M"], levels = c(0,1),
                          direction = "<"
)

auc_overall_odara <- tibble(
  Demographic = c("Men", "Women", "White", "Hispanic"),
  AUC = c(as.numeric(auc(roc_odara_men)),
    as.numeric(auc(roc_odara_women)),
    as.numeric(auc(roc_odara_white)),
    as.numeric(auc(roc_odara_hispanic))
  ),
  CI_Lower = c(as.numeric(ci.auc(roc_odara_men))[1],
    as.numeric(ci.auc(roc_odara_women))[1],
    as.numeric(ci.auc(roc_odara_white))[1],
    as.numeric(ci.auc(roc_odara_hispanic))[1]
  ),
  CI_Upper = c(as.numeric(ci.auc(roc_odara_men))[3],
               as.numeric(ci.auc(roc_odara_women))[3],
               as.numeric(ci.auc(roc_odara_white))[3],
               as.numeric(ci.auc(roc_odara_hispanic))[3]
  ),
  N = c(nrow(odara_longMerge[odara_longMerge$GENDER == "M",]),
        nrow(odara_longMerge[odara_longMerge$GENDER == "F",]),
        nrow(odara_longMerge[odara_longMerge$RACE == "W: White",]),
        nrow(odara_longMerge[odara_longMerge$RACE == "H:  Hispanic",]))
)

auc_odara_clean = auc_overall_odara %>% 
  kbl() %>% 
  kable_minimal(c("striped", "hover"))