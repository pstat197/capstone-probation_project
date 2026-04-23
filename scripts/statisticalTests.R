# DeLong Tests

# Race

## 30 Days

### FTA
roc.test(roc(supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Race == "White"],
             supervision30days_testing$predictions.fta.race[supervision30days_testing$D_Race == "White"]), 
         roc(supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Race == "Hispanic"],
             supervision30days_testing$predictions.fta.race[supervision30days_testing$D_Race == "Hispanic"]))

### NCA
roc.test(roc(supervision30days_testing$NCA_Charge[supervision30days_testing$D_Race == "White"],
             supervision30days_testing$predictions.nca.race[supervision30days_testing$D_Race == "White"]), 
         roc(supervision30days_testing$NCA_Charge[supervision30days_testing$D_Race == "Hispanic"],
             supervision30days_testing$predictions.nca.race[supervision30days_testing$D_Race == "Hispanic"]))

## 90 Days

### FTA
roc.test(roc(supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Race == "White"],
             supervision90days_testing$predictions.fta.race[supervision90days_testing$D_Race == "White"]), 
         roc(supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Race == "Hispanic"],
             supervision90days_testing$predictions.fta.race[supervision90days_testing$D_Race == "Hispanic"]))

### NCA
roc.test(roc(supervision90days_testing$NCA_Charge[supervision90days_testing$D_Race == "White"],
             supervision90days_testing$predictions.nca.race[supervision90days_testing$D_Race == "White"]), 
         roc(supervision90days_testing$NCA_Charge[supervision90days_testing$D_Race == "Hispanic"],
             supervision90days_testing$predictions.nca.race[supervision90days_testing$D_Race == "Hispanic"]))

# Gender

## 30 Days

### FTA
roc.test(roc(supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Gender == "M"],
             supervision30days_testing$predictions.fta.gender[supervision30days_testing$D_Gender == "M"]), 
         roc(supervision30days_testing$FTA_Outcome[supervision30days_testing$D_Gender == "F"],
             supervision30days_testing$predictions.fta.gender[supervision30days_testing$D_Gender == "F"]))

### NCA
roc.test(roc(supervision30days_testing$NCA_Charge[supervision30days_testing$D_Gender == "M"],
             supervision30days_testing$predictions.nca.gender[supervision30days_testing$D_Gender == "M"]), 
         roc(supervision30days_testing$NCA_Charge[supervision30days_testing$D_Gender == "F"],
             supervision30days_testing$predictions.nca.gender[supervision30days_testing$D_Gender == "F"]))

## 90 Days

### FTA
roc.test(roc(supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Gender == "M"],
             supervision90days_testing$predictions.fta.gender[supervision90days_testing$D_Gender == "M"]), 
         roc(supervision90days_testing$FTA_Outcome[supervision90days_testing$D_Gender == "F"],
             supervision90days_testing$predictions.fta.gender[supervision90days_testing$D_Gender == "F"]))

### NCA
roc.test(roc(supervision90days_testing$NCA_Charge[supervision90days_testing$D_Gender == "M"],
             supervision90days_testing$predictions.nca.gender[supervision90days_testing$D_Gender == "M"]), 
         roc(supervision90days_testing$NCA_Charge[supervision90days_testing$D_Gender == "F"],
             supervision90days_testing$predictions.nca.gender[supervision90days_testing$D_Gender == "F"]))


