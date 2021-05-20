source('R_Scripts/2_data_preparation.R')
lookfor(full, "rash")
full$C12
full$C34
full$C56
lookfor(full, "condition")
names(full)
full$PQ54
full$DV
full$C12
var_label(full)
full$STATUS
#Define Correct Responses

full %>% 
  mutate(case_when(
    
  ))
