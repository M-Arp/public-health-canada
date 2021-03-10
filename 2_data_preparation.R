#This script will contain code that prepares the dataset for analysis
####Data Import####
source('1_data_import.R')

#### Science Literacy ####
#Find the questions on science literacy
look_for(full, 'tomatoes')
#Check value labels for Q14
val_labels(full$Q14_1)

full %>% 
  #We are mutating existing variables so the mutate command
  mutate(
    #We are making the variable know1 using the case_when function
    know1=case_when(
      #when Q14_1 is 1 or 2, then they got the question correct
      #So when it less than 3, it gets a 1
      #separate with a comma
    Q14_1<3 ~ 1,
    #When it is greater than 2, it is equal to 0
    # separate with a comma
    Q14_1>2 ~ 0,
    #All other cases
    TRUE ~ 0
  ),
    know2=case_when(
      #when Q14_2 is 1 or 2, then they got the question correct
      #So when it less than 3, it gets a 1
    Q14_2<3 ~ 1,
    #When it is greater than 2, it is equal to 0
    Q14_2>2 ~ 0,
    TRUE ~ 0
  ),
    know3=case_when(
    Q14_3>2 ~ 1,
    #when Q14_3 is 3 or 4, then they got the question correct
    #So when it more than 2, it gets a 1
    Q14_3<3 ~ 0,
    #when it is less than 3, it is equal to 0
    TRUE ~ 0
  ),
    know4=case_when(
    #when Q14_4 is 3 or 4, then they got the question correct
    #So when it more than 2, it gets a 1
    Q14_4>2 ~ 1,
    #when it is less than 3, it is equal to 0
    Q14_4<3 ~ 0,
    TRUE ~ 0  
  )
  )->full

#check full$know
full$know1
full$know2
full$know3
full$know4



#### Cognitive Reflection Questions####
# We need to code the correct answers to these questions as correct.
# This code spits out an html table that has all the responses in it. T/here are 229 unique responses. 
# 
library(knitr)
full %>% 
  select(Q18_1:Q20_1) %>% 
  pivot_longer(cols=1:3) %>% 
  group_by(name, value) %>% 
  summarize(n=n()) %>% 
pivot_wider(., names_from=c('name'), values_from='n') %>% 
  rename(., "response"=1) %>% 
  kable(., format="html") %>% 
  cat(., file=here("Tables", "CRT_responses.html"))
  
#Find the questions on crt
look_for(full, 'race')
#Check value labels for Q18
val_labels(full$Q18_1)

full %>% 
  #We are mutating existing variables so the mutate command
  mutate(
    #We are making the variable crt1 using the case_when function
    crt1=case_when(
     #When it is a variation of 2 or second, it is correct
      #separate with a comma
      Q18_1 == 2 ~ 1,
      Q18_1 == "2 nd" ~ 1,
      Q18_1 == "2e" ~ 1,
      Q18_1 == "2eme" ~ 1,
      Q18_1 == "2 iemes" ~ 1,
      Q18_1 == "2eme" ~ 1,
      Q18_1 == "2nd" ~ 1,
      Q18_1 == "Deuxieme" ~ 1,
      Q18_1 == "deuxième" ~ 1,
      Q18_1 == "sec place" ~ 1,
      Q18_1 == "second" ~ 1,
      Q18_1 == "Second." ~ 1,
      Q18_1 == "secondplac" ~ 1,
      Q18_1 == "2nd place" ~ 1,
      Q18_1 == "2nd Place" ~ 1,
      Q18_1 == "deuxieme" ~ 1,
      Q18_1 == "Deuxième" ~ 1,
      Q18_1 == "DEUXIÈME" ~ 1,
      Q18_1 == "deuxième8" ~ 1,
      Q18_1 == "SECOND" ~ 1,
      Q18_1 == "sexond" ~ 1,
      Q18_1 == "Second" ~ 1,
      Q18_1 == "2ieme" ~ 1,
      Q18_1 == " deuxième" ~ 1, 	
      #All other cases
      TRUE ~ 0
    ),
    crt2=case_when(
      #When it is a variation of 8 or eight, it is correct
      Q19_1 == 8 ~ 1,
      Q19_1 == "8 alive" ~ 1,
      Q19_1 == "8 left" ~ 1,
      Q19_1 == "8 moutons" ~ 1,
      Q19_1 == "8 sheep" ~ 1,
      Q19_1 == "8 vivants" ~ 1,
      Q19_1 == "Eight" ~ 1,
      Q19_1 == "eight" ~ 1,
      Q19_1 == " 8" ~ 1,
      Q19_1 == "8 Sheep" ~ 1,
      Q19_1 == "EIGHT" ~ 1,
      #All other cases
      TRUE ~ 0
    ),
    crt3=case_when(
      #When it is a variation of Emily, it is correct
      Q20_1 == "Emilie" ~ 1,
      Q20_1 == "Émilie" ~ 1,
      Q20_1 == "emille" ~ 1,
      Q20_1 == "Emily" ~ 1,
      Q20_1 == "Emily 9" ~ 1,
      Q20_1 == "Emily." ~ 1,
      Q20_1 == "Emily..." ~ 1,
      Q20_1 == "Emily27" ~ 1,
      Q20_1 == "emille" ~ 1,
      Q20_1 == "Emliy" ~ 1,
      Q20_1 == "Emiky" ~ 1,
      Q20_1 == "emilie" ~ 1,
      Q20_1 == "EMILIE" ~ 1,
      Q20_1 == "émilie" ~ 1,
      Q20_1 == "ÉMILIE" ~ 1,
      Q20_1 == "EMILTY" ~ 1,
      Q20_1 == "eMILY" ~ 1,
      Q20_1 == "EMILY" ~ 1,
      Q20_1 == "Emily’s" ~ 1,
      Q20_1 == "Emilys" ~ 1,
      Q20_1 == "emily" ~ 1,
      #All other cases
      TRUE ~ 0
    ),
    crt4=case_when(
      #When it is a variation of 0 or zero, it is correct
      Q21_1 == 0 ~ 1,
      Q21_1 == "0'" ~ 1,
      Q21_1 == "0 no dirt" ~ 1,
      Q21_1 == "C’est pad" ~ 1,
      Q21_1 == "None" ~ 1,
      Q21_1 == "Zero" ~ 1,
      Q21_1 == "zero dirt" ~ 1,
      Q21_1 == "zzero" ~ 1,
      Q21_1 == "nothing" ~ 1,
      Q21_1 == "none" ~ 1,
      Q21_1 == "NONE" ~ 1,
      Q21_1 == "zero" ~ 1,
      #All other cases
      TRUE ~ 0  
    )
  )->full

#check full$crt
full$crt1
full$crt2
full$crt3
full$crt4


#### This code produces a table that compares the results of the text responses with the numeric responses. 
#### It is a diagnostic tool 
cat(kable(table(full$Q18_1, full$crt1), format="html"), file=here("Tables", "crt1_diagnostics.html"))
cat(kable(table(full$Q19_1, full$crt2), format="html"), file=here("Tables", "crt2_diagnostics.html"))
cat(kable(table(full$Q20_1, full$crt3), format="html"), file=here("Tables", "crt3_diagnostics.html"))
cat(kable(table(full$Q21_1, full$crt4), format="html"), file=here("Tables", "crt4_diagnostics.html"))

#### We need to rename the variables Q1_
full %>% 
  select(Q1_1:Q1_9_SP) %>% 
 var_label()
#### Code Public Health Most Important Problem Respones ####
#### This code below assigns meaningful variable names to the most important problem variables

#Start with the dataframe
full %>% 
  #Select the most important problem question
  select(Q1_1:Q1_9) %>% 
  #Get the variable labels
  var_label() %>% 
  #Stack them
bind_rows() %>% 
pivot_longer(cols=everything()) %>% 
  #This gets the pattern Obesity, VAccine Hesitancy, etc, 
mutate(out=str_extract(value, pattern="\\s(.*?)\\s-")) %>%
  #This delets the remaining - 
  mutate(out=str_replace_all(out, pattern=" -", replace=""),
         #This trims whitespace
         out=str_trim(out, side='both')) %>% 
  #Dump this into a temporary data frame out
  select(out)->out
#Check
out$out

#### Assign variable names to the new variables 
#Start with the data frame
full %>% 
  #Select the questions that are the most important problem
  select(Q1_1:Q1_9) %>% 
  #Rename them with out$out from above
  rename_with(~out$out, everything()) %>% 
  #Bind the columns from the original `full` dataframe and add to them the new variables we just made
  bind_cols(full, .) ->full
View(full)
#Check 
names(full)
#### This code writes out the spreadsheet with the text responses 
#Encoding(full$Q1_9_SP)<-'UTF-8'
#full$Q1_9_SP<-iconv(full$Q1_9_SP, from="UTF-8", to="LATIN1")
library(readr)

# full %>%
#   mutate(other_problem_text=tolower(Q1_9_SP)) %>%
#   group_by(other_problem_text) %>%
#   summarize(n=n()) %>%
#   mutate(category=rep("", nrow(.))) %>%
# REMEMBER TO PUT THE FILENAME BACK IN; THIS IS ONLY FOR SIMON
#   write_excel_csv(file=here("data", ""))


