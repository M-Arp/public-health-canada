#This script will contain code that prepares the dataset for analysis
####This line calls the code that loads the data####
source('1_data_import.R')

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
      #So when it less than three, it gets a 1
      #separate with a comma
    Q14_1< 3~ 1,
    #When it is greater than 2, it is equal to 0
    # separate with a comma
    Q14_1>2 ~ 0,
    #All other cases
    TRUE ~ 0
  ),
  know2=case_when(
    Q14_2 <3~0,
    Q14_2 >2~1,
    TRUE ~ 0 
  ),
  know3=case_when(
    Q14_3 <3~1,
    Q14_3 >2~0,
    TRUE ~ 0 
  ),
  know4=case_when(
    Q14_4 <3~0,
    Q14_4 >2~1,
    TRUE ~ 0 
  )

  )->full

#check full$know1
full$know1
full$know2
full$know3
full$know4


#### Next preparation####
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
  
#### What we need to do now is go through each variable Q18_1, Q19_1 and Q20_1 and code a new variable 1, for correct, 0 for not
#### If the underlying text variable is.
#### We're going to use the str_detect() function to look for the passages that are correct. 
#### Look at the table in the folder "Tables/CRT_resopnses.html"
#### It lists each CRT question, along with each text response and the number of text responses that have been 
#### proivided for each question. So, 1 was provided 210 times for question Q18_1.
#### We have to define **each** string found in this table as correct or incorrect. 
#### The correct answers for each CRT question are, in this order:
#### Q18: Anything representing two, or second. 
### Q9 Anything representing seven or seventh
#### Q20 Anything representing Emily or Emili
#### If the response is incorrect, it gets 0 if it is correct, it getes a zero

#### You can see what str_detect does this way
#It just returns TRUE if a variable in Q18 is "1" or not
str_detect(full$Q18_1, "1")

#### This code brings up a viewer where you can check the results of str_detect. 
full %>% 
  filter(str_detect(Q18_1, "1")) %>% 
  select(Q18_1) %>% 
  View()

#### OK, I'm glad I caught that. So, it'ts a bit greedy. Maybe we want to be a little careful. 
#### str_detect uses "regular expressions" which are ways of capturing strings and patterns in texts. 
#### You can google aroudn for regular expressions. 
#### You can match the beginning of a line with the ^ and the end of the line with $
#### So to match an *exact* string, you can just nest it like this. 
full %>% 
  filter(str_detect(Q18_1, "^1$")) %>% 
  select(Q18_1) %>% 
  View()
#### But if we scroll through the results of this, just using the 1, we see that basically it works pretty well
### It only captures responses that reflect first place. 
full %>% 
  filter(str_detect(Q18_1, "1")) %>% 
  select(Q18_1) %>% 
  View()
#### So we are going to put that in as a condition for the first case_when
#### And it will be set to 0 because that is an incorrect answer.

full %>% 
  mutate(crt1=
           case_when(
    ### here we define any case in Q18_1 that has the character 1 (and only 1) as incorrect
str_detect(Q18_1, "1")~ 0, 
#### If we go back to the file crt_responses.html the next big response is "2"
#### So we can match it in the same way. 
#### But this time, we will return 1 because 2 is a correct answer
str_detect(Q18_1, "2")~1), 
#### After doing the responses for Q18_1, we will have to move to Q19_1
crt2=
  case_when(
# This is where we will put the str_detect for the Q19_Question
    #8 is the correct answer, remember to separate lines with a comma
str_detect(Q19_1, "8") ~ 1
# str_detect(Q19_1, )~ 1
# This bracket closes the crt2 case_when
), 
#### This is the code section for Q20_1 and crt3
crt3=case_when(
  #### Emily is the correct answer
  #### Add other tests, remember to separate with a comma.
str_detect(Q20_1, 'Emily')~ 1
#This bracket clsoes the crt3 case_when
)
#this bracket closes the whole mutate command
)->full

#### Check
table(full$crt1)

#### This code produces a table that compares the results of the text responses with the numeric responses. 
#### It is a diagnostic tool 
cat(kable(table(full$Q18_1, full$crt1), format="html"), file=here("Tables", "crt1_diagnostics.html"))
cat(kable(table(full$Q19_1, full$crt2), format="html"), file=here("Tables", "crt2_diagnostics.html"))
cat(kable(table(full$Q20_1, full$crt3), format="html"), file=here("Tables", "crt3_diagnostics.html"))
