#This script will contain code that prepares the dataset for analysis
####This line calls the code that loads the data####
source('1_data_import.R')
#Find the questions on science literacy
look_for(ph, 'tomatoes')
#Check value labels for Q14
val_labels(ph$Q14_1)

ph %>% 
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
  )->ph

#check ph$know1
ph$know1
ph$know2
ph$know3
ph$know4
#Now repeat that code for Q14_2, Q14_3, Q14_4
#This necessitates reading the text of the questions and setting the conditions <3, > 2 to return the correct value. 
#You can copy and paste the above code to repeat for Q14_2, Q14_3, Q14_4, but ideally, you would be able to nest it above, inside the mutate() command, so add lines for know2=case_when(), know3=case_when() , etc. etc. 
