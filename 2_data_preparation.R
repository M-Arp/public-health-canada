install.packages("labelled")
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
  )->ph

#check ph$know
ph$know1
ph$know2
ph$know3
ph$know4

#Now repeat that code for Q14_2, Q14_3, Q14_4
#This necessitates reading the text of the questions and setting the conditions <3, > 2 to return the correct value. 
#You can copy and paste the above code to repeat for Q14_2, Q14_3, Q14_4, but ideally, you would be able to nest it above, inside the mutate() command, so add lines for know2=case_when(), know3=case_when() , etc. etc. 

source('1_data_import.R')
#Find the questions on CRT
look_for(ph, 'race')
#Check value labels for Q18
val_labels(ph$Q18_1)

ph %>% 
  #We are mutating existing variables so the mutate command
  mutate(
    #We are making the variable CRT1 using the case_when function
    CRT1=case_when(
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
      #All other cases
      TRUE ~ 0
    ),
    CRT2=case_when(
      #When it is a variation of 8 or eight, it is correct
      Q19_1 == 8 ~ 1,
      Q19_1 == "8 alive" ~ 1,
      Q19_1 == "8 left" ~ 1,
      Q19_1 == "8 moutons" ~ 1,
      Q19_1 == "8 sheep" ~ 1,
      Q19_1 == "8 vivants" ~ 1,
      Q19_1 == "Eight" ~ 1,
      #All other cases
      TRUE ~ 0
    ),
    CRT3=case_when(
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
      #All other cases
      TRUE ~ 0
    ),
    CRT4=case_when(
      #When it is a variation of 0 or zero, it is correct
      Q21_1 == 0 ~ 1,
      Q21_1 == "0'" ~ 0,
      Q21_1 == "0 no dirt" ~ 0,
      Q21_1 == "C’est pad" ~ 0,
      Q21_1 == "None" ~ 0,
      Q21_1 == "Zero" ~ 0,
      Q21_1 == "zero dirt" ~ 0,
      Q21_1 == "zzero" ~ 0,
      Q21_1 == "nothing" ~ 0,
      #All other cases
      TRUE ~ 0  
    )
  )->ph

#check ph$CRT
ph$CRT1
ph$CRT2
ph$CRT3
ph$CRT4

