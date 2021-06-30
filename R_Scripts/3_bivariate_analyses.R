source('R_Scripts/2_data_preparation.R')
theme_set(theme_bw())


####TEST SCRIPTS####
full$Sample
lookfor(full, "federal government")
names (full)
  full %>%
    select (contains('Q2'))
  pivot_longer()
  
view()



###Q1 Most Important Problem#### 
names(full)
full %>% 
  select(Sample, Obesity:Race_inequality) %>% 
 pivot_longer(-Sample) %>% 
  group_by(Sample, name, value) %>%
  as_factor() %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(value!="Not Selected") %>% 
  ggplot(., aes(y=name, x=pct, fill=Sample))+geom_col(position="dodge")+scale_fill_grey()+labs(title=str_wrap("Percent Selecting Issues as Most Important Public Health Problem After COVID-19", width=40))
ggsave(here("Plots", "most_important_problem_group.png"), width=6, height=3)



##### Q2 Federal government managing COVID####
lookfor(full, "federal government")
full$Q2_1
ggplot(full, aes(x=Q2_1,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Very Poorly\n 7=Very Well", title="Satisfaction with Federal Government Preventing the Spread of COVID-19")
ggsave(here("Plots", "Federal_Government_Satisfaction_COVID_Spread.png"))

full$Q2_2
ggplot(full, aes(x=Q2_2,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Very Poorly\n 7=Very Well", title="Satisfaction with Federal Government Ensuring Speedy Access to Vaccines")
ggsave(here("Plots", "Federal_Government_Satisfaction_Access_Vaccines.png"))

full$Q2_3
ggplot(full, aes(x=Q2_3,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(x="1=Very Poorly\n 7=Very Well", title="Satisfaction with Federal Government Managing Economic Disruptions")
ggsave(here("Plots", "Federal_Government_Satisfaction_Economic_Disruptions.png"))

#### Q3 Influence #### 
lookfor(full, "influence")
full %>% 
  select(Sample,contains("does"), contains("should")) %>% 
  pivot_longer(., cols=-Sample) %>% 
  mutate(Condition=case_when(
    str_detect(name, pattern="does") ~ "Does Influence",
    str_detect(name, pattern="should") ~"Should Influence"
  )) %>% 
  mutate(name=str_remove_all(name, "_does|_should")) %>% 
  mutate(name=str_replace_all(name, pattern="_", replace=" ")) %>% 
  group_by(Sample, Condition, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100, error=sqrt((Percent*(100-Percent))/n)) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ggplot(., aes(y=Sample, x=Percent, fill=Sample, alpha=Condition))+geom_col(position="dodge")+labs(y="Influence")+scale_fill_discrete(limits=rev)+facet_wrap(~str_wrap(name, width=20))+scale_alpha_discrete(limits=rev)+geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))
ggsave(here("Plots", "influences_do_should_policy_sample.png"), width=6, height=2)

full %>% 
  select(Sample,contains("should")) %>% 
  pivot_longer(., cols=-Sample) %>% 
  mutate(name=str_replace_all(name, pattern="_|_|should", replace=" ")) %>% 
  mutate(name=str_trim(name)) %>% 
  group_by(Sample, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n), error=sqrt((Percent*(1-Percent))/n)) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ggplot(., aes(y=name, x=Percent, fill=Sample, group=Sample))+geom_col(position="dodge")+labs(y="Influence")+geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))

#ggsave(here("Plots", "influences_should_group.png"), width=6, height=2)

#### Q4 Influence #### 
lookfor(full, "influence")
full %>% 
  select(Sample,contains("does")) %>% 
  pivot_longer(., cols=-Sample) %>% 
  mutate(name=str_replace_all(name, pattern="_|_|does", replace=" ")) %>% 
  mutate(name=str_trim(name)) %>% 
  group_by(Sample, name, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)) %>% 
  filter(value==1) %>% 
  as_factor() %>% 
  ggplot(., aes(y=name, x=Percent, fill=Sample))+geom_col(position="dodge")+labs(y="Influence")+scale_fill_discrete(limits=rev)
ggsave(here("Plots", "influences_do_group.png"), width=6, height=2)

#### Q5 Views on CMOH ####
lookfor(full, "heard")
full$Q5
ggplot(full, aes(y=Sample, fill=as_factor(Q5)))+geom_bar(position="fill")+scale_fill_grey(name="Role of Chief Medical Office of Health")
ggsave(here("Plots", "cmoh_role_group.png"), width=8, height=2)

#### Q6 Trust in the Following ####
lookfor(full, "trust")
full %>% 
  #Pick the variables working with
  select(Q6_1:Q6_9, Sample) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Sample) %>% 
   group_by(Sample, name) %>% 
  #Summarize each group by calculating the mean of value, which was also created 
  #in the pivotting process, and the sd, the sample size, and calculate the se for each
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,5))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Cannot be trusted at all\n 5=Can be trusted a lot", title="How much do you trust each of the following?", x="")
ggsave(here('Plots', 'trust_groups.png'), width=6, height=2)

## ? ## How to change the x-axis names from Q6_9, Q6_8, etc. to "Chief Medical Officer..."



#### Q7 Rate Groups in terms of how well they prevent spread of COVID####
lookfor(full, "groups")
full %>% 
  #Pick the variables working with
  select(Q7_1:Q7_8, Sample) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  #Summarize each group by calculating the mean of value, which was also created 
  #in the pivotting process, and the sd, the sample size, and calculate the se for each
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,5))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Very poorly \n 5=Very well", title="How would you rate the following groups in preventing the spread of COVID-19?", x="")
ggsave(here('Plots', 'groups_preventing_spread.png'), width=6, height=2)

## ? ## How to change the x-axis names from Q7_7, Q7_8, etc. to "Public Health Professionals, etc."
## ? ## How to fix the title to word wrap

#### Q8 Difference Between Samples and Support For Measures #####
full %>% 
  select(starts_with('Q8_'), Sample) %>% 
  rename(`Mandatory Vaccines`=Q8_1, `Close Bars`=Q8_2, `Fine Non-Maskers`=Q8_3) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  #group_by(Sample) %>% 
  ggplot(., aes(x=name, y=value, fill=as_factor(Sample)))+geom_boxplot()+
  labs(title="Support For Interventions by Sample", x="Intervention")
ggsave(here('Plots', 'Interventions_by_sample.png'))

#### Correlation between Vaccine Severity and measures - NOT WORKING #### 

full %>% 
  select(starts_with('Q8_'), avgtotal_last7, Sample) %>% 
  pivot_longer(., cols=c(1,2,3)) %>% 
  # group_by(Sample, name) %>% 
  ggplot(., aes(x=avgtotal_last7, y=value, col=as_factor(Sample)))+geom_point()+facet_grid(~name)+geom_smooth(method="lm")

#### Q9 Show differences between genpop and public health on trade-offs ####
#Start with the dataframe
full %>% 
  #Pick the variables working with
  select(decline_economy:seniors_isolation, Sample) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Sample) %>% 
  #Convert to factor
  #as_factor() %>% 
  #form groups based on the variable Sample and the new variable name, which was created
  #In the pivotting process. 
  group_by(Sample, name) %>% 
  #Summarize each group by calculating the mean of value, which was also created 
  #in the pivotting process, and the sd, the sample size, and calculate the se for each
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  #an option here would be to pause this pipe, 
  # and replace the last pipe with a save out -> to some object 
  # like trade_off_Sample
  # But here we are just going right to graph. 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,10))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip()
ggsave(here('Plots', 'trade_off_group.png'), width=6, height=2)

####Q23 Vaccine Hesitancy ####
lookfor(full, "eligible")


##Need to rescale this... Not entirely sure what to do with 98 and 99 ##



####Q24 Increasing Federal Power ####
lookfor(full, "greater powers")
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_1)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Monitoring the spread of diseases")
ggsave(here("Plots", "Greater_Federal_Powers_Monitoring.png"), width=6, height=2)

ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_2)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Emergency response")
ggsave(here("Plots", "Greater_Federal_Powers_Emergency.png"), width=6, height=2)

ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_3)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Control over vaccine supply chain")
ggsave(here("Plots", "Greater_Federal_Powers_Vaccines.png"), width=6, height=2)

ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_4)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Population mental health")
ggsave(here("Plots", "Greater_Federal_Powers_Mental_Health.png"), width=6, height=2)

####Q25 Government Priority to ensure speedy recovery####

#### Q30 Views on science in policy ####
lookfor(full, "policy")
ggplot(full, aes(x=as.numeric(Q30_1), fill=Sample,..scaled..))+
  geom_density(alpha=0.5)+
  labs(x="1=Policy Dictated By Best Scientific Evidence\n 7=Policy Determined By Many Factors Including Scientific Evidence")
ggsave(here("Plots", "science_policy_group.png"))

####Q31 Experience providing scientific evidence to policymakers####

#### Q32-36 Trust ####
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q32)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Politicians Are Ready to Lie")
ggsave(here("Plots", "trust_politicians_lie_group.png"), width=6, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q33)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Trust in Ottawa")
ggsave(here("Plots", "trust_ottawa_group.png"), width=6, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q34)))+geom_bar(position="fill")+labs(y='Sample')+scale_fill_grey(name="Trust in Government To Waste")
ggsave(here("Plots", "trust_taxes_group.png"), width=6, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q35)))+
  geom_bar(position="fill")+labs(y='Sample', title="")+scale_fill_grey(name="Government is...")
ggsave(here("Plots", "trust_interests_group.png"), width=10, height=2)
ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q36)))+
  geom_bar(position="fill")+labs(y='Sample', title="Trust in People by Sample")+scale_fill_grey(name="Trust in people")
ggsave(here("Plots", "trust_people_group.png"), width=6, height=2)

####Q37 Individualism #####

####Q38 Equality & Discrimination####


####Q39 Institutions & Law####

####Q40 Free Will####


####Q51 Ideology ####
ggplot(full, aes(x=Q51, fill=Sample))+geom_density(alpha=0.5)+labs(title="Self-Reported Ideology by Sample")
ggsave(here("Plots", "ideology_group_density.png"))


#####  EXTRA Density plot for distribution of trust scores ####
ggplot(full, aes(x=trust_average,fill=Sample,..scaled..))+geom_density(alpha=0.5)+
  labs(title="Distribution of Average Trust Scores by Sample")
ggsave(here("Plots", "trust_average_group_density.png"))
