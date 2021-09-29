source('R_Scripts/2_data_preparation.R')
theme_set(theme_minimal())
# install.packages("crosstable")
# library(crosstable)
# install.packages("gmodels")
# library(gmodels)

###Q1 Most Important Problem#### 
names(full)
full %>% 
  select(Sample, Obesity:Race_inequality) %>% 
 pivot_longer(-Sample) %>% 
  group_by(Sample, name, value) %>%
  as_factor() %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  filter(value!="Not Selected") %>% 
  ggplot(., aes(y=name, x=pct, fill=Sample))+ geom_col(position="dodge")+theme(legend.position = "bottom")+
  labs(y="", x="Percentage", title=str_wrap("Issue as Most Important Public Health Problem After COVID-19", width=60))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "most_important_problem_group.png"), width=6, height=3)

##### Q2 Federal government managing COVID####

# full$Q2_1
# ggplot(full, aes(Q2_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
# scale_y_continuous(labels=scales::percent) +
# facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
# labs(y="Percentage of Respondents", x="1=Very Poorly\n 7=Very Well", title=str_wrap("Satisfaction with Federal Government Preventing the Spread of COVID-19", width =60))
# Take out the scale_fill_manual and play around with this
# +scale_fill_brewer(type="div", palette=5)
# Here is the help file
# ?scale_fill_brewer
# ggsave(here("Plots", "Federal_Government_Satisfaction_COVID_Spread2.png"))


####Means for Q2: Federal Government Satisfaction

full %>% 
  select(Q2_1:Q2_3, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,7))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Very Poorly\n 7=Very Well", title=str_wrap("Indicate how well you think the Federal Government has done...", width=40), x="")+
  scale_x_discrete(labels = c("Preventing the Spread of COVID-19", "Ensuring Speedy Access to Vaccines", "Managing Economic Disruptions"))
ggsave(here('Plots', 'Means_Satisfaction_Federal_Government.png'))

###Mean and Crosstabs for Q2_1
CrossTable(full$Q2_1, full$Sample)
aggregate(full$Q2_1, list(full$Sample), FUN=mean)

# full$Q2_2
# ggplot(full, aes(Q2_2, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Very Poorly\n 7=Very Well", title=str_wrap("Satisfaction with Federal Government Ensuring Speedy Access to Vaccines", width =60))
#   ggsave(here("Plots", "Federal_Government_Satisfaction_Access_Vaccines2.png"))
  
###Mean and Crosstabs for Q2_2
CrossTable(full$Q2_2, full$Sample)
 aggregate(full$Q2_2, list(full$Sample), FUN=mean)

# full$Q2_3
# ggplot(full, aes(Q2_3, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Very Poorly\n 7=Very Well", title=str_wrap("Satisfaction with Federal Government Managing Economic Disruptions", width =60))
# ggsave(here("Plots", "Federal_Government_Satisfaction_Economic_Disruptions2.png"))

###Mean and Crosstabs for Q2_3
CrossTable(full$Q2_3, full$Sample)
aggregate(full$Q2_3, list(full$Sample), FUN=mean)

##### Q2 Significance Tests ####
wilcox.test(Q2_1 ~ Sample, data=full)
wilcox.test(Q2_2 ~ Sample, data=full)
wilcox.test(Q2_3 ~ Sample, data=full)

#### Q3-Q4 Influence Combined #### 
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
  ggplot(., aes(y=Sample, x=Percent, fill=Sample, alpha=Condition))+geom_col(position="dodge")+
  labs(y="Influence")+scale_fill_discrete()+facet_wrap(~str_wrap(name, width=20))+
  scale_alpha_discrete(range =c(0.4, 1), limits=rev)+theme(legend.position = "bottom")+
  geom_errorbarh(aes(xmin=Percent-(1.96*error), xmax=Percent+(1.96*error)),height=0, position=position_dodge(0.9))+
  guides(fill=guide_legend(title=""))
ggsave(here("Plots", "influences_do_should_policy_sample.png"), width=8, height=6)
rlang::last_error()

####Q3 Influences that should affecting government decision-making ####

#### Q5 Views on CMOH ####
####NEED TO MUTATE X AXIS *100 FOR %
lookfor(full, "heard")
full$Q5
ggplot(full, aes(y=Sample, fill=as_factor(Q5)))+theme(legend.position = "bottom")+geom_bar(position="fill")+scale_fill_brewer(palette= "Set1", name="Role of Chief Medical Office of Health")+
  labs(y="", x="", title= str_wrap("In your view, which of the following best describes the role of your provincial Chief Medical Officer", width=60))+
  guides(fill=guide_legend(title="", ncol=2, byrow=TRUE))
ggsave(here("Plots", "cmoh_role_group.png"))

###Version 2????
full %>% 
  select(Sample, Q5) %>%
  mutate(Role=as_factor(Q5)) %>% group_by(Sample, Role) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  ggplot(., aes(y=Role, x=Percent, fill=Sample))+geom_bar(position="dodge", stat="identity")+labs(y="", x="", title= str_wrap("In your view, which of the following best describes the role of your provincial Chief Medical Officer", width=40))
ggsave(here("Plots", "cmoh_role_group2.png"), width=8, height=3)


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
  labs(y="1=Cannot be trusted at all\n 5=Can be trusted a lot", title=str_wrap("How much do you trust each of the following?", width=60), x="")+
  scale_x_discrete(labels = c("Chief Medical Officer of Health\nin your Province", "Chief Public Health Officer of Canada", "World Health Organization", "United States Centers of Disease\nControl and Prevention", "Chinese Centre for Disease Control\n and Prevention", "Provincial Government", "Federal Government"))
ggsave(here('Plots', 'trust_groups.png'))

####SIGNIFICANCE TEST
wilcox.test(Q6_1 ~ Sample, data=full)
wilcox.test(Q6_2 ~ Sample, data=full)
wilcox.test(Q6_3 ~ Sample, data=full)
wilcox.test(Q6_4 ~ Sample, data=full)
wilcox.test(Q6_5 ~ Sample, data=full)
wilcox.test(Q6_8 ~ Sample, data=full)
wilcox.test(Q6_9 ~ Sample, data=full)

#### Q7 Rate Groups in terms of how well they prevent spread of COVID####
lookfor(full, "groups")
full %>% 
  #Pick the variables working with
  select(Q7_1:Q7_8, Sample) %>% 
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
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,5))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Very poorly \n 5=Very well", x="", title= str_wrap("How would you rate the following groups in preventing the spread of COVID-19?", width=60)) +
  scale_x_discrete(labels = c("Young People", "Your Neighbours", "Senior Citizens", "Federal Government", "Provincial Government", "Municipal Government", "School Boards", "Public Health Professionals"))
ggsave(here('Plots', 'groups_preventing_spread.png'))

####Significance Testing
wilcox.test(Q7_1 ~ Sample, data=full)
wilcox.test(Q7_2 ~ Sample, data=full)
wilcox.test(Q7_3 ~ Sample, data=full)
wilcox.test(Q7_4 ~ Sample, data=full)
wilcox.test(Q7_5 ~ Sample, data=full)
wilcox.test(Q7_6 ~ Sample, data=full)
wilcox.test(Q7_7 ~ Sample, data=full)
wilcox.test(Q7_8 ~ Sample, data=full)

#### Q8 Difference Between Samples and Support For Measures #####
# full %>% 
#   select(starts_with('Q8_'), Sample) %>% 
#   rename(`Mandatory Vaccines`=Q8_1, `Close Bars`=Q8_2, `Fine Non-Maskers`=Q8_3) %>% 
#   pivot_longer(., cols=c(1,2,3)) %>% 
#   #group_by(Sample) %>% 
#   ggplot(., aes(x=name, y=value, fill=as_factor(Sample)))+geom_boxplot()+
#   labs(title="Support For Interventions", x="Intervention", y="")+
#   guides(fill=guide_legend(title=""))
# ggsave(here('Plots', 'Interventions_by_sample.png'))

####Means for Q8:Policy Preferences

full %>% 
  select(Q8_1:Q8_3, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,7))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly Oppose\n 7=Strongly Support", title=str_wrap("How strongly do you support or oppose the following to contain the COVID-19 pandemic in the short-term", width=40), x="")+
  scale_x_discrete(labels = c("It should be mandatory for all residents \n in Canada to get a vaccine that has been \napproved by Health Canada", "Bars and restaurants should be closed down", "People not wearing masks in public, \nindoor places should be fined"))
ggsave(here('Plots', 'Means_Support_Containment.png'))

# lookfor(full, "support or oppose")
# full$Q8_1
# ggplot(full, aes(Q8_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly oppose\n 7=Strongly support", title=str_wrap("It should be mandatory for all residents in Canada to get a vaccine that has been approved by Health Canada", width =60))
# ggsave(here("Plots", "Support_mandatory_vaccines2.png"))

###Mean and Crosstabs for Q8_1
CrossTable(full$Q8_1, full$Sample)
aggregate(full$Q8_1, list(full$Sample), FUN=mean)

# full$Q8_2
# ggplot(full, aes(Q8_2, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly oppose\n 7=Strongly support", title=str_wrap("Bars and restaurants should be closed down", width =60))
# ggsave(here("Plots", "Support_bars_closed2.png"))

CrossTable(full$Q8_2, full$Sample)
aggregate(full$Q8_2, list(full$Sample), FUN=mean)

# full$Q8_3
# ggplot(full, aes(Q8_3, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly oppose\n 7=Strongly support", title=str_wrap("People not wearing masks in public, indoor places should be fined", width =60))
# ggsave(here("Plots", "Support_mask_fines2.png"))

aggregate(full$Q8_3, list(full$Sample), FUN=mean)

##SIGNIFICANCE TEST
wilcox.test(Q8_1 ~ Sample, data=full)
wilcox.test(Q8_2 ~ Sample, data=full)
wilcox.test(Q8_3 ~ Sample, data=full)

#### Correlation between Vaccine Severity and measures - NOT WORKING #### 

# full %>% 
#   select(starts_with('Q8_'), avgtotal_last7, Sample) %>% 
#   pivot_longer(., cols=c(1,2,3)) %>% 
#   # group_by(Sample, name) %>% 
#   ggplot(., aes(x=avgtotal_last7, y=value, col=as_factor(Sample)))+geom_point()+facet_grid(~name)+geom_smooth(method="lm")

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
  ggplot(., aes(x=average, y=fct_recode(name, "Reduce Social Isoation"="social_isolation", "Reduce Seniors' Isolation"="seniors_isolation", "Keep Schools Open"="schools_open","Prevent Economic Decline"="decline_economy" ), col=Sample))+geom_point()+xlim(c(1,10))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(x="1=Stopping the spread of COVID-19 \n 10=Other considerations", y="Policy", title= str_wrap("In public health, it is often important to decide between accomplishing multiple outcomes, which outcome is more important to you?", width=60))

ggsave(here('Plots', 'trade_off_group.png'))

lookfor(full, "decline")
wilcox.test(social_isolation ~ Sample, data=full)
wilcox.test(seniors_isolation ~ Sample, data=full)
wilcox.test(schools_open ~ Sample, data=full)
wilcox.test(decline_economy ~ Sample, data=full)

####Q23 Vaccine Hesitancy ####
lookfor(full, "eligible")

###Need to rescale this... Not entirely sure what to do with 98 and 99 ##

####Q24 Increasing Federal Power ####

full %>% 
  select(Q24_1:Q24_4, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,4))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly Disagree\n 4=Strongly Agree", title=str_wrap("Would you disagree or agree to giving the federal government greater powers to do the following even if it would mean acting in an area of provincial responsibility?", width=40), x="")+
  scale_x_discrete(labels = c("Monitoring the spread of diseases", "Emergency response", "Control over vaccine supply chain","Population mental health"))
ggsave(here('Plots', 'Means_Federal_Powers.png'))

# lookfor(full, "greater powers")
# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_1)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Monitoring the spread of diseases", width = 60 )) +
#   guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "Greater_Federal_Powers_Monitoring.png"))
# 
# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_2)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Emergency response", width = 60 )) +
#   guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "Greater_Federal_Powers_Emergency.png"))
# 
# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_3)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Control over vaccine supply chain", width = 60 )) +
#   guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "Greater_Federal_Powers_Vaccines.png"))
# 
# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q24_4)))+geom_bar(position="fill")+labs(x="", y="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you disagree or agree to giving the federal government greater power to do the following even if it would mean acting in an area of provincial responsibility: Population mental health", width = 60 )) +
#   guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "Greater_Federal_Powers_Mental_Health.png"))

CrossTable(full$Q24_4, full$Sample)
aggregate(full$Q24_4, list(full$Sample), FUN=mean)

####Q25 Government Priority to ensure speedy recovery####

full %>% 
  select(Sample, Q25) %>%
  mutate(Role=as_factor(Q25)) %>% group_by(Sample, Role) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100) %>% 
  ggplot(., aes(y=Role, x=Percent, fill=Sample))+geom_bar(position="dodge", stat="identity")+labs(y="", x="", title= str_wrap("What should governments prioritize to ensure a speedy recovery?", width=40))
ggsave(here("Plots", "Government_Recovery.png"), width=8, height=3)

# lookfor(full, "recovery")
# full$Q25
# full %>% 
#   select(Sample, Q25) %>% 
#   mutate(Preference=as_factor(Q25)) %>% 
#   group_by(Sample, Preference) %>% 
#   summarize(n=n()) %>% 
#   mutate(Percent=n/sum(n)*100) %>% 
#   ggplot(., aes(y=Preference, alpha=Sample, x=Percent))+geom_col(position="dodge")+labs(y="", x="", title= str_wrap("What should governments prioritize to ensure a speedy recovery?", width=60))
# ggsave(here("Plots", "Government_Prioritize_Speedy_Recovery.png"), width=8, height=3)
# 
# ggplot(full, aes(y=Sample, fill=as_factor(Q25)))+theme(legend.position = "bottom")+geom_bar(position="fill")+scale_fill_brewer(palette= "Set1", name="Role of Chief Medical Office of Health")+
#   labs(y="", x="", title= str_wrap("What should governments prioritize to ensure a speedy recovery?", width=60))+
#   guides(fill=guide_legend(title="", ncol=2, byrow=TRUE))
# ggsave(here("Plots", "Government_Prioritize_Speedy_Recovery2.png"))

CrossTable(full$Q25, full$Sample)

#### Q30 Views on science in policy ####

full %>% 
  select(Sample, starts_with('Q30_')) ->check
check
check %>% 
  pivot_longer(cols=-Sample)->check
check
check %>%
  ggplot(., aes(x=value, group=Sample))+ geom_bar(aes(y = ..prop.., fill = Sample), stat="count", position="dodge") + 
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~name, ncol=3)+
  labs(y="Percentage of Respondents", x="1=Policy Dictated By Best Scientific Evidence\n 7=Policy Determined By Many Factors \n(Including Scientific Evidence)", title=str_wrap("How important do you think scientific evidence is in making public policy?", width =60))
  ggsave(here("Plots", "science_policy_bar.png"))

# lookfor(full, "policy")
# ggplot(full, aes(x=as.numeric(Q30_1), fill=Sample,..scaled..))+
#   geom_density(alpha=0.5)+
#   labs(y="", x="1=Policy Dictated By Best Scientific Evidence\n 7=Policy Determined By Many Factors Including Scientific Evidence", title= str_wrap("How important do you think scientific evidence is in making public policy?", width=60))+
#   guides(fill=guide_legend(title=""))
# gsave(here("Plots", "science_policy_group.png"))

aggregate(full$Q30_1, list(full$Sample), FUN=mean)

# full$Q30_1
# ggplot(full, aes(Q30_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) + scale_fill_brewer(palette="Set1")+ 
#   facet_grid(~Sample)+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Policy Dictated By Best Scientific Evidence\n 7=Policy Determined By Many Factors Including Scientific Evidence", title=str_wrap("How important do you think scientific evidence is in making public policy?", width =60))
# ggsave(here("Plots", "science_policy_group2.png"))

####Q31 Experience providing scientific evidence to policymakers####

#### Q32-36 Trust ####

full %>% 
  select(trust_politicians_lie:trust_people, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="0=Less Trusting\n 1= More Trusting", title=str_wrap("Trust Measures", width=40), x="")+
  scale_x_discrete(labels = c("Politicians", "Federal Government", "Wasteful Government Spending", "Democracy", "Other People"))
ggsave(here('Plots', 'Means_Trust.png'))

# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q32)))+geom_bar(position="fill")+
#   labs(y="", x="")+scale_fill_manual(values = c("dark red", "tomato", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Politicians are ready to lie to get elected", width = 60 )) +guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "trust_politicians_lie_group.png"))

CrossTable(full$Q32, full$Sample)
aggregate(full$Q32, list(full$Sample), FUN=mean)

# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q33)))+geom_bar(position="fill")+
#   labs(y="", x="")+scale_fill_manual(values = c("dark red", "tomato", "gray", "royal blue", "dark blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("How much of the time do you think you can trust the government in Ottawa to do what is right", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "trust_ottawa_group.png"))

aggregate(full$Q33, list(full$Sample), FUN=mean)

# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q34)))+geom_bar(position="fill")+
#   labs(y="", x="")+scale_fill_manual(values = c("tomato", "gray", "royal blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Do you think that people in government waste:", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "trust_taxes_group.png"))

aggregate(full$Q34, list(full$Sample), FUN=mean)

# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q35)))+geom_bar(position="fill")+
#   labs(y="", x="")+scale_fill_manual(values = c("tomato", "royal blue") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Would you say the government is...", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "trust_interests_group.png"))

aggregate(full$Q35, list(full$Sample), FUN=mean)

# ggplot(full, aes(y=as.factor(Sample), fill=as_factor(Q36)))+geom_bar(position="fill")+
#   labs(y="", x="")+scale_fill_manual(values = c("royal blue", "tomato") ,(name=""))+theme(legend.position = "bottom")+labs (title= str_wrap("Generally speaking, would you say that most people can be trusted", width = 60 ))+guides(fill = guide_legend(reverse=TRUE))
# ggsave(here("Plots", "trust_people_group.png"))

aggregate(full$Q35, list(full$Sample), FUN=mean)

####Q37 Individualism #####

full %>% 
  select(Q37_1:Q37_4, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,7))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("People in our society often disagree about how far to let individuals go in making decisions for themselves. How strongly do you agree or disagree with each of these statements?", width=40), x="")+
  scale_x_discrete(labels = c("Government programs - not free markets - \nare the best way to supply people with \nthe things they need", "People who make lots of money have a\n moral obligation to share it with others", "Sometimes government need to limit people's choices \n(e.g. ban smoking or require seatbelts) \nto keep them from hurting themselves", "The government should leave it entirely \nto the private sector to create jobs"))
ggsave(here('Plots', 'Means_Individualism.png'))

# lookfor(full, "society")
# ggplot(full, aes(Q37_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Government programs - not free markets - are the best way to supply people with the things they need", width =60))
# ggsave(here("Plots", "Government_Programs_Needed_Things2.png"))

CrossTable(full$Q37_1, full$Sample)
aggregate(full$Q37_1, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q37_2, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("People who make lots of money have a moral obligation to share it with others", width =60))
# ggsave(here("Plots", "Rich_obligation_share2.png"))

CrossTable(full$Q37_2, full$Sample)
aggregate(full$Q37_2, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q37_3, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Sometimes government need to limit people's choices (e.g. ban smoking or require seatbelts) to keep them from hurting themselves", width =60))
# ggsave(here("Plots", "Government_limiting_freedom2.png"))

CrossTable(full$Q37_3, full$Sample)
aggregate(full$Q37_3, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q37_4, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("The government should leave it entirely to the private sector to create jobs", width =60))
# ggsave(here("Plots", "Private_sector_jobs2.png"))

CrossTable(full$Q37_4, full$Sample)
aggregate(full$Q37_4, list(full$Sample), FUN=mean)

###Significance Testing###

wilcox.test(Q37_1 ~ Sample, data=full)
wilcox.test(Q37_2 ~ Sample, data=full)
wilcox.test(Q37_3 ~ Sample, data=full)
wilcox.test(Q37_4 ~ Sample, data=full)

####Q38 Equality & Discrimination####

full %>% 
  select(Q38_1:Q38_3, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,7))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("People in our society often disagree about issues of equality and discrimination. How strongly do you agree or disagree with each of these statements?", width=40), x="")+
  scale_x_discrete(labels = c("We need to dramatically reduce \ninequalities between the rich and the poor", "Discrimination against visible minorities \nis still a very serious problem in our society", "We need to do more to reduce inequalities \nbetween men and women"))
ggsave(here('Plots', 'Means_Equality.png'))

# lookfor(full, "society")
# ggplot(full, aes(Q38_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("We need to dramatically reduce inequalities between the rich and the poor", width =60))
# ggsave(here("Plots", "dramatically_reduce_inequality2.png"))

CrossTable(full$Q38_1, full$Sample)
aggregate(full$Q38_1, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q38_2, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Discrimination against visible minorities is still a very serious problem in our society", width =60))
# ggsave(here("Plots", "discrimination_against_minorities2.png"))

CrossTable(full$Q38_2, full$Sample)
aggregate(full$Q38_2, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q38_3, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("We need to do more to reduce inequalities between men and women", width =60))
# ggsave(here("Plots", "Reduce_gender_inequality2.png"))

CrossTable(full$Q38_3, full$Sample)
aggregate(full$Q38_3, list(full$Sample), FUN=mean)

####Significance Testing####
wilcox.test(Q38_1 ~ Sample, data=full)
wilcox.test(Q38_2 ~ Sample, data=full)
wilcox.test(Q38_3 ~ Sample, data=full)

####Q39 Institutions & Law####

full %>% 
  select(Q39_1:Q39_3, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,7))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("People often disagree about how important institutions should be in society. How strongly do you agree or disagree with each of these statements?", width=40), x="")+
  scale_x_discrete(labels = c("Authorities should impose stricter \npunishment on those who break the law", "Respect for authority should be \na fundamental value in our society", "Compared to regular citizens, First Nations \nhave too many special rights"))
ggsave(here('Plots', 'Means_Institutions.png'))

# lookfor(full, "society")
# ggplot(full, aes(Q39_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Authorities should impose stricter punishment on those who break the law", width =60))
# ggsave(here("Plots", "Impose_stricter_punishment2.png"))

CrossTable(full$Q39_1, full$Sample)
aggregate(full$Q39_1, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q39_2, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Respect for authority should be a fundamental value in our society", width =60))
# ggsave(here("Plots", "respect_authority_fundamental2.png"))

CrossTable(full$Q39_2, full$Sample)
aggregate(full$Q39_2, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q39_3, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Compared to regular citizens, First Nations have too many special rights", width =60))
# ggsave(here("Plots", "First_nations_rights2.png"))

CrossTable(full$Q39_3, full$Sample)
aggregate(full$Q39_3, list(full$Sample), FUN=mean)

####Significance Testing####
wilcox.test(Q39_1 ~ Sample, data=full)
wilcox.test(Q39_2 ~ Sample, data=full)
wilcox.test(Q39_3 ~ Sample, data=full)

####Q40 Free Will####

full %>% 
  select(Q40_1:Q40_3, Sample) %>% 
  pivot_longer(., cols=-Sample) %>% 
  group_by(Sample, name) %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=name, y=average, col=Sample))+geom_point()+ylim(c(1,7))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("People often disagree about how much control they have over their own lives. How strongly do you agree or disagree with each of these statements?", width=40), x="")+
  scale_x_discrete(labels = c("Most of the important things that \ntake place in life happen by random chance", "The course of our lives is largely \ndetermined by forces beyond our control", "The future is too uncertain for people \nto make ANY long-term plans"))
ggsave(here('Plots', 'Means_Control.png'))

# ggplot(full, aes(Q40_1, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("Most of the important things that take place in life happen by random chance", width =60))
# ggsave(here("Plots", "Important_things_random2.png"))

CrossTable(full$Q40_1, full$Sample)
aggregate(full$Q40_1, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q40_2, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("The course of our lives is largely determined by forces beyond our control", width =60))
# ggsave(here("Plots", "Lives_beyond_control2.png"))

CrossTable(full$Q40_2, full$Sample)
aggregate(full$Q40_2, list(full$Sample), FUN=mean)

# ggplot(full, aes(Q40_3, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(~Sample)+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
#   labs(y="Percentage of Respondents", x="1=Strongly Disagree\n 7=Strongly Agree", title=str_wrap("The future is too uncertain for people to make ANY long-term plans", width =60))
# ggsave(here("Plots", "Future_uncertain2.png"))

CrossTable(full$Q40_3, full$Sample)
aggregate(full$Q40_3, list(full$Sample), FUN=mean)

####Significance Testing####
wilcox.test(Q40_1 ~ Sample, data=full)
wilcox.test(Q40_2 ~ Sample, data=full)
wilcox.test(Q40_3 ~ Sample, data=full)

####Q51 Ideology ####

ggplot(full, aes(Q51, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  facet_grid(~Sample)+ theme(legend.position = "none)")+scale_fill_manual(values = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "gray80", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")) +
  labs(y="Percentage of Respondents", x="0=Liberal\n10=Conservative", title=str_wrap("Self-Reported Ideology", width =60))
ggsave(here("Plots", "ideology_group_density2.png"))

CrossTable(full$Q51, full$Sample)
aggregate(full$Q51, list(full$Sample), FUN=mean)

####Creating merged Charts ###

full %>% 
  select(Sample, starts_with('Q2_')) ->check
check
check %>% 
  pivot_longer(cols=-Sample)->check
check
check %>%
  ggplot(., aes(x=value, group=Sample))+ geom_bar(aes(y = ..prop.., fill = Sample), stat="count", position="dodge") + 
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~name, ncol=3)+
  labs(y="Percentage of Respondents", x="1=Very Poorly\n 7=Very Well", title=str_wrap("Satisfaction with Federal Government...", width =60))

full %>% 
  select(Sample, starts_with('Q2_')) ->check 
check
check %>% 
  pivot_longer(cols=-Sample)->check
check

labels <-c(Q2_1 = "Preventing the Spread\n of COVID-19", Q2_2 = "Ensuring Speedy Access\n to Vaccines", Q2_3 = "Managing Economic\n Disruptions")
check %>%
  ggplot(., aes(x=value, group = Sample)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  facet_grid(Sample~name, labeller=labeller(name=labels))+scale_fill_manual(values = c("dark red", "red2", "tomato", "grey60", "royal blue", "blue2", "dark blue"))+ theme(legend.position = "none)")+
  labs(y="Percentage of Respondents", x="1=Very Poorly\n 7=Very Well", title=str_wrap("Satisfaction with Federal Government...", width =60))


