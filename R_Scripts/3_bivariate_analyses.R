#### Show differences between genpop and public health on trade-offs ####
#Start with the dataframe
full %>% 
  #Pick the variables working with
  select(decline_economy:seniors_isolation, phase) %>% 
  #pivot them longer, except for the phase variable
pivot_longer(., cols=-phase) %>% 
  #Convert to factor
  as_factor() %>% 
  #form groups based on the variable phase and the new variable name, which was created
  #In the pivotting process. 
  group_by(phase, name) %>% 
  #Summarize each group by calculating the mean of value, which was also created 
  #in the pivotting process, and the sd, the sample size, and calculate the se for each
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  #an option here would be to pause this pipe, 
  # and replace the last pipe with a save out -> to some object 
  # like trade_off_phase
  # But here we are just going right to graph. 
  ggplot(., aes(x=name, y=average, col=phase))+geom_point()+ylim(c(1,10))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip()
ggsave(here('Plots', 'trade_off_phase.png'), width=6, height=2)
