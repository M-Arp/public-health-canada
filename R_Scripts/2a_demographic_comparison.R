#This script compares our genpop sample with 
source(here('R_Scripts', '2_data_preparation.R'))
#Read in demographic file
demographics<-read.csv(file=here('data', 'canada_demographics_comparison.csv'))

full %>% 
  filter(phase!=2) %>% 
  select(old, degree, female, francophone, rich) %>% 
  gather("variable") %>% 
  group_by(variable, value) %>% 
  summarize(n=n()) %>%   
  mutate(percent=(n/sum(n)*100)) %>% 
  ungroup() %>% 
  mutate(data_source=rep('sample', nrow(.))) %>% 
  filter(value==1) %>% 
  select(!(value:n)) %>% 
bind_rows(., demographics) ->sample_demographics
  
sample_demographics %>% 
  pivot_wider(., names_from=c(data_source), values_from=c(percent)) %>% 
  knitr::kable(., format="html") %>% 
  cat(., file=here("Tables", "comparison_with_census.html"))
sample_demographics %>% 
  ggplot(., aes(x=variable, y=percent, fill=data_source))+geom_col(position="dodge")

ggsave(here("Plots", "sample_census_comparison.png"))


