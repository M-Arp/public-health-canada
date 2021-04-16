#### Merge Region Health name #### 
#This file assigns FSAs to health regions with HR_UIDs and Health REgion names
health_regions<-read.csv(file="data/FSA to health region spatial join-2021 03 25.csv")
names(health_regions)
health_regions$HR
health_regions$HR_NAME
health_regions$HR_UID
health_regions$health_region
table(health_regions$health_region)
names(health_regions)
table(health_regions$PR)
#### Compare with covid19 dataset####
#Uncomment and run this to install the COVID19DATA
#devtools::install_github("ccodwg/Covid19CanadaData")
library(Covid19CanadaData)
#This file has covid case counts for health regions, without HRUIDs
covid<-dl_dataset(uuid='746c01f3-e597-4413-89e0-afa561bf81d8')

covid %>% 
  mutate(prov_hr=str_c(province, health_region, sep=" "))->covid
covid$prov_hr
table(covid$province)

#### Give ####
names(health_regions)
health_regions$PR
health_regions %>% 
  mutate(province=case_when(
    PR==10 ~ "NL",
    PR==11 ~ "PEI",
    PR==12 ~ "Nova Scotia",
    PR==13  ~ "New Brunswick",
    PR==24 ~ "Quebec",
    PR==35~ "Ontario",
    PR==46 ~ "Manitoba",
    PR==47 ~ "Saskatchewan",
    PR==48 ~ "Alberta",
    PR==59 ~ "BC",
    PR==60 ~ "Yukon",
    PR==61~ "NWT",
    PR==62 ~ "Nunavut"
  ))->health_regions

health_regions %>% 
  mutate(prov_hr=str_c(province, health_region, sep=" "))->health_regions

health_regions %>% 
  select(FSA, Comm_Name, prov_hr, HR_UID) %>% 
  distinct(FSA, .keep_all=T) ->health_regions

full %>% 
  left_join(., health_regions, by="FSA")->full

#### Merge Covid Case Count ####
qplot(full$date, geom='histogram')
covid$date<-dmy(covid$date_report)
names(covid)
#ovid<-select(covid, date, prov_hr, health_region, avgtotal_last7)

full %>% 
  left_join(., covid, by=c('prov_hr', 'date')) ->full
names(full)  
# 
# filter(duplicated(CID)) %>% 
# select(CID,FSA, date,  health_region,avgtotal_last7) %>% 
#   write_csv(., file="data/duplicates.csv")

