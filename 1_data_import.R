#### Load libraries ####
library(tidyverse)
library(haven)
library(here)
library(labelled)
#### Import Data####
#Check working directory
getwd()
#What files are there
list.files()
#list.files in /data
list.files(path="./data")
#read in data
#here returns the root directory
here()
#combining here() with 'data' and the file name returns the exact file path whether stored on a PC or Mac
#Store as full
full<-read_sav(here('data', 'ORD-571352-N3W7_Final_SPSS_022421.SAV'), encoding="utf8")
partial<-read_sav(here('data', 'ORD-571352-N3W7_Partial_SPSS_030221.SAV'))
#Check structure
str(full)
#View the data
#View(full)
#load the labelled library
library(labelled)
#check value labels

val_labels(full)

nrow(full)

#### export postal codes####
full %>% 
  distinct(Q65_1) %>% 
  mutate(fsa=tolower(Q65_1), fsa2=toupper(Q65_1)) %>% 
  select(fsa:fsa2) %>% 
  write_csv(., file=here("data", "fsa.csv"))
