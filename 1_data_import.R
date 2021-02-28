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
#Store as ph
ph<-read_sav(here('data', 'ORD-571352-N3W7_Final_SPSS_022421.SAV'))
#Check structure
str(ph)
#View the data
#View(ph)
#load the labelled library
library(labelled)
#check value labels
val_labels(ph)


