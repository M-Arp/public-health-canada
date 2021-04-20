
#### Move Graphs over to Dropbox####
file.copy(here("Plots", list.files("Plots")), to="~/Dropbox/Public_Health/Results/Graphs")

#### Move Recoded Data File ####


file.copy(here('data', str_extract(list.files(path="data"), "^recoded_data.+[0-9].sav?")), to="~/Dropbox/Public_Health/Data")
