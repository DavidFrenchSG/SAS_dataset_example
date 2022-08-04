#Very short example of R code to access SAS datasets, and output the weighted FBI


library(tidyverse)
library(haven)

datayear=2021
sampyear=2021

directorypath='//s0177a/sasdata1/ags/fas'
# list.files()
inputfile=paste(directorypath,"/so_y",datayear,"_fa.sas7bdat",sep='')
 inputfile
FarmAccount <- read_sas(inputfile)
names(FarmAccount) <- tolower(names(FarmAccount))
FarmAccount <- FarmAccount %>% 
  select(fa_id,type,fa_ifuel,fa_fbi)
weights <- read_sas("new_weights.sas7bdat")
names(weights)<-tolower(names(weights))
MergedData <- FarmAccount%>% 
  merge(weights,by="fa_id")
MergedData <- MergedData %>% 
  mutate(ys_year=(fa_id%%10000)) %>% 
  filter(ys_year==sampyear)
# View(MergedData)
mean(MergedData$fa_fbi)
weighted.mean(MergedData$fa_fbi,MergedData$fbswt)
