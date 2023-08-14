#Very short example of R code to access SAS datasets, and output the weighted median FBI

#Import libraries. Tidyverse as normal, haven for importing sas files and spatstat for weighted stats.
library(tidyverse)
library(haven)
library(spatstat)

#Define input parameters
datayear <- 2022
sampyear <- 2022

#Input data folders
FBS_directory_path<- '//s0177a/sasdata1/ags/fas/'
agstemp_path <- '//s0177a/sasdata1/ags/census/agstemp/'

#Read in farm_account data
FBS_fa_data_file <- paste0("so_y", datayear,"_fa.sas7bdat")
FBS_fa_data <- tryCatch(
  {
    FBS_fa_data <- read_sas(FBS_fa_data_file)
  },
  error = function(e)
  {
    file.copy(paste0(FBS_directory_path, FBS_fa_data_file), getwd())
    return(read_sas(FBS_fa_data_file))
  }
)
#Basic cleaning of fa data - convert all column names to lower case and strip sas formatting
names(FBS_fa_data) <- tolower(names(FBS_fa_data))
for (x in colnames(FBS_fa_data)){
  attr(FBS_fa_data[[deparse(as.name(x))]], "format.sas")=NULL
}

#Process fa dataset
FBS_data_process <- FBS_fa_data %>% 
  select(fa_id,fa_fbi)

#Read in weights file
FBS_weights_file <- paste0("new_weights.sas7bdat")
FBS_weights <- tryCatch(
  {
    FBS_weights <- read_sas(FBS_weights_file)
  },
  error = function(e)
  {
    file.copy(paste0(FBS_directory_path, FBS_weights_file), getwd())
    return(read_sas(FBS_weights_file))
  }
)
##Basic data cleaning - convert all column names to lower case and strip sas formatting
names(FBS_weights) <- tolower(names(FBS_weights))
for (x in colnames(FBS_weights)){
  attr(FBS_weights[[deparse(as.name(x))]],"format.sas")=NULL
}

#Read in an agstemp file (not yet working)
FTEUnpaid_file <- paste0("FBS", datayear, "_FTEUnpaid_", sampyear-2000,".sas7bdat")
FTEUnpaid <- tryCatch(
  {
    FTEUnpaid <- read_sas(FTEUnpaid_file)
  },
  error = function(e)
  {
    file.copy(paste0(agstemp_path, FTEUnpaid_file), getwd())
    return(read_sas(FTEUnpaid_file))
  }
)

#Create a merged dataset with FBI and weights 
MergedData <- FBS_data_process%>% 
  left_join(FBS_weights, by="fa_id")
MergedData <- MergedData %>% 
  mutate(ys_year=(fa_id%%10000)) %>% 
  filter(ys_year==sampyear)

#Output some values
mean(MergedData$fa_fbi)
weighted.mean(MergedData$fa_fbi,MergedData$fbswt)
weighted.median(MergedData$fa_fbi, MergedData$fbswt, type=1)