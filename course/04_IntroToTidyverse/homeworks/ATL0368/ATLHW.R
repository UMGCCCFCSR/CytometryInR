#problem1
#taking a dataset, work toght the colums operating functions
select (), rename(), relocate()
#then filter() by conditions on 2 separated colums arrange in order
#export new file.

#problem2
#use mutate to create new colums or modify existing ones
#use round to get less significant digits
# use round to do proportions
#export this file

#problem3
#use mutate to convine colums
# bind, timepoint, condtion
paste0() to save as new colum name 


#problem1 + 2
#get a dataset from females and 27/07/2025
# open packages
library(dplyr)
#set proper location of folders
setwd("C:/Users/anjan/Downloads/R course/Week-4/04_IntroToTidyverse")
#create file path
thefilepath <- file.path("data","Dataset.csv")
#read the exel sheet
exel <- read.csv(file=thefilepath, check.names=FALSE)
#see how many rows and colums it has
dim(exel)
[1] 196  31
#or just to see number of rows
nrow(exel)
[1] 196
#see the colum in the sheet
colnames(exel)
[1] "bid"               "timepoint"         "Condition"         "Date"             
 [5] "infant_sex"        "ptype"             "root"              "singletsFSC"      
 [9] "singletsSSC"       "singletsSSCB"      "CD45"              "NotMonocytes"     
[13] "nonDebris"         "lymphocytes"       "live"              "Dump+"            
[17] "Dump-"             "Tcells"            "Vd2+"              "Vd2-"             
[21] "Va7.2+"            "Va7.2-"            "CD4+"              "CD4-"             
[25] "CD8+"              "CD8-"              "Tcells_count"      "lymphocytes_count"
[29] "Monocytes"         "Debris"            "CD45_count" 

# select infant_sex and date
subset <-exel |> select(infant_sex, Date, `CD8+`,Tcells_count)
# now i can see we have male and female, and all dates
# to select just female and the 29/07/2025
|> filter(Condition == "female")
|> filter(Condition == "29/07/2025")
subset <- exel |> 
  select(infant_sex, Date, `CD8+`, Tcells_count) |> 
  filter(infant_sex == "Female", Date == "29/07/2025")
#also to practise change name cd8+ to CD8poss
subset <-subset |> rename (CD8pos =`CD8+`)
#now i want colum names order to change
# actual infantsex, date, Cd8pos and Tcell count.
# new order infantsex, date, Tcell count, CD8pos
subset <- subset |> relocate(Tcells_count, .after=CD8pos)
? relocate(.data, ..., .before = NULL, .after = NULL)
subset <- subset |> relocate(Tcells_count, .before=CD8pos)
#then also CD8 and do calculation of proportion all cd8 from total Tcell count
mutate()
subset <- subset|> mutate(CD8proportion = Tcells_count / CD8pos)
# not possible to do this because for Cd8 is just the MFI not the count....
# ok then put the MFI instead of 0.273482591, just use 3 digits.
round()
subset <- subset |> round(CD8pos, 2) # error
#round() expects a numeric vector, not a whole data frame
# so do inside of mutate
subset <- subset |> mutate(CD8pos = round(CD8pos, 2))
#now save this new exel sheet
NewName <- paste0("HomeworkDataset", ".csv")
StorageLocation <- file.path("data", NewName)
write.csv(subset, StorageLocation, row.names=FALSE)

#problem3
exel <- read.csv(file=thefilepath, check.names=FALSE)
colnames(exel)
 [1] "bid"               "timepoint"         "Condition" .......
# use mutate to convine colums bind, timepoint, condtion
exel <- exel |> select (bid, timepoint, Condition)
#now I just have the 3 rows of interest
exel <- exel |> mutate(ID = paste0(bid, timepoint, Condition))
#this creates just a new colum but all names togheter
exel <- exel |> mutate(ID = paste(bid, timepoint, Condition, sep = "_"))
# ok now looks like ID : INF0627_9_SEB
# but now I want to delete the other colums: bid, timepoint and Conditions, to just have ID instead
# use select()
exel <- exel |> select(ID)
#now just have the colum all togheter !! :)

git add .
git commit -m "end homeworks"
git push