#### load packages ----
library (tidyverse)

#### load data ----

file_path <- "/Users/juancamilosanchezarcila/Flow_Cytometry/COURSE_keep_updated/CytometryInR/course/04_IntroToTidyverse/data/Dataset.csv"
path <- "/Users/juancamilosanchezarcila/Flow_Cytometry/COURSE_keep_updated/CytometryInR/course/04_IntroToTidyverse/data/"


raw_output <- read_csv(file_path)

# inspect tibble ----
View(raw_output)

###############################
##### Take home problems ######
###############################

### Problem 1 -----
names(raw_output)
View(raw_output)

# convert variables as factors
raw_output$timepoint <- as.factor(raw_output$timepoint)
raw_output$Condition <- as.factor(raw_output$Condition)=

# pipe with the filters
raw_output %>%
  rename(ID = bid) %>%
  ## select(1:3, 4, 18, 23, 25, 27) %>% # optional step if you are sure about the numbers of the vartiables (I like this one when handling several files)
  select(
    "ID",
    "timepoint",
    "Condition",
    "infant_sex",
    "Date",
    "Tcells",
    "CD4+",
    "CD8+",
    "Tcells_count"
  ) %>% # if you want to specify the whole name 
  relocate(Tcells_count, .after = Tcells) %>%
  filter(timepoint %in% c(0, 9)) %>%
  filter(infant_sex %in% "Male") %>% 
  arrange(desc(Date)) %>%
  write_csv(. , file = paste0(path,"export.csv") )

### Problem 2 -----
names(raw_output)
View(raw_output)

raw_output %>%
  mutate_at(7:31, round, digits = 2) %>% # here I specify the columns of interest
  write_csv(., file = paste0(path, "export_mutate_all.csv"))

### Problem 3 -----
names(raw_output)

# tidy alternative ---
raw_output %>%
  unite(filename, c(bid, timepoint), sep = "_")

# alternative # 2 ---
object1 <- raw_output %>%
  unite(id_new, c(bid, timepoint), sep = "_") %>%
  select(id_new)
dim(object1) #196

object2 <- raw_output %>%
  unite(id_new, c(bid, timepoint), sep = "_") %>%
  unique() %>%
  select(id_new)
dim(object2) #196

# my conclusion is that there is no duplicates

# alternative #2 to test
object1 %in% object2 #TRUE

# ALternative using paste0()

object3 <- paste0(raw_output$bid, raw_output$timepoint) 

length(object3) #196

#### final conclusion: there is no diplucated data
 