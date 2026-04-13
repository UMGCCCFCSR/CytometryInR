##### load packages -----
library(tidyverse)
# install.packages('ggbeeswarm')
library(ggbeeswarm)
library(forcats)

StorageLocation <- file.path("course", "06_Visualizing", "data")

TheCSV <- list.files(StorageLocation, pattern = ".csv", full.names = TRUE)
Data <- read.csv(TheCSV, check.names = FALSE)

########################
###### problem #1 ######
########################

###### create T cell associated variables ----
Data <- Data |>
  mutate(TcellProportion = Tcells_count / CD45_count) |>
  mutate(TcellFrequency = TcellProportion * 100) |>
  mutate(TcellFrequency = round(TcellFrequency, 1))

Data$timepoint <- factor(Data$timepoint)

Data$Condition <- factor(Data$Condition)
Data$Condition <- fct_relevel(Data$Condition, c("SEB", "Ctrl", "PPD")) #### using fct_relevel function from forcats package

  ##### plotting
  names(Data)
Plot <- Data |>
  ggplot() +
  aes(x = Condition, y = TcellFrequency) +
  geom_boxplot() +
  geom_beeswarm(
    size = 2.5,
    cex = 2.5,
    aes(shape = timepoint, color = timepoint) # indeed this could be done also by using a combination of geom_boxplot() + geom_jitter()
  ) 
Plot

########################
###### problem #2 ######
########################
library(CytoML)
library(ggcyto)

#### load files ---
# StorageLocation Defined Above
FlowJoWsp <- list.files(path = StorageLocation, pattern = ".wsp", full = TRUE)
ws <- open_flowjo_xml(FlowJoWsp)
gs <- flowjo_to_gatingset(
  ws = ws,
  name = 1,
  path = StorageLocation,
  additional.keys = "GROUPNAME"
)

#### plot ----
Plot2 <- ggcyto(gs[6], subset = "Tcells", aes(x = "CD8", y = "CD4")) +
  geom_hex(bins = 100)

# 1. Get all population paths from your GatingSet
all_nodes <- gs_get_pop_paths(gs)

# 2. Filter out the "root" (optional, usually index 1)
nodes_to_plot <- all_nodes[c(6,11,16)]

# 3. Plot 

#-- select the sample from the gating set
gh <- gs[[6]] 

#-- define the cell populations to show in the plot
nodes <- gs_get_pop_paths(gh, path = "auto")[c(3:9, 14)] 

#-- plot
Plot3 <- autoplot(gh, nodes, bins = 64)

#-- define the cell populations to show in the plot
nodes_test <- gs_get_pop_paths(gs, path = "auto")[16] 

#-- plot
Plot4 <- autoplot(gs, nodes_test, bins = 64) #### when I do this, I see that in the SEB sample there are several gates, why? Maybe there are several SEB files from other treatments. SO I have to discover how to separate them using IDs (are these contained within the gs object?)

########################
###### problem #3 ######
########################

#### Here I use the therm "fill" inside aesthetics to divide the histogram by the variable of interest
Data |> 
  ggplot(aes(x = TcellFrequency, fill = infant_sex)) +
  geom_density(alpha = 0.2, position = "identity") 

