#### Week 5 ----

# Load packages ---
library(flowCore)
library(flowWorkspace)
# install.packages("lobstr") # CRAN
library(lobstr)
# BiocManager::install("CytoML") #Bioconductor
library(CytoML)
library(openCyto)
library("ggplot2")
library(ggcyto)
library(tidyverse)
library("devtools")
library("usethis")
library("patchwork")

# if (!require("BiocManager", quietly = TRUE)) {
#   install.packages("BiocManager")
# }

# BiocManager::install("openCyto")

##### Import gating set ---------

# Using what you learned last week in Introduction to Tidyverse, for the imported GatingSet, retrieve the data.frame from cell counts per gate and attempt to mutate a new column showing percent of the parent gate. Remember, this is intentionally tricky at this point, we will go over how to efficiently do this in a few weeks

# specify path of files ----

setwd(
  "/Users/juancamilosanchezarcila/Flow_Cytometry/COURSE_keep_updated/CytometryInR/"
)

path <- "/Users/juancamilosanchezarcila/Flow_Cytometry/COURSE_keep_updated/CytometryInR/"

file.path(path)

Folder <- file.path("course", "05_GatingSets", "data") # For Testing

# Folder <- file.path("data") # For Quarto Rendering

fcs_files <- list.files(Folder, pattern = ".fcs", full.names = TRUE)


### create flow frame
flowFrame <- read.FCS(
  filename = fcs_files[1],
  truncate_max_range = FALSE,
  transformation = FALSE
)

### create a flowset
flowSet <- read.flowSet(
  files = fcs_files,
  truncate_max_range = FALSE,
  transformation = FALSE
)

### create a cytoframe
cytoframe <- load_cytoframe_from_fcs(
  fcs_files[1],
  truncate_max_range = FALSE,
  transformation = FALSE
)

#### create a cytoset
cytoset <- load_cytoset_from_fcs(
  fcs_files,
  truncate_max_range = FALSE,
  transformation = FALSE
)

#### check size of objects
obj_size(flowFrame) #3.53Mb
obj_size(flowSet) #20.99Mb
obj_size(cytoframe) #5.4kb
obj_size(cytoset) #3.88kb

# if I want to convert from [heavy] flowFrame to [light] cytoframe
ConvertedToCytoframe <- flowFrame_to_cytoframe(flowFrame)

########################
###### gating sets #####
########################

# coming from flowset
GatingSet1 <- GatingSet(flowSet)

# coming from cytoset
GatingSet2 <- GatingSet(cytoset)


####### Reading data from FJ -----

FlowJoWsp <- list.files(path = Folder, pattern = ".wsp", full = TRUE)

ThisWorkspace <- FlowJoWsp[stringr::str_detect(FlowJoWsp, "Opened")]

#### make the intermediate object ---

ws <- open_flowjo_xml(ThisWorkspace)

gs <- flowjo_to_gatingset(
  ws = ws,
  name = 1,
  path = Folder,
  additional.keys = "GROUPNAME"
)


## Check if the gates from flowjo were preserved after data inclusion

#### to see the gating strategy from FJ
plot(gs) 

# to see the paths (gating strategy) from the gating strategy
gs_get_pop_paths(gs)

#### retrieve counts
gs_pop_get_count_fast(gs)

#### see metadata from Gating Sets
pData(gs)


##### Start plotting ---
ggcyto(gs[1], subset="root", aes(x="FSC-A", y="SSC-A")) + geom_hex(bins=100)
ggcyto(gs[1], subset="CD4+", aes(x="FSC-A", y="SSC-A")) + geom_hex(bins=100) 
ggcyto(gs[1], subset="Live", aes(x="FSC-A", y="SSC-A")) + geom_hex(bins=100) 
ggcyto(gs[6], subset = "Tcells", aes(x = "CD4", y = "CD8")) + geom_hex(bins = 100)

# View(installed.packages())

#############################
######### Problem 1 #########
#############################

#### retrieve counts
cell_count  <- gs_pop_get_count_fast(gs)

View(cell_count)
names(cell_count)

cell_count <-  cell_count |>
  mutate(frequency_parent = (Count * 100) / ParentCount)

#############################
######### Problem 2 #########
#############################

ThisWorkspace_HW <- FlowJoWsp[stringr::str_detect(FlowJoWsp, "Opened")]

ws_2 <- open_flowjo_xml(ThisWorkspace_HW)
gs_2 <- flowjo_to_gatingset(
  ws = ws_2,
  name = 1,
  path = Folder,
  additional.keys = "GROUPNAME"
)

#### When I deleted the samples, I got the following message:
# FCS not found for sample 00_Ctrl.fcs_INF052 from searching the file extension: .fcs
# FCS not found for sample 00_Ctrl.fcs_INF100 from searching the file extension: .fcs
# FCS not found for sample 00_Ctrl.fcs_INF179 from searching the file extension: .fcs

#############################
######### Problem 3 #########
#############################
gs_get_pop_paths(gs)

ggcyto(gs[6], subset = "Tcells", aes(x = "CD4", y = "CD8")) +
  geom_hex(bins = 100)

nodes <- gs_get_pop_paths(gs)[c(5,6,11,16)]

ggcyto(gs, subset = "Tcells", aes(x = "IFNg", y = "TNFa")) +
  geom_hex(bins = 100)

plot1 <- ggcyto(gs, subset = nodes[1], aes(x = "IFNg", y = "TNFa")) +
  geom_hex(bins = 100) +
  facet_grid(name ~ .)

plot2 <- ggcyto(gs, subset = nodes[2], aes(x = "IFNg", y = "TNFa")) +
  geom_hex(bins = 100) +
  facet_grid(name ~ .)

plot3 <- ggcyto(gs, subset = nodes[3], aes(x = "IFNg", y = "TNFa")) +
  geom_hex(bins = 100) +
  facet_grid(name ~ .)

plot4 <- ggcyto(gs, subset = nodes[4], aes(x = "IFNg", y = "TNFa")) +
  geom_hex(bins = 100) +
  facet_grid(name ~ .)

library("ggpubr")
library("gridExtra")

grid.arrange(plot1, plot2, plot3, plot4, ncol=1)

View(plot1)

### I was not able to plot the arrage of plots.
