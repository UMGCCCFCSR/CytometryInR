# Problem 1
# create gating set
library(flowWorkspace)
library(CytoML)
filepath <- file.path("data")
flowjopath <- file.path("data", "FlowJoWSP_Unopened.wsp")
fcs_files <- list.files(filepath, pattern=".fcs", full.names=TRUE)
cytoset <- load_cytoset_from_fcs(fcs_files)
workspace <- open_flowjo_xml(flowjopath)
gatingset <- flowjo_to_gatingset(ws=workspace, name=1, path=filepath, additional.keys="GROUPNAME")

# create data frame of cell counts per gate
popcount <- gs_pop_get_count_fast(gatingset)

# create % of parent column
library(dplyr)
mutated_popcount <- mutate(.data=popcount, percent_of_parent = 100*Count/ParentCount)
mutated_popcount <- mutate(.data=mutated_popcount, percent_of_parent = round(percent_of_parent, 1))
mutated_popcount |> head(10)

# Problem 2
# As long as the code includes a line to save a vector of file paths in the data folder with the .fcs extension, there were no errors. 
# However, if the vector of file paths is defined previously and files are later moved or deleted from the data folder, the load_cytoset_from_fcs function generates an error since it can't locate the files that were removed.

# Problem 3
library(ggplot2)
library(ggcyto)
bins10 <- ggcyto(gatingset[2], subset="CD4+", aes(x="TNFa", y="IFNg")) + geom_hex(bins=10)
bins10
# 10 bins is definitely not enough bins

bins100 <- ggcyto(gatingset[2], subset="CD4+", aes(x="TNFa", y="IFNg")) + geom_hex(bins=100)
bins100
# 100 bins can be interpreted but the dots are still a bit large

bins200 <- ggcyto(gatingset[2], subset="CD4+", aes(x="TNFa", y="IFNg")) + geom_hex(bins=200)
bins200
# 200 bins can be interpreted but the pseudocolor density axis is harder to see

bins500 <- ggcyto(gatingset[2], subset="CD4+", aes(x="TNFa", y="IFNg")) + geom_hex(bins=500)
bins500
# 500 bins is definitely too many bins. The dots are way too small and almost no density information
# 200 bins would be my preference but ultimately it depends on the resolution at which the plot is rendered.