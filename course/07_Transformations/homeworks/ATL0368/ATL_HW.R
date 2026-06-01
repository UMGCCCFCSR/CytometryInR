#Problem 1
#We had not selected FSC and SSC parameters in this attempt, 
# as they are normally displayed in the linear scale. 
# Include them in the list of fluorophores to be transformed, 
# and see how this impacts the visualization 
# (imitating what could accidentally happen in practice if they were left in)

#Problem 2
#For the SFC data, I showed the setup for both Logicle and Biexponential,
#  but didn’t have time to dive into the Logicle transformation. 
# Select a couple markers of interest for the SFC data, 
# visualize and screenshot the before, and then attempt to customize 
# the biexponential arguments to best visualize the underlying data, 
# and then repeat for Logicle. Take screenshots of both and compare/contrast 
# the difference.

#Problem 3
#There are to asinh style transformations provided by the flowWorkspace package.
#Using the mass cytometry data, select two metal markers of interest,
# visualize each, customize the arguments until you have properly
# visualized the underlying populations, and see if you can spot any 
# major differences between the methods.


problem 1
#load all packages
library(flowWorkspace)
library(ggcyto)
library(dplyr) 
library(stringr)

#then location and files
folder_uni <- file.path("07_Transformations", "data")
StorageLocation <- file.path("data") 
list.files(folder_uni)
fcs_files <- list.files(folder_uni, ".fcs", full.names=TRUE)
#select files of interest
SFC_files <- fcs_files[stringr::str_detect(fcs_files, "2025")]

SFC_cytoset <- load_cytoset_from_fcs(SFC_files, truncate_max_range = FALSE, transformation = FALSE)
# no transformation
SFC_GatingSet <- GatingSet(SFC_cytoset)
Metadata <- pData(SFC_GatingSet)

#see list of colums
SFC_Parameters <- colnames(SFC_GatingSet)
# just take out time
Parameters_wrong <- SFC_Parameters[!stringr::str_detect(SFC_Parameters, "Time")]
Parameters_wrong 
#
 [1] "SSC-W"             "SSC-H"             "SSC-A"             "FSC-W"             "FSC-H"            
 [6] "FSC-A"             "SSC-B-W"           "SSC-B-H"           "SSC-B-A"           "BUV395-A"         
[11] "BUV563-A"          "BUV615-A"          "BUV661-A"          "BUV737-A"          "BUV805-A"         
[16] "Pacific Blue-A"    "BV480-A"           "BV570-A"           "BV605-A"           "BV650-A"          
[21] "BV711-A"           "BV750-A"           "BV786-A"           "Alexa Fluor 488-A" "Spark Blue 550-A" 
[26] "Spark Blue 574-A"  "RB613-A"           "RB705-A"           "RB780-A"           "PE-A"             
[31] "PE-Dazzle594-A"    "PE-Cy5-A"          "PE-Fire 700-A"     "PE-Fire 744-A"     "PE-Vio770-A"      
[36] "APC-A"             "Alexa Fluor 647-A" "APC-R700-A"        "Zombie NIR-A"      "APC-Fire 750-A"   
[41] "APC-Fire 810-A"    "AF-A"
# so SSC and FCS prsent
Biexponential <- flowjo_biexp_trans()
TransformBiex_wrong <- transformerList(Parameters_wrong, Biexponential)

#before tranformation
ggcyto(SFC_GatingSet[2], subset="root",
       aes(x="FSC-A", y="SSC-A")) +
  geom_hex(bins=100)

# apply transformation (including FSC/SSC — WRONG)
transform(SFC_GatingSet, TransformBiex_wrong)

ggcyto(SFC_GatingSet[2], subset="root",
       aes(x="FSC-A", y="SSC-A")) +
  geom_hex(bins=100)

#looks quite ok, change other parameters
Biexponential_bad <- flowjo_biexp_trans(
  channelRange = 4096,
  maxValue = 262144,
  pos = 4.5,
  neg = 0,
  widthBasis = -10)
Transform_bad <- transformerList(Parameters_wrong, Biexponential_bad)

#problem 2
# for logaritmic tranformation
LogTransform <- logicle_trans()   # better than pure log

Transform_log <- transformerList(Parameters_wrong, LogTransform)

# reload again
SFC_cytoset <- load_cytoset_from_fcs(
  SFC_files,
  truncate_max_range = FALSE,
  transformation = FALSE)

SFC_GatingSet <- GatingSet(SFC_cytoset)

transform(SFC_GatingSet, Transform_log)

ggcyto(SFC_GatingSet[2], subset="root",
       aes(x="FSC-A", y="SSC-A")) +
  geom_hex(bins=100)

# also you can change some parameteres in logical
?logicle_trans(w, t, m, a)
#w (width of linear region) controls how much space around zero is “linear”
# t (top of scale) max data value (instrument-dependent)
# m (number of decades) how much compression on the positive side
# a (additional negative range) how far into negatives you go

Logicle_custom <- logicle_trans( w = 0.5,   t = 262144,  m = 4.5,  a = 0)

logicle_trans(w = 2, t = 262144, m = 4.5, a = 0)

#problem 3 for mass cytometry not interesed on this. 

#end of the homeworks
git add .
git commit -m "end homeworks"  
git push           