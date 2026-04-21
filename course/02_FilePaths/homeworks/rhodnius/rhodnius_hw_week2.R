### Take home problems:

####### Problem 1 -----

# create directory
dir.create(path = "/Volumes/RhodniusUSB/TargetFolder")

#### define paths
origin_dir <- "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile/data/"
dest_dir <- "/Volumes/RhodniusUSB/TargetFolder"

#### define full path
files_origin <- list.files(origin_dir, full.names = TRUE, recursive = FALSE, pattern = ".fcs")

##### Copy the files
file_copy(files_origin, dest_dir, overwrite = TRUE)

####### Problem 2 -----

list.files(origin_dir, full.names = TRUE, recursive = TRUE)

#generates:
#[1] "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile/data//AdditionalFCSFiles/2025_07_26_AB_02_NY068_02_Ctrl.fcs"
#[2] "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile/data//AdditionalFCSFiles/2025-10_22_Contrad.fcs"            
#[3] "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile/data//BioconductorDownloads.csv"                            
#[4] "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile/data//BioconductorFlow.csv"                                 
#[5] "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile/data//CellCounts4L_AB_05_ND050_05.fcs"                      

dirname(origin_dir)

# generates: 
#[1] "/Users/juancamilosanchezarcila/Flow_Cytometry/CytometryInR/course/03_InsideFCSFile"

####### Problem 3 -----

flowsom_files <- system.file("extdata", package = "FlowSOM")
list.files(flowsom_files, full.names = TRUE, recursive = TRUE, , pattern = ".fcs")

# the file present from FlowSom is:
#[1] "/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/FlowSOM/extdata/68983.fcs"


