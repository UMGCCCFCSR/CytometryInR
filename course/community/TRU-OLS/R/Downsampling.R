#' This function downsamples from a designated gate to our desired number
#' of cells, returning as a new .fcs file
#' 
#' @param x A GatingSet object, typically iterated in.
#' @param subset The gate from which to retrieve cell counts from 
#' @param inverse.transform Whether to revert values back to their
#' original untransformed values before export as an .fcs file, default
#' is set to TRUE
#' @param DownsampleCount The desired number of cells to downsample from
#' each gated population. If value is less than 1, subsets out the 
#' equivalent proportion from that specimen
#' @param addon An additional character value to add before .fcs in the GUID
#' keyword to tell the downsampled file apart from the original. 
#' @param StorageLocation A file.path to the folder you want to store the new downsampled
#' fcs file to. Default NULL results in .fcs file being stored in current working directory
#' @param returnType Whether to return as a "fcs" file (default), or "flowFrame" or "data.frame"
#' 
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore parameters keyword write.FCS
#' @importFrom Biobase exprs
#' @importFrom dplyr slice_sample
#'  
#' 
Downsampling <- function(x, subset, inverse.transform=TRUE, DownsampleCount,
  addon, StorageLocation=NULL, returnType="fcs"){
  EventsInTheGate <- flowWorkspace::gs_pop_get_data(x, subset,
   inverse.transform=inverse.transform)
  MeasurementData <- Biobase::exprs(EventsInTheGate[[1]])
  MeasurementDataFramed <- as.data.frame(MeasurementData, check.names = FALSE)

  if (DownsampleCount < 1) {
      Count <- nrow(EventsInTheGate) # Original Count
      Count <- as.numeric(Count) #Sanity Check on Value Type
      Count <- Count*DownsampleCount # Target Cells
      Count <- round(Count, 0)
      DownsampleCount <- Count # Over-writting DownsampleCount used for downsampling
    }

  Downsampled_DataFrame <- dplyr::slice_sample(MeasurementDataFramed, n = DownsampleCount, replace = FALSE)

  DownsampledMatrix <- as.matrix(Downsampled_DataFrame)

  flowFrame <- EventsInTheGate[[1, returnType = "flowFrame"]]
  OriginalParameters <- flowCore::parameters(flowFrame)
  OriginalDescription <- flowCore::keyword(flowFrame)

  OriginalName <- OriginalDescription$`GUID`
  UpdatedName <- paste0("_", addon, ".fcs")
  UpdatedGUID <- sub(".fcs", UpdatedName, OriginalName) #Swtiching out .fcs for Updated Name via the sub function
  OriginalDescription$`GUID` <- UpdatedGUID

  NewFCS <- new("flowFrame", exprs=DownsampledMatrix, parameters=OriginalParameters, description=OriginalDescription)

  if (is.null(StorageLocation)){StorageLocation <- getwd()}

  StoreFCSFileHere <- file.path(StorageLocation, UpdatedGUID)
  
  if (returnType == "fcs"){
    flowCore::write.FCS(NewFCS, filename = StoreFCSFileHere, delimiter="#") # Write out .fcs file
  } else if (returnType == "data.frame"){
    return(Downsampled_DataFrame) #Return data.frame without metadata
  } else {
    return(NewFCS) #All other criterias return a flowFrame with metadata
    }

}