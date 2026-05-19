#' Concatenate Internal
#' 
#' @param x TBD
#' @param y TBD
#' @param metadata TBD
#' 
#' @importFrom dplyr filter bind_cols
#' 
KeywordAppend <- function(x, y, metadata) {
  df <- y
  rownames(metadata) <- NULL
  AddThisRow <- metadata |> filter(name %in% x)
  ExpandedData <- bind_cols(df, AddThisRow)
  return(ExpandedData)
}

#' Concatenate Internal
#' 
#' @param DictionaryList TBD
#' @param data TBD
#' 
#' @importFrom dplyr left_join select rename
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
#' 
KeywordTranslate <- function(DictionaryList, data) {

  for (Entry in DictionaryList) {
    ColumnName <- names(Entry)[1] 
    KeyName <- names(Entry)[2]

    data <- data |> dplyr::left_join(Entry, by = ColumnName) |>
      dplyr::select(-tidyselect::all_of(ColumnName)) |> dplyr::rename(!!ColumnName := !!rlang::sym(KeyName))
  }

  return(data)
}

#' Concatenate Internal
#' 
#' @param x TBD
#' @param data TBD
#' 
#' @importFrom dplyr select pull
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' 
#' 
ColumnToKeyword <- function(x, data){
  IndividualColumn <- data |> dplyr::select(tidyselect::all_of(x))
  
  if(!is.numeric(IndividualColumn)){ # Is not numeric
    Values <- IndividualColumn |> dplyr::pull(x) |> unique()

    Dictionary <- tibble::tibble(Values = Values, Values_Key = seq(1000, by = 1000, length.out = length(Values)))
    colnames(Dictionary) <- gsub("Values", x, colnames(Dictionary))
    return(Dictionary)
  } else { # Is numeric already
    Values <- IndividualColumn |> dplyr::pull(x) |> unique()
    Dictionary <- tibble::tibble(Values = Values, Values_Key = Values)
    colnames(Dictionary) <- gsub("Values", x, colnames(Dictionary))
    return(Dictionary)
  }
}

#' Concatenate Internal
#' 
#' @param flowFrame TBD
#' @param NewColumns TBD
#' 
#' @importFrom flowCore pData parameters
#' 
ParameterUpdate <- function(flowFrame, NewColumns){
	NewColumnLength <- ncol(NewColumns)
	NewColumnNames <- colnames(NewColumns)
	OldParameters <- pData(parameters(flowFrame))
	NewParameter <- max(as.integer(gsub("\\$P", "", rownames(OldParameters)))) + 1
	NewParameter <- seq(NewParameter, length.out = NewColumnLength)
	NewParameter <- paste0("$P", NewParameter)
	
	UpdatedParameters <- do.call(rbind, lapply(NewColumnNames, function(i){
						vec <- NewColumns[,i]
						rg <- range(vec)
						data.frame(name = i, desc = NA, range = diff(rg) + 1, minRange = rg[1], maxRange = rg[2])
					}))
	rownames(UpdatedParameters) <- NewParameter
	return(UpdatedParameters)
}

#' Concatenates together .fcs files present in the GatingSet on the
#'  basis of a given gate
#' 
#' @param gs A GatingSet object
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
#' @param desiredCols A vector containing the names of the columns from the pData metadata
#' that need to be added as keywords to the concatenated .fcs file. 
#' @param specimenIndex Which specimen in the GatingSet to use as the metadata
#' framework for the new fcs file. Default is set to 1. 
#' @param filename Desired name for the concatenated file, default is MyConcatenatedFCS
#' 
#' @importFrom flowCore pData parameters keyword exprs write.FCS
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom dplyr select bind_rows
#' @importFrom tidyselect all_of
#' @importFrom purrr map map2 flatten
#' 
Concatenate <- function(gs, subset, inverse.transform=TRUE, DownsampleCount,
  addon, StorageLocation=NULL, returnType="flowFrame", desiredCols,
  specimenIndex=1, filename="MyConcatenatedFCS"){

  Metadata <- flowCore::pData(gs)
  DesiredMetadata <- Metadata |> dplyr::select(tidyselect::all_of(desiredCols))

  dataFrameList <- purrr::map(.x=gs, subset=subset, .f=Downsampling,
   DownsampleCount=DownsampleCount, addon=addon, returnType="data.frame",
  inverse.transform=inverse.transform, StorageLocation=StorageLocation)

  TheFileNames <- DesiredMetadata |> dplyr::pull(name)

  ExpandedDataframes <- purrr::map2(.x=TheFileNames, .y=dataFrameList,
   .f=KeywordAppend, metadata=DesiredMetadata)

  CombinedData <- dplyr::bind_rows(ExpandedDataframes)

  NewData <- CombinedData |> dplyr::select(tidyselect::all_of(desiredCols))
  OldData <- CombinedData |> dplyr::select(!tidyselect::all_of(desiredCols))

  Dictionaries <- purrr::map(.x=desiredCols, .f=ColumnToKeyword, data=NewData)

  EventsInTheGate <- flowWorkspace::gs_pop_get_data(gs[[specimenIndex]], subset,
   inverse.transform=inverse.transform)
  flowFrame <- EventsInTheGate[[1, returnType = "flowFrame"]]
  OriginalParameters <- flowCore::parameters(flowFrame)
  OriginalDescription <- flowCore::keyword(flowFrame)

  NewKeywords <- purrr::flatten(Dictionaries)
  NewDescriptions <- c(OriginalDescription, NewKeywords)

  TranslatedNewData <- KeywordTranslate(data=NewData, DictionaryList=Dictionaries)

  NewDataMatrix <- as.matrix(TranslatedNewData)
  OldDataMatrix <- as.matrix(OldData)

  new_fcs <- new("flowFrame", exprs=OldDataMatrix, parameters=OriginalParameters,
                 description=NewDescriptions)

  NewParameters <- ParameterUpdate(flowFrame=new_fcs, NewColumns=NewDataMatrix)

  pd <- pData(parameters(new_fcs))
  pd <- rbind(pd, NewParameters)
  new_fcs@exprs <- cbind(exprs(new_fcs), NewDataMatrix)
  pData(parameters(new_fcs)) <- pd
  new_pid <- rownames(pd)
  new_kw <- new_fcs@description

  for (i in new_pid){
    new_kw[paste0(i,"B")] <- new_kw["$P1B"] #Unclear Purpose
    new_kw[paste0(i,"E")] <- "0,0"
    new_kw[paste0(i,"N")] <- pd[[i,1]]
    #new_kw[paste0(i,"V")] <- new_kw["$P1V"] # Extra Unclear Purpose
    new_kw[paste0(i,"R")] <- pd[[i,5]]
    new_kw[paste0(i,"DISPLAY")] <- "LIN"
    new_kw[paste0(i,"TYPE")] <- "Identity"
    new_kw[paste0("flowCore_", i,"Rmax")] <- pd[[i,5]]
    new_kw[paste0("flowCore_", i,"Rmin")] <- pd[[i,4]]
  }
  
  UpdatedParameters <- parameters(new_fcs)
  UpdatedExprs <- exprs(new_fcs)

  UpdatedFCS <- new("flowFrame", exprs=UpdatedExprs, parameters=UpdatedParameters, description=new_kw)

  AssembledName <- paste0(filename, ".fcs")
  UpdatedFCS@description$GUID <- AssembledName
  UpdatedFCS@description$`$FIL` <- AssembledName 
  #UpdatedFCS@description$CREATOR <- "CytometryInR_2026"
  #UpdatedFCS@description$GROUPNAME <- filename
  #UpdatedFCS@description$TUBENAME <- filename
  #UpdatedFCS@description$USERSETTINGNAME <- filename
  #Date <- Sys.time()
  #Date <- as.Date(Date)
  #UpdatedFCS@description$`$DATE` <- Date

  if (is.null(StorageLocation)){StorageLocation <- getwd()}

  StoreFCSFileHere <- file.path(StorageLocation, AssembledName)
  
  if (returnType == "fcs"){
    flowCore::write.FCS(UpdatedFCS, filename = StoreFCSFileHere, delimiter="#") # Write out .fcs file
  } else if (returnType == "data.frame"){
    return(Downsampled_DataFrame) #Return data.frame without metadata
  } else {
    return(UpdatedFCS) #All other criterias return a flowFrame with metadata
    }
}