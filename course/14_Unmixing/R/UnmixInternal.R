#' Internal function for OldFashinedUnmix, handles fixing the 
#' formatting going from raw to unmixed .fcs file
#' 
#' @param ff The original raw flowFrame (used for the initial metadata)
#' @param data The updated data.frame following unmixing
#' @param panel Metadata from the signature matrix, containing
#'  Fluorophore and Antigen
#' 
#' @importFrom flowCore parameters keyword
#' @importFrom dplyr filter mutate row_number relocate pull
#' select
#' @importFrom tibble rownames_to_column column_to_rownames
# 
UnmixInternal <- function(ff, data, panel){

    # Identify the retained columns

    TheOriginalColumns <- colnames(ff)
    TheNewColumns <- colnames(data)
    RetainedColumns <- intersect(TheOriginalColumns, TheNewColumns)

    # Retrieve original parameters data with "$P" rownames

    TheOriginalParameters <- flowCore::parameters(ff)@data

    # Identify "$P" rownames to eliminate or keep

    KeepThese <- TheOriginalParameters |>
         dplyr::filter(name %in% RetainedColumns)
    GetRidThese <- TheOriginalParameters |> 
        dplyr::filter(!name %in% RetainedColumns) |> rownames()

    # Identify existing keyword names

    TheOriginalDescription <- flowCore::keyword(ff)
    OriginalKeywordNames <- names(TheOriginalDescription)

    # Identification of keywords containing "$P"

    Escaped <- gsub("\\$", "\\\\$", GetRidThese)
    RegexFormatted <- paste0("^", Escaped, "($|[^0-9])")
    RegexCombinatorial <- paste(RegexFormatted, collapse = "|")

    IdentifiedElimination <- OriginalKeywordNames[
        grepl(RegexCombinatorial, OriginalKeywordNames)]
    IdentifiedRetention <- OriginalKeywordNames[
        !grepl(RegexCombinatorial, OriginalKeywordNames)]

    # Identification of keywords containing "flowCore_$P"

    SecondRegexPattern <- paste0("(?<!\\d)", Escaped, "(?!\\d)")
    SecondRegexCombinatorial <- paste(SecondRegexPattern, collapse = "|")

    SecondElimination <- IdentifiedRetention[
        grepl(SecondRegexCombinatorial, IdentifiedRetention, perl = TRUE)]
    SecondRetention <- IdentifiedRetention[
        !grepl(SecondRegexCombinatorial, IdentifiedRetention, perl = TRUE)]

    # Identification of keywords containing "PDisplay"

    NoDollars <- sub("^\\$", "", GetRidThese)
    NoDollarsRegex <- paste0("(?<!\\d)\\$?", NoDollars, "(?!\\d)")
    NoDollarsCombined <- paste(NoDollarsRegex, collapse = "|")

    ThirdElimination <- SecondRetention[
        grepl(NoDollarsCombined, SecondRetention, perl = TRUE)]
    ThirdRetention <- SecondRetention[
        !grepl(NoDollarsCombined, SecondRetention, perl = TRUE)]

    # Subset original description for keyword names we want to retain

    IntermediateDescription <- TheOriginalDescription[ThirdRetention]

    # Renumbering retained SSC, FSC, SSC-B columns in parameters

    IntermediateParameters <- KeepThese |>
         tibble::rownames_to_column("OriginalRowNumber") |>
       dplyr::mutate(NewRowNumber=paste0("$P", dplyr::row_number())) |>
       relocate(NewRowNumber, .before=1)

    # Renumbering the retained keywords with new "$P" row numbers

    NewRowNumbers <- IntermediateParameters |>
         dplyr::pull(NewRowNumber)
    OriginalRowNumbers <- IntermediateParameters |>
         dplyr::pull(OriginalRowNumber)

    ForLoopDescription <- IntermediateDescription

    # For-loop to renumber the retained keywords

    for (i in seq_along(NewRowNumbers)) {
    
        NewX <- NewRowNumbers[i]
        NewXNum <- sub("^\\$P", "", NewX)

        OldX <- OriginalRowNumbers[i]
        OldXNum <- sub("^\\$P", "", OldX)

        # Rename matching keywords with new row numbers

        InternalEscaped <- gsub("\\$", "\\\\$", OldX)
        InternalRegexFormatted <- paste0(
            "(?<!\\d)", InternalEscaped, "(?!\\d)")
        InternalRegexCombinatorial <- paste(
            InternalRegexFormatted, collapse = "|")

        InternalIdentifiedRename <- names(ForLoopDescription)[
            grepl(InternalRegexCombinatorial, 
            names(ForLoopDescription), perl = TRUE)]

        ## Actual renaming in action

        RenamePattern <- paste0("^(flowCore_)?(\\$)?P", OldXNum, "(.*)$")
        InternalRenamed <- sub(RenamePattern,
         paste0("\\1\\2P", NewXNum, "\\3"), InternalIdentifiedRename)

        names(ForLoopDescription)[match(InternalIdentifiedRename,
         names(ForLoopDescription))] <- InternalRenamed

        # Matching for rename the "PDisplay" keywords
        InternalNoDollars <- sub("^\\$", "", OldX)   # "P18"
        InternalNoDollarsRegex <- paste0(
            "(?<!\\d)\\$?", InternalNoDollars, "(?!\\d)")

        ThirdRename <- names(ForLoopDescription)[
        grepl(InternalNoDollarsRegex, names(ForLoopDescription),
         perl = TRUE)]

        # Final renaming in action for "PDisplay"

        DisplayRenamePattern <- paste0("^(\\$)?P", OldXNum, "(.*)$")
        ThirdRenamed <- sub(DisplayRenamePattern, 
        paste0("\\1P", NewXNum, "\\2"), ThirdRename)

        names(ForLoopDescription)[match(ThirdRename,
         names(ForLoopDescription))] <- ThirdRenamed
    }

    # Add the new parameter rows for the Unmixed Fluorophores

    IntermediateParameters <- IntermediateParameters |>
         dplyr::select(-OriginalRowNumber) |>
         tibble::column_to_rownames("NewRowNumber")

    UpdatedParameters <- UnmixedParameterUpdate(OldParameters=IntermediateParameters, NewExprs=data)
    TheAntigens <- panel |> pull(Antigen)
    UpdatedParameters$desc <- TheAntigens
    pd <- rbind(IntermediateParameters, UpdatedParameters)

    # Generate new keywords for the new fluorophore rows in parameters

    new_pid <- rownames(UpdatedParameters)
    new_kw <- ForLoopDescription

    for (i in new_pid){
        NoDollarCode <- sub("^\\$", "", i) # For "PDisplay keyword"
        new_kw[paste0(i,"B")] <- new_kw["$P1B"] # Bytes
        new_kw[paste0(i,"E")] <- "0,0"
        new_kw[paste0(i,"N")] <- pd[[i,1]] # Fluorophore Name
        new_kw[paste0(i,"R")] <- pd[[i,5]] # Range Default
        new_kw[paste0(i,"S")] <- pd[[i,2]] # Antigen Name
        new_kw[paste0(i,"TYPE")] <- "Unmixed_Fluorescence"
        new_kw[paste0(i,"V")] <- "0" # Voltage/Gain, unmixed default 0
        new_kw[paste0("flowCore_", i,"Rmax")] <- pd[[i,5]] # maxRange
        new_kw[paste0("flowCore_", i,"Rmin")] <- pd[[i,4]] # minRange
        new_kw[paste0(NoDollarCode,"DISPLAY")] <- "LOG"
    }

    # Order Keywords by default sequence

    new_kw <- new_kw[order(names(new_kw))]

    # Overwrite old parameters "data" with the new parameters 

    OriginalParameterSlot <- flowCore::parameters(ff)
    OriginalParameterSlot@data <- pd

    # Convert unmixed data.frame to unmixed matrix

    UnmixedMatrix <- as.matrix(data)

    # Last keyword overrides
    new_kw$`CREATOR` <- "CytometryInR version 1.0.0"

    TheColumnNames <- colnames(data)
    TheSpilloverNames <- TheColumnNames[!grepl("Time|FSC|SSC", TheColumnNames)]
    MatrixSize <- length(TheSpilloverNames)
    NewMatrix <- matrix(0, nrow = MatrixSize, ncol = MatrixSize, byrow = TRUE)
    diag(NewMatrix) <- 1
    colnames(NewMatrix) <- TheSpilloverNames
    new_kw$`$SPILLOVER` <- NewMatrix

    new_fcs <- new("flowFrame", exprs=UnmixedMatrix, parameters=OriginalParameterSlot,
                 description=new_kw)

    return(new_fcs)
}


#' Internal for UnmixInternal, creates the new parameter
#' data rows needed to properly integrate new fluorophore columns
#' in exprs matrix
#' 
#' @param OldParameters The parameter data.frame with 
#' modified row numbers
#' @param NewExprs The unmixed data that will eventually 
#' be placed back into exprs
#' 
#' @importFrom Biobase pData
#' @importFrom dplyr pull select
#' @importFrom tidyselect all_of
#'  
UnmixedParameterUpdate <- function(OldParameters, NewExprs){

    # Remove the Retaind
    OldNames <- OldParameters |> dplyr::pull(name) |> unname()
    Overlapped <- intersect(OldNames, colnames(NewExprs))
    NewExprs <- NewExprs |> select(-all_of(Overlapped))

    # Create new rows for the unmixed fluorophore columns

	NewColumnLength <- ncol(NewExprs)
	NewColumnNames <- colnames(NewExprs)
	NewParameter <- max(as.integer(gsub("\\$P", "", rownames(OldParameters)))) + 1
	NewParameter <- seq(NewParameter, length.out = NewColumnLength)
	NewParameter <- paste0("$P", NewParameter)

    # Provide Hard Coded Numbers for Range, minRange and maxRange
    SSCRange <- OldParameters[2,3] # Hard-Coded based on Cytek Aurora SpectroFlo unmixed value
    MinRange <- -111.0001 # Hard-Coded based on Cytek Aurora SpectroFlo unmixed value
    MaxRange <- 4192505.7500 # Hard-Coded based on Cytek Aurora SpectroFlo unmixed value
	
	UpdatedParameters <- do.call(rbind,  lapply(NewColumnNames, function(i){
						vec <- NewExprs[,i]
						rg <- range(vec)
						data.frame(name = i,
                       desc = NA,
                       range = SSCRange,
                       minRange = MinRange,
                       maxRange = MaxRange)
					}))
          
	rownames(UpdatedParameters) <- NewParameter
	return(UpdatedParameters)
}