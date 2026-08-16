#' This function unmixes our raw full-stained .fcs files using the
#'  signature matrix we provide.
#' 
#' @param x A file.path to a raw full-stained .fcs file we want to 
#' unmix. 
#' @param retainThese Default "FSC|SSC|Time", used to separate out
#' columns not used for unmixing, but that should be retained for 
#' the final unmixed .fcs files. 
#' @param detectorExclude Default is "-H|-W", intended to remove 
#' additional detector columns other than -A, adjust as needed for 
#' your own instruments configuration
#' @param SignatureData A data.frame containing a Fluorophore,
#'  Antigen and Detector columns. 
#' @param returnType Default "fcs", for residual plots use "residual"
#' @param sample.name Keywords to pull for the new name, Cytek Aurora
#' default keywords are c("GROUPNAME", "TUBENAME"). 
#' @param addon Default "Unmixed", added right before the ".fcs"
#' @param outpath File.path to where we want to store the .fcs files, 
#' Default is NULL, which results in being saved to working directory
#' 
#' @importFrom flowCore read.FCS exprs write.FCS keyword
#' @importFrom dplyr select matches where pull bind_cols
#' @importFrom utils read.csv
#' 
OldFashionedUnmix <- function(x, retainThese="FSC|SSC|Time",
 detectorExclude="-H|-W", SignatureData, returnType="fcs", 
 sample.name=c("GROUPNAME", "TUBENAME"), addon="Unmixed",
 outpath=NULL){

    # Retrieve the underlying MFI values from exprs slot
    TheRawFCS <- flowCore::read.FCS(filename=x,
     transformation=FALSE, truncate_max_range = FALSE)
    TheRawMatrix <- flowCore::exprs(TheRawFCS)
    TheRawDataFrame <- data.frame(TheRawMatrix, check.names=FALSE)

    # Identify the detector columns
    StashedColumns <- TheRawDataFrame |>
         dplyr::select(dplyr::matches(retainThese)) 
    WorkingColumns <- TheRawDataFrame |>
         dplyr::select(!dplyr::matches(retainThese)) 
    WorkingColumns <- WorkingColumns |> 
        dplyr::select(!dplyr::matches(detectorExclude)) 
    # TheColNames <- colnames(WorkingColumns)
# Load the signature matrix

    if(is.data.frame(SignatureData)){
        Signatures <- SignatureData
    } else { 
        Signatures <-read.csv(SignatureData, check.names=FALSE)
    }

    Metadata <- Signatures |>
         dplyr::select(!dplyr::where(is.numeric))
    Numerics <- Signatures |>
         dplyr::select(dplyr::where(is.numeric))

    if (any(Numerics > 1)) {
        message("Signature values greater than 1 detected, normalizing")
        n <- Numerics
        # n[n < 0] <- 0
        A <- do.call(pmax, n)
        Normalized <- n/A
        Numerics <- Normalized
    }

    # Make sure dimensions match each other

    if (!all(colnames(Numerics) == colnames(WorkingColumns))){
        stop("colnames of SignatureData due not match the internal colnames of exprs")
    }

    DetectorNameBackups <- colnames(Numerics)
    TransposedSignatureValues <- t(Numerics)

    TransposedSampleValues <- t(WorkingColumns)

    # Unmixing 

    LeastSquares <- lsfit(x = TransposedSignatureValues,
     y = TransposedSampleValues, intercept = FALSE)

    # Residual Plot Fork

    if (returnType == "residuals"){
    Plot <- ResidualPlots(LeastSquaresList = LeastSquares)
    return(Plot)
    }

    # Reassembly
    TransposedLeastSquares <- t(LeastSquares$coefficients)

    FluorophoreNames <- Metadata |> dplyr::pull(Fluorophore)
    TheDetectorColNames <- colnames(WorkingColumns)
    AppendThisLetter <- sub("^[^-]*", "", TheDetectorColNames) |> unique()
    FluorophoreNames <- paste0(FluorophoreNames, AppendThisLetter)
    colnames(TransposedLeastSquares) <- FluorophoreNames

    # Bind the stashed Time, SSC and FSC columns to the new fluorophore columns
    UnmixedData <- dplyr::bind_cols(StashedColumns, TransposedLeastSquares)

    # Return properly formatted flowFrame

    new_fcs <- UnmixInternal(ff=TheRawFCS, data=UnmixedData, panel=Metadata)

    # Provide a new name to GUID and FIL

    if (length(sample.name) == 2){
        first <- sample.name[[1]]
        second <- sample.name[[2]]
        first <- flowCore::keyword(new_fcs, first)
        second <- flowCore::keyword(new_fcs, second)
        name <- paste(first, second, sep="_")
    } else {name <- flowCore::keyword(new_fcs, sample.name)}

    if (!is.null(addon)){name <- paste0(name, "_", addon)}

    AssembledName <- paste0(name, ".fcs")

    new_fcs@description$GUID <- AssembledName
    new_fcs@description$`$FIL` <- AssembledName

    if (is.null(outpath)) {outpath <- getwd()}

    fileSpot <- file.path(outpath, AssembledName)

    if (returnType == "fcs") {
        flowCore::write.FCS(new_fcs, filename = fileSpot, delimiter="#")
    } else {return(new_fcs)}
}