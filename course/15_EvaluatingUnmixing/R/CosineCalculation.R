#' Takes a data.frame, returns the Cosine Matrix
#' 
#' @param data The data.frame object with a single character column
#' 
#' @importFrom dplyr select where pull
#' @importFrom lsa pull
#' 
CosineCalculation <- function(data){
     Names <- data |> select(!where(is.numeric))
     Numbers <- data |> select(where(is.numeric))
     NumbersTransposed <- t(Numbers)

     TheNames <- Names |> pull(Fluorophore)
     rownames(NumbersTransposed) <- NULL
     colnames(NumbersTransposed) <- TheNames

     CosineValues <- lsa::cosine(NumbersTransposed)
     CosineValues <- round(CosineValues, 3)
     return(CosineValues)
}
