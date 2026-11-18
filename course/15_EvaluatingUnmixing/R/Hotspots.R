#' Returns Unmixing Dependendent Spread Matrix to determine
#'  collinearity of a given panel
#' 
#' @param data The Signature Matrix
#' @param round_digits The number of significant digits to round to
#' @param outpath Defaul NULL, will go to working directory
#' @param fileName Default UnmixingDependentSpreadMatrix
#' 
#' @importFrom MASS ginv
#' 
Hotspots <- function(data, round_digits=2,
   outpath=NULL, fileName="UnmixingDependentSpreadMatrix",
   returnType="plot"){

    DatasetCosines <- CosineCalculation(data=data)
    TheseFluorophores <- rownames(DatasetCosines) 
    PseudoInverse <- MASS::ginv(DatasetCosines)
    Absolute <- abs(PseudoInverse)
    TheSQRT <- sqrt(Absolute)
    row.names(TheSQRT) <- TheseFluorophores
    Hotspots <- round(TheSQRT, round_digits)
  
    final_matrix <- Hotspots
    lower_tri <- final_matrix
    lower_tri[upper.tri(final_matrix)] <- NA 
    ReturnTri <- data.frame(lower_tri)
    colnames(ReturnTri) <- row.names(ReturnTri)
    ReturnTri <- ReturnTri |> 
        tibble::rownames_to_column("Fluorophore")
  
if(returnType == "plot"){

    # write.csv(ReturnTri, "UnmixingDependentHotspot.csv", row.names=FALSE)
 
    Longer <- as.data.frame(lower_tri) |> 
        tibble::rownames_to_column("row") |> 
        tidyr::pivot_longer(cols = -row, names_to = "col_num",
          names_prefix = "V",values_to = "value") |>
        mutate(row = factor(row, levels=rev(TheseFluorophores)),
              col = as.numeric(col_num),
              col_label = factor(TheseFluorophores[col],
              levels = TheseFluorophores)) |>
        filter(!is.na(value)) |> arrange(row)
  
    Plot <- ggplot(Longer, aes(x = col_label, y = row, fill = value)) +
     geom_tile(color = "white", linewidth = 0.3) +
     geom_text(aes(label = round(value, 2)), 
     color = ifelse(Longer$value > max(Longer$value)/2, "white", "black"),
          size = 3, fontface = "bold") +
     scale_fill_gradient(low = "white",high = "red2",
          na.value = "white",name="Hotspot") +
     scale_x_discrete(position = "bottom") + labs(x = NULL, y = NULL) +
     coord_fixed() + theme_minimal(base_size = 12) +
     theme(panel.grid = element_blank(),legend.position = "right",
     axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,),
     axis.text.y = element_text(hjust = 1))
  
    if (is.null(outpath)){outpath <- getwd()}
    TheFileName <- paste0(fileName, ".png")
    SaveAsThis <- file.path(outpath, TheFileName)
    ggsave(SaveAsThis, units ="in", width = 10, height = 10)

    } else {
    return(ReturnTri)
  }
}
