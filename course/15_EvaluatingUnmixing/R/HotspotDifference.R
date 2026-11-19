#' Internal, visualizes changes between hotspot plots
#'
#' @param HotspotBrighter Brighter hotspot data.frame
#' @param HotspotDimmer Dimmer hotspot data.frame
#' @param savePlot TBD
#' @param outpath TBD
#' @param filename TBD
#' @param device TBD
#' @param width TBD
#' @param height TBD
#'
#' @importFrom dplyr mutate arrange filter
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2
#'   scale_x_discrete labs coord_fixed theme_minimal theme element_blank
#'   element_text ggsave
#'
#' @return The residual plot of the difference between the two hotspots
#'
#' @noRd
HotspotDifference <- function(HotspotBrighter, HotspotDimmer, returnType = "plot",
                        outpath = NULL, fileName = "HotspotDifference",
                        width = 15, height = 15) {

  TheseFluorophores <- HotspotBrighter[[1]]

  residuals <- HotspotBrighter[, -1] - HotspotDimmer[, -1]
  lower_tri <- as.matrix(residuals)
  lower_tri[upper.tri(lower_tri)] <- NA
  rownames(lower_tri) <- TheseFluorophores

  Longer <- as.data.frame(lower_tri) |>
  tibble::rownames_to_column("row") |>
  tidyr::pivot_longer(
    cols = -row, names_to = "col_label", values_to = "value"
  ) |>
  dplyr::mutate(
    row       = factor(row, levels = rev(TheseFluorophores)),
    col_label = factor(col_label, levels = TheseFluorophores)
  ) |>
    dplyr::filter(!is.na(value)) |>
    arrange(row)

  Plot <- ggplot(Longer, aes(x = col_label, y = row, fill = value)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = round(value, 2)),
      color = ifelse(
        Longer$value > max(Longer$value) / 2, "white", "black"
      ),
      size = 3, fontface = "bold"
    ) +
    scale_fill_gradient2(
      low = "blue2", high = "red2", mid = "white",
      midpoint = 0, na.value = "white", name = "Hotspot",
      limits = c(-max(abs(Longer$value)), max(abs(Longer$value)))
    ) +
    scale_x_discrete(position = "bottom") +
    labs(x = NULL, y = NULL) +
    coord_fixed() +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(), legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(hjust = 1)
    )

  if (returnType == "plot") {

    if (is.null(outpath)) {
      outpath <- getwd()
    }

    TheName <- paste0(fileName, ".png")

    FinalPath <- file.path(outpath, TheName)

    ggsave(
      filename = FinalPath, plot = Plot,
      width = width, height = height, units = "in",
      dpi = 300, bg = "white"
    )

  } else {
    return(lower_tri)
  }
}
