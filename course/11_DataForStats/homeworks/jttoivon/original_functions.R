#' Helper function that wraps the various statistical test to
#' enable quick statistical outputs for our extracted data
#' 
#' @param data The data.frame or tibble object containing our dataset
#' @param theColumn The column containing our measurements of interest
#' @param theFactor The column containing our metadata to compare by
#' @param parametric Whether to use parametric or non-parametric test
#' @param alpha Default set to 0.05
#' @param correction Method for FDR correction, default is "none"
#' 
StatsFromFlow <- function(data, theColumn, theFactor, parametric,
 alpha=0.05, correction="none"){

     #theColumn <- x
     GateName <- theColumn
     FactorName <- theFactor

     FactorLevels <- data[[theFactor]] |> unique() |> length()

     if (parametric=="parametric" && FactorLevels == 2){

          # message("parametric with 2-levels")
          Results <- tidy(t.test(data[[theColumn]] ~ data[[theFactor]],
          alternative = "two.sided", var.equal = TRUE))

     } else if (parametric=="non-parametric" && FactorLevels == 2){

          # message("non-parametric with 2-levels")
          Results <- tidy(wilcox.test(data[[theColumn]] ~ data[[theFactor]],
          alternative = "two.sided", exact=FALSE))

     } else if (parametric=="parametric" && FactorLevels == 3){
          # message("parametric with 3-levels")
          Results <- tidy(aov(data[[theColumn]] ~ data[[theFactor]], data = data))
          Results <- Results[1,]
          Results <- Results |> mutate(method="One-Way Anova")

          if (!is.na(Results$p.value) && Results$p.value <= alpha){
               Pairwise <- tidy(pairwise.t.test(data[[theColumn]], data[[theFactor]],
                                   p.adjust.method = correction))
               Pairwise$method <- "Pairwise t-test"
               Results <- Pairwise
          } 

     } else if (parametric=="non-parametric" && FactorLevels == 3){
          # message("non-parametric with 3-levels")
          Results <- tidy(kruskal.test(data[[theColumn]] ~ data[[theFactor]], data = data))

          if (!is.na(Results$p.value) && Results$p.value <= alpha){
               Pairwise <- tidy(pairwise.wilcox.test(data[[theColumn]], g=data[[theFactor]],
               p.adjust.method = correction, exact=FALSE))
               Pairwise$method <- "Pairwise Wilcox test"
               Results <- Pairwise 
          }

     } else (stop("Sorry, we didn't plan for your use case"))

     Results <- Results |> mutate(GateName=GateName) |> mutate(FactorName=FactorName)
     return(Results)
}

#' Small helper function to appropiately round up
#'  significant digits, since being used in a plot. 
#' 
#' @param x The p-value being passed in
#' @param alpha Default is 0.05
#' 
pval_mold <- function(x, alpha=0.05){
  if (x >= alpha){"n.s"
  } else if (x > 0.01) {round(x, 2)
  } else if (x > 0.001) {round(x, 3)
  } else if (x > 0.0001) {round(x, 4)
  } else if (x > 0.00001) {round(x, 5)
  } else if (x > 0.000001) {round(x, 6)
  } else{return(x)
  }
} 

#' Coereba Package Internal, used for Utility_Behemoth
#'
#' @param plot The ggplot2 object
#' @param Method Passed method for statistics
#' @param ThePData Dataframe containing index and pvalues
#' @param SingleY The derrived height at which to place the lines
#'
#' @importFrom dplyr select filter pull
#' @importFrom stringr str_wrap
#' @importFrom ggplot2 ggplot aes geom_boxplot scale_shape_manual
#' scale_fill_manual labs theme_bw element_blank element_text theme
#' geom_line geom_text scale_y_continuous lims
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom tibble tibble
#' @importFrom scales percent
#'
#' @return Updated ggplot2 plot with stat lines
#'
#' @noRd
LineAddition <- function(plot, Method, ThePData, SingleY){
  if (!is.null(ThePData)){
    if (nrow(ThePData) > 0){
      IndexSlots <- ThePData |> dplyr::pull(Index)
    } else {IndexSlots <- NULL} 
  } else {IndexSlots <- NULL}

  if (!is.null(IndexSlots)){
  if (length(IndexSlots) < 3 & Method %in% c("Pairwise t-test", "Pairwise Wilcox test")){
    Index1 <- ThePData |> dplyr::filter(Index == 1)
    Index2 <- ThePData |> dplyr::filter(Index == 2)
    Index3 <- ThePData |> dplyr::filter(Index == 3)
    
    if (nrow(Index1) > 0){
      FirstY <- SingleY*1
      FirstP <- ThePData |> dplyr::filter(Index == 1) |> dplyr::pull(ThePvalues)
      
      plot <- plot +
        geom_line(data=tibble(x=c(1,1.9), y=c(FirstY, FirstY)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(1,1), y=c(FirstY*0.98,FirstY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(1.9,1.9), y=c(FirstY*0.98,FirstY*1.02)),aes(x=x, y=y), inherit.aes = FALSE) +
        geom_text(data=tibble(x=1.5, y=FirstY*1.04), aes(x=x, y=y, label = FirstP),size = 4, inherit.aes = FALSE)
    }

    if (nrow(Index2) > 0){
      SecondY <- SingleY*1.1
      SecondP <- ThePData |> dplyr::filter(Index == 2) |> dplyr::pull(ThePvalues)
      plot <- plot +
      geom_line(data=tibble(x=c(1,3), y=c(SecondY, SecondY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SecondY*1.04), aes(x=x, y=y, label = SecondP), size = 4, inherit.aes = FALSE) 
    }

    if (nrow(Index3) > 0){
      ThirdY <- SingleY*1
      ThirdP <- ThePData |> dplyr::filter(Index == 3) |> dplyr::pull(ThePvalues)
      plot <- plot + geom_line(data=tibble(x=c(2.1,3), y=c(ThirdY, ThirdY)),aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(2.1,2.1), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(3,3), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_text(data=tibble(x=2.55, y=ThirdY*1.04), aes(x=x, y=y, label = ThirdP), size = 4, inherit.aes = FALSE)
    }

  } else if (length(IndexSlots) == 3 & Method %in% c("Pairwise t-test", "Pairwise Wilcox test")){

    FirstP <- ThePData |> dplyr::filter(Index == 1) |> dplyr::pull(ThePvalues)
    SecondP <- ThePData |> dplyr::filter(Index == 2) |> dplyr::pull(ThePvalues)
    ThirdP <- ThePData |> dplyr::filter(Index == 3) |> dplyr::pull(ThePvalues)
    FirstY <- SingleY*1
    SecondY <- SingleY*1.1
    ThirdY <- SingleY*1

    plot <- plot +
      geom_line(data=tibble(x=c(1,1.9), y=c(FirstY, FirstY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(FirstY*0.98,FirstY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1.9,1.9), y=c(FirstY*0.98,FirstY*1.02)),aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=FirstY*1.04), aes(x=x, y=y, label = FirstP),size = 4, inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2.1,3), y=c(ThirdY, ThirdY)),aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2.1,2.1), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2.55, y=ThirdY*1.04), aes(x=x, y=y, label = ThirdP), size = 4, inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,3), y=c(SecondY, SecondY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SecondY*1.04), aes(x=x, y=y, label = SecondP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (length(IndexSlots) == 1 & Method %in% c("Two Sample t-test",
  "Wilcoxon rank sum test with continuity correction", "Wilcoxon rank sum exact test")){
    SingleP <- ThePData |> pull(ThePvalues)
    plot <- plot + geom_line(data=tibble(x=c(1,2), y=c(SingleY, SingleY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2,2), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=SingleY*1.04), aes(x=x, y=y, label = SingleP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (length(IndexSlots) == 1 & Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    SingleP <- ThePData |> pull(ThePvalues)
    plot <- plot + geom_line(data=tibble(x=c(1,3), y=c(SingleY, SingleY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SingleY*1.04), aes(x=x, y=y, label = SingleP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else {return(plot)}
  } else {return(plot)}

  return(plot)
}

#' This function runs the desired statistical test on a column of
#' interest for a particular factor column, and then adds the results
#' to a ggbeeswarm boxplot, returning a ggplot2 object. 
#' 
#' @param data The data.frame or tibble object containing our dataset
#' @param theColumn The column containing our measurements of interest
#' @param theFactor The column containing our metadata to compare by
#' @param shape_palette R shape values given to scale_shape_manual,
#' should match levels present in theFactor. For example...
#' shape_sex <- c("Female" = 22, "Male" = 21)
#' @param fill_palette R fill values given to scale_fill_manual, 
#' should match levels present in theFactor. For example...
#' fill_sex <- c("Female" = "white", "Male" = "black")
#' @param parametric Whether to use "parametric" or "non-parametric" test
#' @param alpha Default set to 0.05
#' @param correction Method for FDR correction, default is "none"
#' 
#' 
CombinedFlow <- function(data, theColumn, theFactor,
     shape_palette, fill_palette, parametric="parametric",
     alpha=0.05, correction="none"){

     Stats <- StatsFromFlow(data=data, theColumn=theColumn,
     theFactor=theFactor, parametric=parametric, alpha=alpha,
     correction=correction)

     Method <- Stats$method |> unique()
     thePvalues <- Stats$p.value

     if(any(thePvalues == "NaN")){
           MoldedPvalue <- "n.s"
          } else if (Method %in% c("Two Sample t-test",
            "Wilcoxon rank sum test with continuity correction",
            "Wilcoxon rank sum exact test")){
            # These methods would only return a single p-value
            MoldedPvalue <- pval_mold(thePvalues, alpha=alpha)
          } else if (Method %in% c("One-Way Anova",
           "Kruskal-Wallis rank sum test")){
            # These methods would only return a single p-value
            PresentPvalue <- thePvalues[!is.na(thePvalues)]
            MoldedPvalue <- pval_mold(PresentPvalue, alpha=alpha)
            } else if (Method %in% c("Pairwise t-test",
             "Pairwise Wilcox test")) {
            # These methods might return multiple p-values
            MoldedPvalue <- purrr::map(.x=thePvalues, .f=pval_mold, alpha=alpha)
            } else {
            message("Method not recognized, returning n.s for p-value")
            MoldedPvalue <- "n.s"
            }

     ActualValues <- which("n.s" != MoldedPvalue)
     ShownPvalues <- MoldedPvalue[ActualValues]
     ShownPvalues <- unlist(ShownPvalues)
     ThePData <- data.frame(Index=ActualValues, ThePvalues=ShownPvalues)

     PlotName <- theColumn

     plot <- ggplot(data, aes(x =.data[[theFactor]], y = .data[[theColumn]])) +
          geom_boxplot(show.legend = FALSE) +
          ggbeeswarm::geom_beeswarm(show.legend = FALSE, aes(shape = .data[[theFactor]],
               fill = .data[[theFactor]]), method = "center", side = 0,
               priority = "density", cex = 3, size = 3, corral = "wrap",
               corral.width = 3) + 
          scale_shape_manual(values = shape_palette) +
          scale_fill_manual(values = fill_palette) +
          labs(title = PlotName, x = NULL, y = NULL) +
          theme_bw() + 
          theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               plot.title = element_text(hjust = 0.5, size = 8)
               )

     SingleY <- max(data[[theColumn]])
     SingleY <- SingleY*1.1 #For a little wiggle room

     plot2 <- LineAddition(plot=plot, Method=Method, ThePData=ThePData,
       SingleY=SingleY)

     return(plot2)
}
