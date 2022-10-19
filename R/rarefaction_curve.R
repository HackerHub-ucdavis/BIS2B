library(ggplot2)

#' Rarefaction Plot
#'
#' make a rarefaction plot based on a two column data frame
#'
#' @param df A two column dataframe that contains
#'  \# of new species and \# of individuals in order
#' @param title A name of the plot
#' @return a ggplot graph object
#'
#' @examples
#' rf <- read.csv("../data/lab1_rarefaction.csv", header = F)
#' makeRarefactionPlot(rf[, 1:2], title = "Group 1")
#' @export
#' @import ggplot2
rarefaction_plot <- function(df, title = "") {
    colnames(df) <- c("y", "x")
    ggplot(df, aes(x, y)) +
        geom_point() +
        geom_smooth(method = "loess", se = F, span = 2) +
        labs(x = "#Individuals", y = "#Species") +
        ggtitle(title)
}
