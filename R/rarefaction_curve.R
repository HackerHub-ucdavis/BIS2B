
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
    ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "loess", se = F, span = 2) +
        ggplot2::labs(x = "#Individuals", y = "#Species") +
        ggplot2::ggtitle(title)
}
