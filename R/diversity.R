#' D of an environment (multiple samples)
#'
#' calculate true diversity index of multiple sample
#' see lab 1 for example
#'
#' @param df each row is the number of each species in the sample
#' @return true diversity index of environment
#' @examples
#' df <- read.csv("../data/lab1.csv")
#' true_diversity_index(df)
#' @export
true_diversity_index <- function(df) {
    total <- sum(df)
    p <- sapply(df, function(sample) sum(sample) / total)
    prod(p^(-p))
}

#' D of one sample
#'
#' calculate true diversity index of one sample
#' see study guide 1 #12
#' PPT 3_Biodiversity P7
#'
#' @param ns number of each species in the sample
#' @return true diversity index of this sample
#' @examples
#' true_diversity_index.sample(c(4, 2, 3, 11))
#' @export
true_diversity_index.sample <- function(ns) {
    total <- sum(ns)
    p <- ns / total
    prod(p^(-p))
}
