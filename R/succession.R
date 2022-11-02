# lab 6

#' complete the coverage table for one panel
#' @param a total % converage
#' @param b % converage on panel surface
#' @return complete table with side information
#'
#' @examples
#' A_3_months <- c(17, 1 / 2, 10 + 1 / 4, 1 / 4, 1 / 2) / 25
#' B_3_months <- c(17, 1 / 2, 5 + 1 / 4, 1 / 4, 0) / 25
#' panel_coverage(A3, B3)
#'
#' @export
panel_coverage <- function(a, b) {
    total_percent_cover <- sum(a)
    df <- data.frame(
        A = a,
        P = a / total_percent_cover,
        B = b,
        overgrowth = a - b
    )
    list(
        table = df,
        total_number_species = nrow(df),
        total_percent_cover = total_percent_cover,
        true_diversity_D = prod(df$P^(-df$P)),
        total_percent_on_primary_surface = sum(df$B),
        total_percent_overgrowth = sum(df$overgrowth)
    )
}
