# lab 6
library(readxl)

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

#' load data from lab6 excel
#' NOTE not a genaric function
#'
#' @param xlsx string path to excel file
#' @return list of dataframes
#' @import readxl
#' @export
from_lab6_excel <- function(xlsx) {
    sheets <- excel_sheets(xlsx)
    to_succession_obj <- function(sheet) {
        df <- na.omit(read_xlsx(xlsx, sheet = sheet))[-1]
        class(df) <- c("succession", class(df))
        df
    }
    lapply(sheets, to_succession_obj)
}

#' summary of succession (lab6 data)
#'
#' @param df dataframe
#' @param month age of panel
#' @return summary dataframe
#' @export
summary.succession <- function(df, age) {
    summ <- as.data.frame(t(rbind(colMeans(df), sapply(df, sd))))
    colnames(summ) <- c("mean", "std")
    summ$age <- as.factor(rep(age, ncol(df)))
    summ$category <- rownames(summ)
    summ
}
