# lab 4

#' clean the lab data sheet
#'
#' this function is form specific
#' may not appliable if the form changes
#'
#' @param raw a raw dataframe from reading the excel
#' @return a cleaned version for computation
#' @export
clean_lab4_excel <- function(raw) {
    clean <- raw[, !(names(raw) %in% c("Grp. Name", "Mean", "SD", "SE"))]
    clean <- clean[rowSums(is.na(clean)) == 0, ]
    clean
}

#' Test for a Treatment
#'
#' t test for a treatment by given compoents
#'
#' @param data a cleaned dataframe (from clean_lab4_excel)
#' @param treatement string the name of this treatement
#' @param components list of string components names to compare
#' @param  alpha float Significance Level
#' @return a complete table of test results
#' @export
test_treatment <- function(data, treatement, components, alpha = 0.05) {
    # compute the intermediate data
    memo <- data.frame(
        Mean = rowMeans(data),
        SD = apply(data, 1, sd)
    )
    memo$SE <- memo$SD / sqrt(ncol(data))
    memo <- round(memo, 2)

    # critical value
    crit <- qt(1 - alpha, 2 * ncol(data) - 2)

    # t test
    l <- length(components)
    result <- data.frame(
        Treatment = rep(treatement, l),
        Component = components,
        Null = rep(NA, l),
        Alt = rep(NA, l),
        t.val = rep(NA, l),
        Reject = rep(NA, l)
    )
    row_odd <- seq_len(nrow(memo)) %% 2
    morph1 <- memo[row_odd == 1, ]
    morph2 <- memo[row_odd == 0, ]

    for (i in 1:3) {
        mean_g <- morph1$Mean[i]
        mean_yg <- morph2$Mean[i]
        sd_g <- morph1$SD[i]
        sd_yg <- morph2$SD[i]

        t <- abs(mean_g - mean_yg) /
            sqrt(sd_g^2 / 12 + sd_yg^2 / 12)
        result[i, ]$Null <- paste(mean_g, "=", mean_yg, sep = " ")
        result[i, ]$Alt <- paste(mean_g, "!=", mean_yg, sep = " ")
        result[i, ]$t.val <- t
        result[i, ]$Reject <- t > crit
    }
    result
}
