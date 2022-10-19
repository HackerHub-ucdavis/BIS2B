
#' Expoential Growth Population Size
#'
#' calculate the  predicted population size under exponential growth
#'
#' @param N0 the initial population size
#' @param r the intrinsic growth rate
#' @param t units of time
#' @return predicted population size
#' @export
pop_prowth.exp <- function(N0, r, t) {
    N0 * exp(r * t)
}

#' Expoential Growth Rate
#'
#' calculate the growth rate under exponential growth
#'
#' @param Nt population size after time t
#' @param N0 the initial population size
#' @param t units of time
#' @return growth rate
#' @export
growth_rate.exp <- function(Nt, N0, t) {
    log(Nt / N0) / t
}

#' Expoential Growth Time
#'
#' calculate the growth time under exponential growth
#'
#' @param Nt population size after time t
#' @param N0 the initial population size
#' @param r the intrinsic growth rate
#' @return time takes the population size to grow
#' @export
growth_time.exp <- function(Nt, N0, r) {
    log(Nt / N0) / r
}

# Logit Growth
#' Logistic Growth Population Size
#'
#' calculate the  predicted population size under logit growth
#'
#' @param N0 the initial population size
#' @param r the intrinsic growth rate
#' @param t units of time
#' @return predicted population size
#' @export
pop_growth.logit <- function(N0, r, t, K) {
    K / (1 + ((K - N0) / N0) * exp(-r * t))
}

#' Logistic Growth Rate
#'
#' calculate the growth rate under logit growth
#'
#' @param Nt population size after time t
#' @param N0 the initial population size
#' @param t units of time
#' @return growth rate
#' @export
growth_rate.logit <- function(Nt, N0, t) {
    log(Nt / N0) / t
}

#' Logistic Growth Time
#'
#' calculate the growth time under logit growth
#'
#' @param Nt population size after time t
#' @param N0 the initial population size
#' @param r the intrinsic growth rate
#' @return time takes the population size to grow
#' @export
growth_time.logit <- function(Nt, N0, r) {
    log(Nt / N0) / r
}

#' Life Table
#'
#' complete a life table
#' see PPT 8_PopulationDemographic P5-11
#'
#' @param df an initial life table with
#' Age in years (x)
#' Number of Individuals (Nx)
#' Number of offspring (N x off)
#' @return a Life Table object with everything needed
#' @examples
#' df <- data.frame(
#'     x = 0:12,
#'     Nx = c(90, 39, 33, 30, 29, 27, 16, 8, 5, 5, 3, 2, 0),
#'     Nxoff = c(0, 2, 22, 45, 19, 149, 11, 0, 0, 11, 0, 0, NA)
#' )
#' complete_life_table(df)
#' @export
complete_life_table <- function(df) {
    df$lx <- df$Nx / df$Nx[1]
    df$mx <- df$Nxoff / df$Nx
    n <- nrow(df)
    R0 <- sum(df$lx * df$mx, na.rm = T)
    G <- sum(df$x * df$lx * df$mx, na.rm = T) / R0
    r <- log(R0) / G

    ltout <- list(
        complete_table = df,
        R0 = R0,
        G = G,
        r = r
    )
    class(ltout) <- c("LifeTable")
    ltout
}