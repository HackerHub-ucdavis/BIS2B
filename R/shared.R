# shared functions
library(ggplot2)

#' make plot on the mean measurements with error bar
#' @param df dataframe of all means and errors, etc
#' @param x string column name of df to put on x axis
#' @param y string column name of df to put on y axis, mean
#' @param err string column name of df to put as error bar
#' @param color string column name of df to group plots
#' @param title string name of the plot
#' @return ggplot graph object
#' @examples
#' # Mean	    SD	    SE	    treatment	morph
#' # 22.44	8.04	2.32	No	        0
#' # 14.55	2.68	0.77	No	        1
#' # 22.06	12.29	3.55	Intramorph	0
#' # 17.03	4.53	1.31	Intramorph	1
#' # 16.72	3.97	1.15	Intermorph	0
#' # 17.67	7.27	2.10	Intermorph	1
#' plot_mean_err(comp, "treatment", "Mean", "SE", "morph", title = "Height")
#' @import ggplot2
#' @export
plot_mean_err <- function(df, x, y, err, color, title = "") {
    ggplot(df, aes_string(x = x, y = y, color = color)) +
        geom_point() +
        geom_errorbar(
            aes_string(ymin = paste0(y, "-", err), ymax = paste0(y, "+", err)
        ),
            width = .2,
            position = position_dodge(0.05)
        ) +
        ggtitle(title)
}

#' string to list of char
#' @param s str
#' @return list of characters in s
#' @export
str2chars <- function(s) strsplit(s, "")[[1]]