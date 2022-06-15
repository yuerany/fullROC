###################################
# FUNCTION to create lines with defaults
###################################
# set up the default of
# type = "o"
# pch = 20

lines_add <- function(x, y, ...){

  # set up arguments
  arguments <- list(
    x = x,
    y = y,
    ...,
    type = "o",
    pch = 20
  )

  arguments <- arguments[!duplicated(names(arguments))]

  do.call(lines, arguments)
}


###############################################
# FUNCTION to add an ROC curve to existing ROC plots
# and calculate AUC
# ******export*******
###############################################
#' A function to add an ROC curve to an existing ROC plot.
#'
#' @param cp A vector of cp id rates or frequencies.
#' @param ca A vector of ca id rates or frequencies.
#' @param byDR Whether to order ids by diagnosticity ratios. Defaults to FALSE.
#' @param ... Additional plotting parameters.
#'            For example, users can change x-axis and y-axis labels using \code{xlab} and \code{ylab}.
#' @return Plot ROC curves and calculate AUCs as side effects.
#'
#' @importFrom graphics lines
#' @export

roc_line <- function(cp, ca,
                     byDR = FALSE,
                     ...){

  message("check order of input: cp first, ca second")

  # set up data
  data <- data.frame(cp, ca)

  # for a single group
  ### cumulative data
  d_cum <- data_cum(data, byDR = byDR)

  ### add ROC curve
  lines_add(d_cum[, 2], d_cum[, 1], ...)

  ### calculate auc
  message("AUC = ", roc_auc0(data, byDR = byDR), "\n")

}


