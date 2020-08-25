###################################
# FUNCTION to create lines with defaults
###################################

# set up the default of line type to be "o" (overlay),
# pch = 20 (a dot),
# and lwd = 2

line_add <- function(x, y, ...){

  # set up arguments
  arguments <- list(
    x = x,
    y = y,
    ...,
    type = "o",
    pch = 20,
    lwd = 2
  )

  arguments <- arguments[!duplicated(names(arguments))]

  do.call("lines", arguments)
}




###############################################
# FUNCTION to plot ROC curve and calculate AUC
###############################################
#' A function to plot the cumulative ROC curve.
#'
#' @param cpr A vector of cp id rate
#' @param car A vector of ca id rate
#' @param group Grouping variable to indicate group membership. Will create an ROC curve and calculate AUC for each group.
#' @param byDR Whether to order the ids by DR. Defaults to FALSE.
#' @return NULL. Generate an ROC plot and auc as side effects.
#' @export

roc_plot <- function(cpr, car,
                     group = NULL, byDR = FALSE,
                     xlab = "Innocent suspect rate", ylab = "Guilty suspect rate",
                     ...){

  message("check order of input: cpr first, car second")

  # set up data
  data <- data.frame(cpr, car)

  # set up plotting area
  plot(NA,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = xlab, ylab = ylab,
       ...)

  # plot by group
  if(!is.null(group)){

    # color index
    ic <- 1

    # create ROC curve and calculate AUC for each group
    for(g in unique(group)){

      # get subset of data
      dtmp <- data[group == g, ]

      ### cumulative data
      d_cum <- data_cum(dtmp, byDR = byDR)

      ### add ROC curve
      line_add(d_cum[, 2], d_cum[, 1], col = ic, ...)

      ### calculate auc
      cat(g, "\n", "AUC =", roc_auc0(dtmp, byDR = byDR), "\n")

      ic <- ic + 1
    }

    legend("bottomright", col = seq_along(unique(group)), bty = "n",
           legend = unique(group), ...)

  }

  # for a single group
  else{

    ### cumulative data
    d_cum <- data_cum(data, byDR = byDR)

    ### add ROC curve
    line_add(d_cum[, 2], d_cum[, 1], ...)

    ### calculate auc
    cat("AUC =", roc_auc0(data, byDR = byDR), "\n")
  }

}
