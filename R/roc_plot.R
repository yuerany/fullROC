###################################
# FUNCTION to create plots with defaults
###################################

# set up the default of
# xlab and ylab
# xlim and ylim

plot_add <- function(x, ...){

  # set up arguments
  arguments <- list(
    x = x,
    ...,

    xlim = c(0, 1),
    ylim = c(0, 1),

    xlab = "Innocent suspect rate",
    ylab = "Guilty suspect rate"
  )

  arguments <- arguments[!duplicated(names(arguments))]

  do.call(plot, arguments)
}


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


###################################
# FUNCTION to create legend with defaults
###################################
# set up the default of
# lty = 1
# pch = 20

legend_add <- function(...){

  # set up arguments
  arguments <- list(
    ...,
    lty = 1,
    pch = 20
  )

  # select legend arguments
  arguments <- arguments[!duplicated(names(arguments))]

  do.call(legend, arguments)
}

###############################################
# FUNCTION to plot ROC curve and calculate AUC
###############################################
#' A function to plot ROC curves. Note that the NA values in the data will be replaced with zero.
#'
#' @param cp A vector of cp id rates or frequencies.
#' @param ca A vector of ca id rates or frequencies.
#' @param group Grouping variable to indicate group membership. Will create an ROC curve and calculate AUC for each group.
#' @param byDR Whether to order ids by diagnosticity ratios. Defaults to FALSE.
#' @param grayscale Whether to produce the plot in grayscale. Defaults to FALSE.
#' @param ... Additional plotting parameters.
#'            For example, users can change x-axis and y-axis labels using \code{xlab} and \code{ylab}.
#' @return Plot ROC curves and calculate AUCs as side effects.
#'
#' @references
#' Yueran Yang & Andrew Smith. (2020). "fullROC: An R package for generating and analyzing eyewitness-lineup ROC curves"
#' \doi{10.13140/RG.2.2.20415.94885/1}
#'
#' Andrew Smith, Yueran Yang, & Gary Wells. (2020). "Distinguishing between investigator discriminability and eyewitness discriminability: A method for creating full receiver operating characteristic curves of lineup identification performance". \emph{Perspectives on Psychological Science, 15}(3), 589-607.
#' \doi{10.1177/1745691620902426}
#'
#' @examples
#' cpf1 <- c(100, 90, 80, 20, 10, 5)
#' caf1 <- c(6, 7, 15, 50, 75, 120)
#' roc_plot(cpf1, caf1)
#'
#'
#' cpf2 <- c(90, 40, 20)
#' caf2 <- c(10, 70, 80)
#' roc_plot(cpf2, caf2)
#'
#' ## plot two ROC curves
#' cpf <- c(cpf1, cpf2)
#' caf <- c(caf1, caf2)
#' group <- rep(letters[1:2], times = c(length(cpf1), length(cpf2) ) )
#' roc_plot(cpf, caf, group = group)
#'
#' @importFrom graphics plot lines legend
#' @importFrom grDevices gray
#' @export

roc_plot <- function(cp, ca,
                     group = NULL,
                     byDR = FALSE,
                     grayscale = FALSE,
                     ...){

  message("check order of input: cp first, ca second")

  # set up data
  data <- data.frame(cp, ca)

  # set up plotting area
  plot_add(NA, ...)

  # group color indicator
  lc <- seq_along(unique(group))

  # grayscale
  lgray <- gray((lc-1)/length(lc))

  # line color indicator
  if(grayscale == TRUE) {lc <- lgray}

  # plot by group
  if(!is.null(group)){

    # line color index
    i <- 1

    # create ROC curve and calculate AUC for each group
    for(g in unique(group)){

      # get subset of data
      dtmp <- data[group == g, ]

      ### cumulative data
      d_cum <- data_cum(dtmp, byDR = byDR)

      ### add ROC curve
      lines_add(d_cum[, 2], d_cum[, 1], col = lc[i], ...)

      ### calculate auc
      message(g, "\n", "AUC = ", roc_auc0(dtmp, byDR = byDR), "\n")

      i <- i + 1
    }

    # add legend
    legend_add("bottomright", col = lc, legend = unique(group), bty = "n", ...)

  }

  # for a single group
  else{

    ### cumulative data
    d_cum <- data_cum(data, byDR = byDR)

    ### add ROC curve
    lines_add(d_cum[, 2], d_cum[, 1], ...)

    ### calculate auc
    message("AUC = ", roc_auc0(data, byDR = byDR), "\n")
  }

}


