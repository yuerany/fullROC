# set up the default of
# xlab and ylab
# xlim and ylim

zplot_add <- function(x, ...){

  # set up arguments
  arguments <- list(
    x = x,
    ...,

    xlim = c(-2.5, 2.5),
    ylim = c(-2.5, 2.5),

    xlab = "z(false positive)",
    ylab = "z(true positive)"
  )

  arguments <- arguments[!duplicated(names(arguments))]

  do.call(plot, arguments)
}


###################################
# FUNCTION to create points with defaults
###################################
# set up the default of
# type = "o"
# pch = 20

points_add <- function(x, y, ...){

  # set up arguments
  arguments <- list(
    x = x,
    y = y,
    ...,
    pch = 20
  )

  arguments <- arguments[!duplicated(names(arguments))]

  do.call(points, arguments)
}



###############################################
# FUNCTION to plot z-ROC curve
# ******export******
###############################################
#' A function to plot z-ROC curves. Note that the NA values in the data will be replaced with zero.
#'
#' @param cp A vector of cp id rates or frequencies.
#' @param ca A vector of ca id rates or frequencies.
#' @param group Grouping variable to indicate group membership. Will create an ROC curve and calculate AUC for each group.
#' @param byDR Whether to order ids by diagnosticity ratios. Defaults to FALSE.
#' @param grayscale Whether to produce the plot in grayscale. Defaults to FALSE.
#' @param ... Additional plotting parameters.
#'            For example, users can change x-axis and y-axis labels using \code{xlab} and \code{ylab}.
#' @return Plot z-ROC curves.
#'
#' @examples
#' cpf1 <- c(100, 90, 80, 20, 10, 5)
#' caf1 <- c(6, 7, 15, 50, 75, 120)
#' zroc_plot(cpf1, caf1)
#'
#'
#' cpf2 <- c(90, 40, 20)
#' caf2 <- c(10, 70, 80)
#' zroc_plot(cpf2, caf2)
#'
#' ## plot two ROC curves
#' cpf <- c(cpf1, cpf2)
#' caf <- c(caf1, caf2)
#' group <- rep(letters[1:2], times = c(length(cpf1), length(cpf2) ) )
#' zroc_plot(cpf, caf, group = group)
#'
#' @importFrom graphics plot points abline legend
#' @importFrom stats qnorm lm
#' @importFrom grDevices gray
#' @export

zroc_plot <- function(cp, ca,
                      group = NULL,
                      byDR = FALSE,
                      grayscale = FALSE,
                      ...){

  message("check order of input: cp first, ca second")

  # set up data
  data <- data.frame(cp, ca)

  # set up plotting area
  zplot_add(NA, ...)
  abline(0, 1)

  #====================================
  # plot by group
  if(!is.null(group)){

    # group color indicator
    lc <- seq_along(unique(group))
    # grayscale
    if(grayscale == TRUE) {lc <- gray((lc-1)/length(lc))}

    #----------------------
    # line color index
    i <- 1

    # create ROC curve and calculate AUC for each group
    for(g in unique(group)){

      # get subset of data
      dtmp <- data[group == g, ]

      ### cumulative data
      d_cum <- data_cum(dtmp, byDR = byDR)
      d_z <- qnorm(d_cum)
      d_z[!is.finite(d_z)] <- NA

      ### add z-ROC points
      points_add(d_z[, 2], d_z[, 1], col = lc[i], ...)

      ### add regression line
      abline(lm(d_z[, 1] ~ d_z[, 2]), col = lc[i])

      i <- i + 1
    }

    # add legend
    legend_add("bottomright", col = lc, legend = unique(group), bty = "n", ...)

  }

  # for a single group
  else{

    ### cumulative data
    d_cum <- data_cum(data, byDR = byDR)
    d_z <- qnorm(d_cum)
    d_z[!is.finite(d_z)] <- NA

    ### add z-ROC points
    points_add(d_z[, 2], d_z[, 1],  ...)

    ### add regression line
    abline(lm(d_z[, 1] ~ d_z[, 2]))
  }

}


