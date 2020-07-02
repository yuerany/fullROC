#######################################
# Author: Yueran Yang
# Date: 5/4/20
# Purpose: create functions to create ROC plots
#######################################

###############################################
# FUNCTION to calculate AUC
###############################################
#' A function to calculate AUC
#' @param cp_cum a vector of cumulative cp id rate.
#' @param ca_cum a vector of cumulative ca id rate.
#' @return Calculated AUC.

ROCarea <- function(cp_cum, ca_cum){

  # cummulative true positives -- each repeat twice (except for the last element)
  cp_cum1 <- rep(cp_cum[-1], each = 2)
  cp_cum2 <- cp_cum1[-length(cp_cum1)]

  # binned rates
  ca_bin <- diff(ca_cum)

  # false positives
  ca_bin1 <- rep(ca_bin, each = 2)
  ca_bin2 <- ca_bin1[-1]

  # calculate AUC
  area <- sum(cp_cum2*ca_bin2)/2

  # print out results
  cat(paste0("AUC = ", area), "\n")
}


###############################################
# FUNCTION to calculate AUC from response rates
###############################################
#' A function to calculate AUC using non-cumulative response rates.
#' @param cpr a vector of cp id rate.
#' @param car a vector of ca id rate.
#' @param byDR Whether to order the ids by DR. Defaults to TRUE.
#' @return Calculated AUC.
#' @export

AUC <- function(cpr, car, byDR = TRUE){

  # judge if inputs are correct
  stopifnot(length(cpr) == length(car) )

  #####################################
  # calculate cumulative ID rates
  # Ranked by DR or NOT
  #####################################
  # if ranked by DR
  if(byDR == TRUE){
    # diagnostic ratio
    DR <- cpr/car

    # calculate cumulative id rates; order data by DR; NAs go first
    cp_cum <- cumsum(cpr[order(DR, decreasing = T, na.last = F)])
    ca_cum <- cumsum(car[order(DR, decreasing = T, na.last = F)])
  }
  # not ranked by DR
  else{
    cp_cum <- cumsum(cpr)
    ca_cum <- cumsum(car)
    }

  # calculate cumulative id rates
  cp_cum <- c(0, cumsum(cpr))
  ca_cum <- c(0, cumsum(car))

  # compute AUC
  ROCarea(cp_cum, ca_cum)
}


###############################################
# FUNCTION to plot ROC curve
###############################################
#' A function to plot the cumulative ROC curve.
#'
#' @param cpr A vector of cp id rate
#' @param car A vector of ca id rate
#' @param cumulative Whether the id rates are cumulative. Defaults to FALSE.
#' @param byDR Whether to order the ids by DR. Defaults to TRUE.
#' @param overlay Whether to overlay the ROC curve on an existing plot. Defaults to FALSE.
#' @param cumdata Whether to output cumulative id rates. Defaults to be FALSE.
#' @return An ROC plot and a data matrix of cumulative id rates used to create the plot.
#' @export

ROCplot <- function(cpr, car,
                    cumulative = FALSE,
                    byDR = TRUE,
                    overlay = FALSE,
                    cumdata = FALSE, ...){

  # judge if inputs are correct
  stopifnot(length(cpr) == length(car) )

  ############################
  # if id rates are not cumulative rates; binned rates;
  if(cumulative == FALSE){
    #####################################
    # calculate cumulative ID rates
    # Ranked by DR or NOT
    #####################################
    # if ranked by DR
    if(byDR == TRUE){
      # diagnostic ratio
      DR <- cpr/car

      # calculate cumulative id rates; order data by DR; NAs go first
      cp_cum <- cumsum(cpr[order(DR, decreasing = T, na.last = F)])
      ca_cum <- cumsum(car[order(DR, decreasing = T, na.last = F)])

      # name the vectors
      if(!is.null(names(cpr))) {
        names(cp_cum) <- names(ca_cum) <- names(cpr)[order(DR, decreasing = T, na.last = F)]
      }

    }
    # not ranked by DR
    else{
      cp_cum <- cumsum(cpr)
      ca_cum <- cumsum(car)

      # name the vectors
      if(!is.null(names(cpr))) {
        names(cp_cum) <- names(ca_cum) <- names(cpr)
      }

    }

  }


  # if id rates are cumulative
  else{
    # order by size
    cp_cum <- cpr[order(cpr)]
    ca_cum <- car[order(car)]

    # name the vectors
    if(!is.null(names(cpr))) {
      names(cp_cum) <- names(ca_cum) <- names(cpr)[order(cpr)]
    }

  }

  # combine the id rates
  cpa_cum <- cbind(cp_cum, ca_cum)

  #################
  # add 0's to the id rates
  cp_cum0 <- c(0, cp_cum)
  ca_cum0 <- c(0, ca_cum)

  #########################################
  # generating a new plot
  #########################################
  if(overlay == FALSE){

    plot(x = ca_cum0, y = cp_cum0,
         xlim = c(0, 1),
         ylim = c(0, 1),
         xlab = "Innocent suspect id",
         ylab = "Guilty suspect id",
         ...)

  } else{

    lines(x = ca_cum0, y = cp_cum0, ...)

  }


  ################################################
  ## calculate the area under the curve (AUC)
  ################################################
  ROCarea(cp_cum0, ca_cum0)

  ######################
  # print cumulative id rates
  if(cumdata == TRUE) return(cpa_cum)

}

