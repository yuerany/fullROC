###############################################
# FUNCTION to convert frequencies to proportions
###############################################

prop_rate <- function(x){ x/sum(x) }

###############################################
# FUNCTION to generate cumulative id rates
###############################################

id_cum <- function(rate, dr = NULL){

  # order by DR
  if(!is.null(dr)){
    y <- cumsum(rate[order(dr, decreasing = TRUE, na.last = FALSE)])
  }
  # not order by DR
  else{
    y <- cumsum(rate)
  }

  # add 0 to the cumulative id vector
  return( c(0, y) )

}

###############################################
# FUNCTION to generate cumulative id rates for both cp and ca lineups
# ******export*******
###############################################
#' A function to generate cumulative id rates for both cp and ca lineups
#' @param data A matrix with both cp and ca id rates.
#' @param byDR Whether to order ids by diagnosticity ratios. Defaults to FALSE.
#' @return A data matrix with cumulative cp and ca id rates.
#' @export

data_cum <- function(data, byDR = FALSE){

  data <- as.matrix(data)

  # convert frequencies to rates if necessary
  if(sum(data > 1) > 0){
    data <- apply(data, 2, prop_rate)
  }

  # order by DR
  if(byDR == TRUE){

    dr <- data[, 1]/data[, 2]
    d_cum <- apply(data, 2, id_cum, dr = dr)
  }
  # not order
  else{
    d_cum <- apply(data, 2, id_cum)
  }

  return(d_cum)
}


###############################################
# FUNCTION to calculate AUC from cumulative id rates
###############################################

auc <- function(d_cum){

  d_cum <- as.matrix(d_cum)

  # get cumulative id rates
  cp_cum <- d_cum[, 1]
  ca_cum <- d_cum[, 2]

  # cumulative true positives -- each repeat twice (except for the last element)
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
  return(area)
}

###############################################
# FUNCTION to calculate AUC from cumulative id rates for a single group
###############################################

# without group variable
roc_auc0 <- function(data, byDR = FALSE){

  data <- as.matrix(data)

  # get cumulative id rates; d_cum converts frequencies to rates
  d_cum <- data_cum(data, byDR = byDR)

  # compute auc
  return(auc(d_cum))

}


###############################################
# FUNCTION to calculate AUC from original id rates
# FUCNTION: roc_auc
# export
###############################################
#' A function to calculate AUC using non-cumulative response rates.
#' @param cpr A vector of cp id rates.
#' @param car A vector of ca id rates.
#' @param group A vector indicating group membership. If specified, will calculate AUC by group.
#' @param byDR Whether to order ids by diagnosticity ratios. Defaults to FALSE.
#' @return Area under the curve.
#' @export

# add group variable
roc_auc <- function(cpr, car, group = NULL, byDR = FALSE){

  message("check order of input: cpr first, car second")

  # set up data
  data <- data.frame(cpr, car)


  # calculate auc by group
  if(!is.null(group)){
    by(data, group, roc_auc0, byDR = byDR)

  }
  # one single group
  else {
    roc_auc0(data, byDR = byDR)
  }

}


