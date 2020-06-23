#######################################
# Author: Yueran Yang
# Date: 5/5/20
# Purpose: use 1/m method to re-compute id rates
#######################################

##################################################
# FUNCTION 1: ADJUSTING MISTAKEN ID RATES
# create adjusted id rates using the 1/m method
##################################################

#' A function to adjust the id rates for ca lineups using the 1/m method.
#'
#' @param id ID rate vector.
#' @param conf Confidence levels for the id rate vector. Default to be NULL.
#' @param fid Mapping confidence levels from filler id.
#' @param sid To-be-matched confidence levels for suspect id. Must have equal length as fid.
#' @param size Size of the lineup. Defaults to 6.
#' @examples
#' ca_id <- c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2))
#' names(ca_id) <- paste0(rep(c("IDS", "IDF", "REJ"), each = 3), c("high", "medium", "low"))
#'
#' fid_conf <- paste0("IDF", c("high", "medium", "low"))
#' sid_conf <- paste0("IDS", c("high", "medium", "low"))
#'
#' IDadj(ca_id, fid = fid_conf, sid = sid_conf)
#' @export

IDadj <- function(id, conf = NULL, fid, sid, size = 6){

  stopifnot(is.numeric(id) & is.integer(size))

  # get the confidence levels
  if(is.null(conf)){
    conf <- names(id)
  }

  #############
  # judgment regarding confidence levels
  jconf <- c(fid, sid)

  if(sum(!(jconf %in% conf)) > 0){
    cat(paste(jconf[jconf %in% conf == FALSE], "not identified. \n"))
    stop("cannot find confidence levels")
  }

  if(length(fid) != length(sid)){
    stop("FID and SID confidence levels do not match")
  }

  #################
  # calculate the adjusted ID rates
  else{

    # create a matrix to identify the transformation

    y_fid <- id[match(fid, conf)]

    # new suspect id rates
    y_sid_new <- y_fid/size
    # new filler id rates
    y_fid_new <- y_fid*(size - 1)/size

    # create a new vector
    ynew <- id
    ynew[match(sid, conf)] <- y_sid_new
    ynew[match(fid, conf)] <- y_fid_new

    return(ynew)
  }

}



