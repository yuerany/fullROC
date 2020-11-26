##################################################
# FUNCTION: ADJUSTING MISTAKEN ID RATES
# create adjusted id rates using the 1/m method
##################################################

# 3 DIFFERENT FUNCTIONS
# 1. SIMPLE PROJECTION (FOR ORDERED ID RATES WITH EQUAL CONFIDENCE LEVELS FOR ALL RESPONSES)
# 2. MATCH BY POSITION
# 3. MATCH BY NAME


#' #=========================================
#' # VARIATION 1: SIMPLE PROJECTION
#' #=========================================
#' #' @title Simple adjustment
#' #' @description A function to adjust the id rates for ca lineups using the 1/(lineup size) method;
#' #' is applicable to ordered id rates with the same confidence levels for all responses.
#' #'
#' #' @param rate ID rate vector.
#' #' @param lsize Lineup size. Defaults to 6.
#' #' @param csize Number of confidence levels. Defaults to 3.
#' #' @return Adjusted ID vector.
#' #' @examples
#' #' ca_id <- c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2))
#' #' id_adj_old(ca_id)
#' #'
#' #' ## change line size to 5
#' #' id_adj_old(ca_id, lsize = 5)
#' #'
#' #' ## For multiple groups
#' #' ca_id2 <- c(c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2)),
#' #'             c(rep(0,3), rep(c(0.1, 0.2, 0.3), 2)) )
#' #' group <- rep(c("a", "b"), each = 9)
#' #' by(ca_id2, group, id_adj_old)
#' #'
#' #' ## can be used jointly with the pipes commands
#' #' d <- data.frame(group, ca_id2)
#' #' d %>%
#' #'   group_by(group) %>%
#' #'   group_map(~ id_adj_old(.x$ca_id2))
#'
#'
#' id_adj_old <- function(rate, lsize = 6, csize = 3){
#'
#'   ##  compute the projection matrix
#'   proj <- matrix(c(0, 1/lsize, 0,
#'                    0, (lsize-1)/lsize, 0,
#'                    0, 0, 1),
#'                  byrow = T, nrow = 3) %x% diag(csize)
#'
#'   return(as.vector(proj %*% rate))
#'
#' }
#'


#=========================================
# VARIATION 1.2: A MORE STABLE VERSION; CAN BE USED WITH PIPES
#=========================================
#' @title Simple adjustment
#' @description A function to adjust the id rates for ca lineups using the 1/(lineup size) method;
#' is applicable to ordered id rates with the same confidence levels for all responses.
#'
#' @param rate ID rate vector.
#' @param lsize Lineup size. Defaults to 6.
#' @param csize Number of confidence levels. Defaults to 3.
#' @return Adjusted ID vector.
#' @examples
#' ca_id <- c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2))
#' id_adj(ca_id)
#'
#' ## change line size to 5
#' id_adj(ca_id, lsize = 5)
#'
#' ## For multiple groups
#' ca_id2 <- c(c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2)),
#'             c(rep(0,3), rep(c(0.1, 0.2, 0.3), 2)) )
#' group <- rep(c("a", "b"), each = 9)
#' ## Adjust id rates by groups
#' by(ca_id2, group, id_adj)
#'
#' @export

id_adj <- function(rate, lsize = 6, csize = 3){

  ##  extract idf
  idf_old <- rate[(csize+1):(2*csize)]
  rej <- rate[(2*csize + 1):length(rate)]

  ## compute adjusted ids and idf
  ids <- idf_old/lsize
  idf <- idf_old - ids

  ## new rate vector
  rate_new <- c(ids, idf, rej)

  return(rate_new)
}



#=========================================
# VARIATION 2: MATCH BY POSITION
#=========================================
#' @title Match by position
#' @description A function to adjust the id rates for ca lineups using the 1/(lineup size) method;
#' match and adjust id rates by positions of filler and suspect ids.
#'
#' @param rate ID rate vector.
#' @param fid Mapping positions from filler id.
#' @param sid To-be-matched positions for suspect id. Must have equal length as fid.
#' @param lsize Lineup size. Defaults to 6.
#'
#' @return Adjusted ID vector.
#' @examples
#' ca_id <- c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2))
#' id_adj_pos(ca_id, fid = 4:6, sid = 1:3, lsize = 5)
#' @export


id_adj_pos <- function(rate, fid, sid, lsize = 6) {

  y <- rate <- as.vector(rate)

  # original fid
  rfid <- rate[fid]

  # adjust sid and fid
  y[sid] <- rfid/lsize
  y[fid] <- rfid*(lsize - 1)/lsize

  return(y)
}




#=========================================
# VARIATION 3: MATCH BY CONFIDENCE NAME
#=========================================
#' @title Match by confidence levels
#' @description A function to adjust the id rates for ca lineups using the 1/(lineup size) method;
#' match and adjust id rates by names of confidence levels for both filler and suspect ids.
#' @param rate ID rate vector.
#' @param conf Confidence levels for the id rate vector. Default to be NULL.
#' @param fid Mapping confidence levels from filler id.
#' @param sid To-be-matched confidence levels for suspect id. Must have equal length as fid.
#' @param lsize Lineup size. Defaults to 6.
#'
#' @return Adjusted ID vector.
#' @examples
#' ca_id <- c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2))
#' names(ca_id) <- paste0(rep(c("IDS", "IDF", "REJ"), each = 3), c("high", "medium", "low"))
#'
#' fid_conf <- paste0("IDF", c("high", "medium", "low"))
#' sid_conf <- paste0("IDS", c("high", "medium", "low"))
#'
#' id_adj_name(ca_id, fid = fid_conf, sid = sid_conf)
#' @export


id_adj_name <- function(rate, conf = NULL, fid, sid, lsize = 6){

  # get the confidence levels
  if(is.null(conf)){
    conf <- names(rate)
  }

  #############
  # check confidence levels
  jconf <- c(fid, sid)

  if(sum(!(jconf %in% conf)) > 0){
    stop(paste("cannot find confidence level:", jconf[jconf %in% conf == FALSE], "\n  ") )
  }

  if(length(fid) != length(sid)){
    stop("FID and SID confidence levels do not match")
  }

  #################
  # calculate the adjusted ID rates
  else{

    # create a new vector
    y <- rate <- as.vector(rate)

    # original fid
    rfid <- rate[match(fid, conf)]

    # adjust sid and fid
    y[match(sid, conf)] <- rfid/lsize
    y[match(fid, conf)] <- rfid*(lsize - 1)/lsize

    return(y)
  }

}


