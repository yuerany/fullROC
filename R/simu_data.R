######################################
# FUNCTION to calculate response
######################################
#' A function to calculate responses from simulated memory distribution
#'
#' @param memory A simulated data matrix with suspect (column 1) and filler memory.
#' @param criterion A vector of judgment criterion. Must have odd number of elements if id_criterion is not specified.
#' @param id_criterion A number to define the criterion for id or rejection. Use the middle element of criterion if not specified.
#' @param suspect Whether there is a designated suspect. Defaults to TRUE.
#' @return A data matrix of id responses (IDS, IDF, or REJ) and confidence levels.
#
# y_memory <- replicate(n_sim, rnorm(1, mean = 2))
# x_memory <- replicate(n_sim, rnorm(1, mean = 0))
# filler <- t(replicate(n_sim, rnorm(5, mean = 0)))
#
# cp_memory <- cbind(y_memory, filler)
# ca_memory <- cbind(x_memory, filler)
# response_calculate(cp_memory, 1:3)

response_calculate <- function(memory, criterion,
                     id_criterion = NULL, suspect = TRUE){

  ######################
  # judge input values
  ######################
  if(is.null(id_criterion) & (length(criterion) %% 2 == 0) ){
    stop("Must have odd number of criteria or specify the criterion for id.")
  }

  #####################
  # get suspect memory
  suspect_memory <- memory[,1]

  # caculate the max memory for each simulated row
  max_memory <- apply(memory, 1, max)

  #############################
  # calculate id responses
  #############################

  if(is.null(id_criterion)){
    # id/reject criterion: the middle criterion
    id_criterion <- criterion[median(1:length(criterion))]
  }

  # for a designated suspect
  if(suspect == TRUE){
    # suspect id
    ID <- ifelse(max_memory <= id_criterion, "REJ",
                          ifelse(suspect_memory == max_memory, "IDS", "IDF") )
  }
  # no designated suspect
  else{
    ID <- ifelse(max_memory <= id_criterion, "REJ", "IDF")
  }

  #############################
  # calculate associated confidence levels
  #############################

  # confidence bins
  cll <- c(-Inf, criterion)
  cul <- c(criterion, Inf)

  # upper bin
  cbin <- sapply(max_memory, function(x) min(which(x - cul <= 0)))

  # create bin labels
  clabel <- paste(paste0("c", cll), paste0("c", cul), sep = ":")

  # confience levels
  confidence <- as.character(factor(cbin, levels = 1:(length(criterion)+1),
                       labels = clabel))

  #############################
  # return to id responses
  #############################

  return(data.frame(ID, confidence))

}



##############################################
# FUNCTION to simulate both CP and CA responses
##############################################
#' @title Simulate witness responses
#' @description A function to simulate both CP and CA responses
#'
#' @param guilt_diff Mean difference between guilty suspect and filler distributions.
#' @param inno_diff Mean difference between innocent suspect and filler distributions. Defaults to 0.
#' @param n_sim Number of simulations per condition. Defaults to 1,000.
#' @param size Number of lineup members. Defaults to 6.
#' @param inno_suspect Whether there is a designated innocent suspect. Defaults to FALSE.
#' @param criterion A vector of response criteria. Must have odd number of elements if id_criterion is not specified.
#' @param id_criterion A number to define the criterion for id or rejection. Use the middle element of criterion if not specified.
#'
#' @return A data frame including both CP and CA ID responses and confidence levels.
#'
#' @examples
#' # Set up response criteria
#' rc1 <- seq(-1, 3, length.out = 5)
#'
#' # no designated innocent suspect
#' response_simu(guilt_diff = 2, criterion = rc1)
#'
#' # with a designated innocent suspect
#' response_simu(guilt_diff = 2, inno_diff = 0.2, inno_suspect = TRUE, criterion = rc1)
#'
#' # define a criterion for id/rejection instead of using the middle criterion
#' response_simu(guilt_diff = 2, criterion = 0:3, id_criterion = 1)
#'
#' @export


response_simu <- function(guilt_diff, inno_diff = 0,
                   n_sim = 1000, size = 6,
                   inno_suspect = FALSE,
                   criterion, id_criterion = NULL){

  # number of fillers
  fsize <- size-1

  # simulate suspect and filler memory
  y_memory <- replicate(n_sim, rnorm(1, mean = guilt_diff))
  x_memory <- replicate(n_sim, rnorm(1, mean = inno_diff))
  filler1 <- t(replicate(n_sim, rnorm(fsize, mean = 0)))
  filler2 <- t(replicate(n_sim, rnorm(fsize, mean = 0)))

  # combine suspect and filler memory
  cp_memory <- cbind(y_memory, filler1)
  ca_memory <- cbind(x_memory, filler2)

  # cp responses
  cp_id <- data.frame( lineup = "cp", response_calculate(cp_memory, criterion, id_criterion) )
  # ca responses
  ca_id <- data.frame( lineup = "ca", response_calculate(ca_memory, criterion, id_criterion, suspect = inno_suspect) )

  # combine the two
  id <- rbind(cp_id, ca_id)

  return(id)
}


