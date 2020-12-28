#######################################
# Purpose: bootstrap methods to calculate CI's for AUC
#######################################

# simulate responses for one bootstrap trial
boot_simu <- function(x, xr = NULL){

  # x is a vector of frequencies

  # compute response rate from frequencies
  if(is.null(xr)) xr <- prop_rate(x)

  # sample a new sample
  xs <- sample(seq_along(x), round(sum(x)), prob = xr, replace = TRUE)
  # convert into a factor in case of missing levels
  xs <- factor(xs, levels = seq_along(x))


  # compute response rates
  xsr <- c(prop.table(table(xs)))

  return(xsr)
}


#=======================
# for a single group
# calculate auc's for simulated samples
auc_boot0 <- function(data,
                      nboot,
                      byDR,
                      ca_adj, lsize, csize){

  cpf <- data[, 1]
  caf <- data[, 2]

  # calculate response rates
  cpr <- prop_rate(cpf)
  car <- prop_rate(caf)

  # repeat simulation nboot times
  aucsimu <- replicate(nboot, expr = {

    # simulate responses for both cp and ca
    cpsr <- boot_simu(cpf, cpr)
    casr <- boot_simu(caf, car)

    if(ca_adj == TRUE){
      casr <- id_adj(casr, lsize = lsize, csize = csize)
    }

    # auc for simulated data
    aucs <- roc_auc0(data.frame(cpsr, casr), byDR = byDR)

  } )

  return(aucsimu)

}


#=======================================
# EXPORT FUNCTION: auc_boot
#=======================================
# for multiple groups
#' @title Bootstrap AUCs
#' @description A function to simulate bootstrap samples and calculate AUC.
#' @param data A data frame or matrix saving both cp and ca frequencies. cp must precede ca.
#' @param group A vector indicating group membership. Will calculate AUCs by group.
#' @param nboot Number of bootstrap iterations for each group. Defaults to 1,000.
#' @param byDR Whether to order ids by diagnosticity ratios. Defaults to FALSE.
#' @param ca_adj Whether to adjust id rates for ca lineups *after* simulating a sample from the unadjusted rates.
#' @param lsize Size of lineup (used to adjust id rates). Defaults to 6.
#' @param csize Number of confidence levels (used to adjust id rates). Defaults to 3.
#' @return A list with simulated AUCs.
#'
#' @examples
#' cpf <- c(100, 90, 80, 20, 10, 5)
#' caf <- c(6, 7, 15, 50, 75, 120)
#' auc_boot(cbind(cpf, caf), nboot = 100)
#'
#' @export

auc_boot <- function(data, group = NULL,
                     nboot = 1000,
                     byDR = FALSE,
                     ca_adj = FALSE, lsize = 6, csize = 3) {

  message("Simulating ", nboot, " samples for each group...", "\n")

  if(ca_adj == TRUE) message("Adjusting ca rates...", "\n")

  #----------------------------
  # set up arguments
  # default arguments
  dargs <- formals()
  # entered arguments
  eargs <- as.list(match.call())[-1]

  # add default arguments
  fargs <- c(eargs, dargs[setdiff(names(dargs), names(eargs))])

  # remove the data and group arguments
  fargs[c("cpf", "caf", "group")] <- NULL

  #-----------------------------
  # save into a data frame
  data <- data.frame(data)

  # for a single group
  if(is.null(group)) {
    fargs[["data"]] <- data
    do.call(auc_boot0, fargs)
  }

  # simulate by group
  else {

    # empty auc list
    aucslist <- NULL

    for (g in unique(group)){

      dtmp <- data[group == g, ]

      fargs[["data"]] <- dtmp

      atmp <- do.call(auc_boot0, fargs)

      # list name
      gname <- ifelse(is.numeric(g), paste0("X", g), g)

      # save simulated auc
      aucslist[[gname]] <- atmp

    }
    return(aucslist)
  }

}



####################################
# FUNCTION TO COMPUTE CI FOR BOOTSTRAP SAMPLES
####################################

#========================
# generate mean, median, and CI; and calculate bootstrapping p-value.

mean_quan <- function(x, alpha){

  # mean
  m <- mean(x); names(m) <- "Mean"
  # quantile
  q <- quantile(x, probs = c(alpha/2, 0.5, 1-alpha/2))
  # p-value (2-sided)
  p <- ifelse(mean(x>0) > 0.5, 2*mean(x<=0), 2*mean(x>0))
  names(p) <- "p-value"

  y <- c(m, q, p)

  return(y)
}


#===================================
# Compute CIs
#' @title Bootstrap confidence intervals for AUC
#' @description A function to simulate bootstrap samples and calculate CIs for AUC and differences.
#' @param cpf A vector of cp frequencies.
#' @param caf A vector of ca frequencies.
#' @param group A vector indicating group membership. Will calculate AUC by group.
#' @param nboot Number of bootstrap iterations. Defaults to 1,000.
#' @param alpha Alpha level for the CIs. Defaults to 0.05.
#' @param ... Additional arguments in \link[fullROC:auc_boot]{auc_boot}. Will allow users to adjust ca id rates in each simulation.
#' @return A data frame of CIs for each group and group differences.
#'
#' @examples
#' cpf1 <- c(100, 90, 80, 20, 10, 5)
#' caf1 <- c(6, 7, 15, 50, 75, 120)
#' auc_ci(cpf1, caf1, nboot = 50)
#'
#' cpf2 <- c(90, 40, 20)
#' caf2 <- c(10, 70, 80)
#' auc_ci(cpf2, caf2, nboot = 100)
#'
#' ## compare two groups
#' cpf <- c(cpf1, cpf2)
#' caf <- c(caf1, caf2)
#' group <- rep(letters[1:2], times = c(length(cpf1), length(cpf2) ) )
#' auc_ci(cpf, caf, group = group)
#'
#' @importFrom stats median quantile
#' @export

auc_ci <- function(cpf, caf, group = NULL,
                   nboot = 1000,
                   alpha = 0.05,
                   ...){

  # check frequency input
  stopifnot("input must be frequencies" = sum(cpf >= 1) > 0,
            "input must be frequencies" = sum(caf >= 1) > 0)

  # set up data
  data <- data.frame(cpf, caf)

  # get bootstrap samples for auc
  ssample <- auc_boot(data = data, group = group, nboot = nboot, ...)

  # one group
  if(is.null(group)){
    ci <- mean_quan(ssample, alpha = alpha)
    return(ci)
  }

  # multiple groups
  else{
    # for each group
    ci <- lapply(ssample, mean_quan, alpha = alpha)

    # for group differences
    # empty difference list
    sdiff <- NULL

    # unique group
    ug <- unique(group)
    if(is.numeric(group)) ug <- paste0("X", ug)

    # number of groups
    ng <- length(ug)

    for(i in 1:(ng-1)){

      for(j in (i+1):ng){
        # select two groups
        g1 <- ug[i]
        g2 <- ug[j]

        sdiff[[paste(g1, g2, sep = "..")]] <- (ssample[[g1]] - ssample[[g2]])
      }
    }

    # ci for group differences
    ci_diff <- lapply(sdiff, mean_quan, alpha = alpha)

    return(t(data.frame(ci, ci_diff)))
  }

}

