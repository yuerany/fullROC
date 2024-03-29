% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc_line.R
\name{roc_line}
\alias{roc_line}
\title{A function to add an ROC curve to an existing ROC plot.}
\usage{
roc_line(cp, ca, byDR = FALSE, cumdata = FALSE, ...)
}
\arguments{
\item{cp}{A vector of cp id rates or frequencies.}

\item{ca}{A vector of ca id rates or frequencies.}

\item{byDR}{Whether to order ids by diagnosticity ratios. Defaults to FALSE.}

\item{cumdata}{Whether to output the cumulative data that are used to create the ROC curves. Default to FALSE.}

\item{...}{Additional plotting parameters.
For example, users can change x-axis and y-axis labels using \code{xlab} and \code{ylab}.}
}
\value{
Plot ROC curves and calculate AUCs as side effects.
}
\description{
A function to add an ROC curve to an existing ROC plot.
}
