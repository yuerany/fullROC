% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust_id.R
\name{id_adj_pos}
\alias{id_adj_pos}
\title{Match by position}
\usage{
id_adj_pos(rate, fid, sid, lsize = 6)
}
\arguments{
\item{rate}{ID rate vector.}

\item{fid}{Mapping positions from filler id.}

\item{sid}{To-be-matched positions for suspect id. Must have equal length as fid.}

\item{lsize}{Lineup size. Defaults to 6.}
}
\value{
Adjusted ID vector.
}
\description{
A function to adjust the id rates for ca lineups using the 1/(lineup size) method;
match and adjust id rates by positions of filler and suspect ids.
}
\examples{
ca_id <- c(rep(0,3), rep(c(0.1, 0.15, 0.25), 2))
id_adj_pos(ca_id, fid = 4:6, sid = 1:3, lsize = 5)
}
