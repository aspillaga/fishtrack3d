% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/burstInfo.R
\name{burstInfo}
\alias{burstInfo}
\title{Extract information from the names of movement bursts}
\usage{
burstInfo(burst.names, time = NULL)
}
\arguments{
\item{burst.names}{Vector with the burst names, as created by the
\code{burstTrack} function.}

\item{time}{Vector of factors to separte the old bursts in new ones.}
}
\description{
This function takes the names that separate the different bursts within
individual trajectories (created by the \code{burstTrack} function), and
returns a data frame with the information about each burst (individual id,
date, day-night period, etc.). If the \code{time} argument is provided, the
function creates a new vector of burst-names grouping them depending on the
intervals specified by the argument.
}
