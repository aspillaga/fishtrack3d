% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDayNight.R
\name{getDayNight}
\alias{getDayNight}
\title{Classify the time stamps of a track into 'day' and 'night' categories}
\usage{
getDayNight(time.stamp, coord,
  proj = sp::CRS("+proj=longlat +datum=WGS84"))
}
\arguments{
\item{time.stamp}{vector with the date and time of the detections, in
\code{POSIXt} format.}

\item{coord}{a \code{data.frame} or \code{matrix} object with a unique pair
of coordinates or with a pair of coordinates for each time stamp ('x'
and 'y' columns with the coordinates).}

\item{proj}{\link[rgdal]{CRS} object with the reference system of the
coordinates, by default longlat WGS84.}
}
\value{
a character vector with a 'D' (day) or 'N' (night) label for each
    time stamp.
}
\description{
This function classifies the time stamps of a track into day and night
categories according to the local sunrise and sunset times.
}
\examples{

times <- Sys.time() + seq(-48, 48, 12) * 3600
coord <- cbind(x =  42.04818, y = 3.19555)
daynight <- getDayNight(times, coord)
daynight


}
