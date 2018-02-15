#' Passive acoustic telemetry data of common dentex in the Medes Islands
#'
#' Data frame with passive acoustic telemetry monitoring data of two common
#' dentex (\emph{Dentex dentex}) individuals (\code{'dentex18'} and
#' \code{'dentex43'}) in the Medes Islands marine reserve (Catalonia, NW
#' Mediterranean Sea). The tracking period took place from 01/Oct/2007 to
#' 31/Dec/2007. Movements were tracked with a fixed array of 16 acoustic
#' \link{receivers}.
#'
#' @format Data frame with the following columns:
#'     \describe{
#'         \item{\code{tag.id}}{ID of the tagged individual}
#'         \item{\code{time.stamp}}{date and time of each detection, in
#'             \code{POSIXt} format}
#'         \item{\code{rec.id}}{ID of the acoustic receiver that registered
#'             each detection}
#'         \item{\code{depth}}{depth of the fish, provided by the acoustic
#'             transmitters in the coded signals}
#'     }
#'
#' @source
#' Aspillaga, E., 2017. \emph{Movement ecology of coastal fishes in a marine
#' protected area: implications for management and conservation}. Doctoral
#' thesis. Universitat de Barcelona. 218 pp.
#'
#'
"tracking"



#' Location of acoustic receivers in the Medes Islands
#'
#' Data frame with the geographical coordinates of the 17 acoustic receivers
#' placed around the Medes Islands marine reserve (Catalonia, NW Mediterranean
#' Sea). Coordinates are in UTM, referred to the datum WGS84 zone 31N.
#'
#' @format Data frame with the following columns:
#'    \describe{
#'        \item{\code{id}}{ID number of each receiver}
#'        \item{\code{long.utm}}{longitude of the coordinate, in m}
#'        \item{\code{lat.utm}}{latitude of the coordinate, in m}
#'     }
#'
#' @source
#' Aspillaga, E., 2017. \emph{Movement ecology of coastal fishes in a marine
#' protected area: implications for management and conservation}. Doctoral
#' thesis. Universitat de Barcelona. 218 pp.
#'
#'
"receivers"



#' Bathymetric raster
#'
#' Bathymetric raster of the Medes Islands marine reserve (Catalonia, NW
#' Mediterranean Sea). The coordinates are in UTM, referred to the datum WGS84
#' zone 31N.
#'
#' @format A \code{RasterLater} object. It has a resolution of 10x10 m and a
#'     total extension of 221x221 cells. Land areas are denoted by \code{NA}
#'     values.
#'
#'
"bathymetry"



#' Data from an acoustic signal range test experiment
#'
#' Data corresponding to an acoustic signal range test experiment performed in
#' the Medes Islands marine reserve (Catalonia, NW Mediterranean Sea). The
#' experiment was performed by placing receivers at 5 different distances
#' from one acoustic transmitter during a 24 hour period.
#'
#'
#' @format A data frame, where each row corresponds to a 1 h interval, and with
#'     the following columns:
#'    \describe{
#'        \item{\code{date.time}}{date and time of each 1h interval}
#'        \item{\code{tag.id}}{ID of the acoustic transmitter}
#'        \item{\code{rec.id}}{ID of acoustic receivers}
#'        \item{\code{dist.m}}{distance between the receiver and the
#'            transmitter, in m}
#'        \item{\code{rec.depth.m}}{depth at which the receiver was placed. For
#'            each distance from the transmitter, two receivers were placed at
#'            different depths (10 and 12 m)}
#'        \item{\code{detected}}{number of acoustic signals detected by the
#'            receiver in each 1h interval}
#'        \item{\code{emitted}}{total number of signals emitted by the
#'            transmitter in each  1h interval. It is calculated as the amount
#'            of signals detected by the receiver that was placed just next to
#'            the transmitter (\code{dist.m = 0})}
#'        \item{\code{det.ratio}}{acoustic efficiency ratio, i.e. ratio between
#'            the signals detected by the receivers at different distances and
#'            the total number of signals originally emitted by the
#'            transmitter}
#'     }
#'
#' @source
#' Aspillaga, E., 2017. \emph{Movement ecology of coastal fishes in a marine
#' protected area: implications for management and conservation}. Doctoral
#' thesis. Universitat de Barcelona. 218 pp.
#'
#'
"range_test"



#' Viewshed analysis output
#'
#' Output of the viewshed analysis performed for the acoustic receivers
#' (\code{\link{receivers}}) placed around the Medes Islands MPA (Catalonia, NW
#' Mediterranean Sea), using a local bathymetry raster
#' (\code{\link{bathymetry}}). The analysis was performed using the \code{GRASS}
#' software through the \code{rgrass7} package.
#'
#' @format A \code{RasterStack} object with one layer for each acoustic
#'     receiver.
#'
"viewshed"


# List of transition layers
#
# Explanation of the depth cost list
#
# @format A \code{list} of \code{TransitionLayer}, with one element for each
#     1 m depth-interval between 0 and 70 m, for the Medes Islands MPA. Each
#     \code{TransitionLayer} excludes the cells at depths shallower than the
#     depth-interval that defines each element in the list.
#
#
# "depth_cost_list"
