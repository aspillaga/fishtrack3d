#' Classify the time stamps of a track into 'day' and 'night' categories
#'
#' This function classifies the time stamps of a track into day and night
#' categories according to the local sunrise and sunset times.
#'
#' @param time.stamp vector with the date and time of the detections, in
#'     \code{POSIXt} format.
#' @param coord a \code{data.frame} or \code{matrix} object with a unique pair
#'     of coordinates or with a pair of coordinates for each time stamp ('x'
#'     and 'y' columns with the coordinates).
#' @param proj \link[rgdal]{CRS} object with the reference system of the
#'     coordinates, by default longlat WGS84.
#'
#' @return a character vector with a 'D' (day) or 'N' (night) label for each
#'     time stamp.
#'
#' @export
#'
#' @examples
#'
#' times <- Sys.time() + seq(-48, 48, 12) * 3600
#' coord <- cbind(x =  42.04818, y = 3.19555)
#' daynight <- getDayNight(times, coord)
#' daynight
#'
#'
getDayNight <- function(time.stamp, coord,
                        proj = sp::CRS("+proj=longlat +datum=WGS84")) {

  # This function needs the 'maptools' package to work
  if (!requireNamespace("maptools", quietly = TRUE)) {
    stop(paste("Package 'maptools' needed for this function to work. Please",
               "install it."), call. = FALSE)
  }

  # Check if arguments are correct =============================================
  if (is.null(time.stamp) | class(time.stamp)[2] != "POSIXt") {
    stop("Time stamps must be in 'POSIXt' format.", call. = FALSE)
  }

  if (is.null(coord) | !class(coord) %in% c("data.frame", "matrix") |
      ncol(coord) != 2) {
    stop(paste("Coordinates must be provided in a 'data.frame' or 'matrix'",
               "with the 'x' and 'y' columns."), call. = FALSE)
  }


  ref.proj <- sp::CRS("+proj=longlat +datum=WGS84")

  coord <- as.matrix(coord)

  if (proj@projargs != ref.proj@projargs) {
    points <- sp::SpatialPoints(coord, proj = proj)
    points <- sp::spTransform(points, CRSobj = ref.proj)
    coord <- sp::coordinates(points)
  }

  coordinates <- sp::SpatialPoints(matrix(coord, 1, 2), proj4string = ref.proj)

  sunset <- maptools::sunriset(coordinates, as.POSIXct(time.stamp),
                               POSIXct.out = TRUE, direction = 'sunset')$time
  sunrise <- maptools::sunriset(coordinates, as.POSIXct(time.stamp),
                                POSIXct.out = T, direction = 'sunrise')$time

  day.night <- ifelse(time.stamp >= sunrise & time.stamp < sunset, "D", "N")

  return(day.night)

}
