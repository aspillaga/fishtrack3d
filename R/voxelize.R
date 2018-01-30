#' Voxelize list of trajectories
#'
#' This function takes a list of 3D trajectories and calculates the average
#' proportion of time that they expend in each voxel of the grid in which the
#' study area is divided.
#'
#' @param synt.list list with data frames containing the 3D trajectories to
#'     process. Each element of the list must be a \code{data.frame} containing
#'     the columns \code{x}, \code{y}, \code{depth}, and \code{time.stamp}
#'     columns.
#' @param raster raster dataset (\code{RasterLayer} object) used to establish
#'     the horizontal dimension and resolution of the 3D grid.
#' @param depth.int sequence of depth values indicating the breaks into which
#'     the vertical axis is cut.
#' @param max.lag maximum time period, in hours, above which the segment
#'     between two locations will not be included in the rasterization.
#' @param time.step time interval, in minutes, into which the trajectory will
#'     be interpolated before the rasterization. This parameter is given to the
#'     \link{interpPath} function.
#' @param parallel option to apply the function in parallel (if \code{TRUE}).
#'     This will be passed to the \code{.parallel} argument of the
#'     \code{\link[plyr]{ldply}} function in the \code{plyr} package.
#'
#' @return The function returns a \code{RasterStack} object, with one layer for
#'     each depth interval. The name of each layer corresponds to the mean depth
#'     of the interval.
#'
#' @export
#'
#'
voxelize <- function(synt.list, raster, depth.int, max.lag = 24,
                     time.step = 2, parallel = FALSE) {

  # Check if arguments are correct =============================================

  if (is.null(synt.list) | class(synt.list) != "list") {
    stop("The 'synt.list' object must be a list containing trajectories.",
         call. = FALSE)
  }

  if (class(synt.list[[1]]) != "data.frame" |
      any(!c("x", "y", "depth", "time.stamp") %in% colnames(synt.list[[1]]))) {
    stop(paste("Trajectories in 'synt-list' must have the 'x', 'y', 'depth'",
               "and 'time.stamp' columns."), call. = FALSE)
  }

  if (is.null(raster) | class(raster) != "RasterLayer") {
    stop("The 'raster' object must be a 'RasterLayer' object.", call. = FALSE)
  }


  depth.int.m <- depth.int[-length(depth.int)]

  depth.int.lab <- apply(cbind(depth.int.m, depth.int[-1]), 1, mean)

  # Interpolate x, y and z positions ===========================================
  interp.list <- plyr::llply(synt.list, .parallel = parallel, function(x) {

    interp <- interpPath(path = x, max.lag = max.lag, time.step = time.step)

    depth.cut <- as.numeric(cut(interp$depth, depth.int, labels = depth.int.m))

    return(cbind(interp[, c("x", "y")], depth.cut))

  })

  # Rasterize trajectories =====================================================
  rast <- plyr::llply(depth.int.m, .parallel = parallel, function(d) {

    interp.tmp <- lapply(interp.list, function(x) {
      return(list(n = nrow(x), x[x$depth.cut == d, ]))
    })

    rast.tmp <- lapply(interp.tmp, function(x) {

      if (nrow(x[[2]]) > 0) {
        # Count the number of segments per raster cell
        raster.z <- raster::rasterize(x[[2]][, c("x", "y")], raster,
                                      background = 0)
        # Divide the number locations in each cell by the total number of
        # locations of the interpolated path
        raster::values(raster.z) <- raster::values(raster.z)/x[[1]]

      } else {

        raster.z <- raster::raster(raster)
        raster::values(raster.z) <- 0

      }

      return(raster.z)

    })

    depth.r <- raster::stack(rast.tmp)
    depth.r <- raster::stackApply(depth.r, 1, mean)

    return(depth.r)

  })

  rast <- raster::stack(rast)
  names(rast) <- paste0("d", depth.int.lab)

  return(rast)

}



#' Interpolate trajectory into points
#'
#' This function takes a single trajectory and interpolates it into a set of
#' points that are spaced by a given time step. If a maximum time lag is
#' provided, the function will not interpolate segments between locations that
#' are separated by more than the maximum lag.
#'
#' @param path a data.frame with the trajectory of a individual. It must
#'     contain the \code{time.stamp}, \code{x}, \code{y} and \code{depth}
#'     columns.
#' @param time.step time interval into which interpolate the trajectory, in
#'     minutes.
#' @param max.lag maximum time period, in hours, above which the trajectory
#'     between two locations will not be interpolated.
#'
#' @return a \code{data.frame} with the resulting interpolated locations.
#'
#' @export
#'
#'
interpPath <- function(path, time.step = 2, max.lag = NULL) {

  # Check if arguments are correct =============================================

  if (is.null(path) | !is.data.frame(path) |
      any(!c("time.stamp", "x", "y", "depth") %in% colnames(path))) {
    stop(paste0("'path' must be a 'data.frame' containing the 'time.stamp',",
                " 'x', 'y', and 'depth' columns."), call. = FALSE)
  }

  if (!class(path$time.stamp)[2] == "POSIXt") {
    stop("Time stamps must be in POSIXlt or POSIXct format.", call. = FALSE)
  }

  # If 'max.lag' is provided, separate the data frame into chunks where the
  # time between locations is bigger than the max.lag value

  diff <- diff(path$time.stamp)
  units(diff) <- "hours"
  diff <- as.numeric(diff)

  chunks <- cbind(1, length(diff))

  if (!is.null(max.lag)) {
    indx <- which(diff > max.lag)
    if (length(indx) > 1) {
      chunks <- cbind(c(1, indx + 1), c(indx, nrow(path)))
      chunks <- chunks[chunks[, 2] - chunks[, 1] != 0, ]
    }
  }

  # Interpolate chunks
  interp <- plyr::ldply(1:nrow(chunks), function(i) {

    path.sub <- path[chunks[i, 1]:chunks[i, 2], ]

    times <- seq(path.sub$time.stamp[1],
                 path.sub$time.stamp[nrow(path.sub)], time.step * 60)

    out.tab <- path.sub[match(times,
                              lubridate::floor_date(path.sub$time.stamp,
                                                    paste0(time.step,
                                                           "mins"))), ]

    out.tab$time.stamp <- times
    out.tab$x <- zoo::na.approx(out.tab$x)
    out.tab$y <- zoo::na.approx(out.tab$y)
    out.tab$depth <- zoo::na.approx(out.tab$depth)

    return(out.tab)

  })

  return(interp)

}

