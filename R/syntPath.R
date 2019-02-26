#' Generate synthetic trajectories of fishes from acoustic telemetry data
#'
#' This function generates a stochastic trajectory of a fish by sampling a
#' possible pair of coordinates for each time step. It takes into account the
#' depth of the fish, the probability of being detected as function of the
#' distance to the receiver (including acoustic shadow areas outside the
#' acoustic range), and the topography of the study area.
#'
#' @param track.data \code{data.frame} with acoustic telemetry data containing
#'     the following columns:
#'         \describe{
#'             \item{\code{rec.id}}{ID of the acoustic receiver that registered
#'                 each detection}
#'             \item{\code{time.stamp}}{date and time of each detection, in
#'                 \code{POSIXt} format}
#'             \item{\code{depth}}{depth of the fish, provided by the acoustic
#'                 transmitters in the coded signals}
#'             \item{\code{tag.id}}{ID of the tagged individual (Optional)}
#'         }
#' @param topo raster dataset (\code{RasterLayer} object) with the topographic
#'     information of the study area (bathymetry or elevation).
#' @param dist.rec \code{RasterStack} object with the distances between all the
#'     cells and a given receiver in each layer. Acoustic shadows and other
#'     areas to exclude (e.g. land areas) must be identified with \code{NA}
#'     values.
#' @param ac.range.mod logistic regression model of the relationship between
#'     the detection probability and the distance from the receiver. It must be
#'     a \code{glm} object.
#' @param max.vel Maximum average velocity at which the animal is supposed to
#'     move. This argument is used to limit the maximum space between sampled
#'     locations. By default is set to 1 m/s.
#' @param depth.cost.list A list containing \code{TransitionLayer} objects,
#'     each one excluding the cells placed in areas shallower thant the depth
#'     that gives the name to the element in the list.
#' @param check Print a message after sampling each coordinate.
#'
#' @return A \code{data.frame} containing the time stamp, receiver, coordinates
#'     and depth of each sampled location.
#'
#' @export
#'
#'
syntPath <- function(track.data, topo, dist.rec, ac.range.mod, max.vel = 1,
                     depth.cost.list = NULL, check = FALSE) {

  # Check if arguments are correct =============================================

  if (is.null(track.data) | class(track.data) != "data.frame") {
    stop("Acoustic telemetry data must be provided as a 'data.frame'.",
         call. = FALSE)
  }

  if (any(!c("rec.id", "time.stamp", "depth") %in% colnames(track.data))) {
    stop(paste("'track.data' must contain the columns 'rec.id', 'time.stamp',",
               "and 'depth'."), call. = FALSE)
  }

  if (is.null(topo) | class(topo) != "RasterLayer") {
    stop("The topography ('topo') must be a 'RasterLayer' object.",
         call. = FALSE)
  }

  if (is.null(dist.rec) |
      !class(dist.rec) %in% c("RasterStack","RasterBrick")) {
    stop(paste("The 'dist.rec' object must be a 'RasterStack' or ",
               "'RasterBrick' object."), call. = FALSE)
  }

  if(!is.null(track.data$tag.id)) {

    id <- unique(track.data$tag.id)

    if (length(id) > 1) {
      stop(paste("The acoustic telemetry dataset contains the tracks of more",
                 "than one individual. Provide a dataset of only one of",
                 "them."), call. = FALSE)
    }
  }

  if (any(dim(topo)[1:2] != dim(dist.rec)[1:2])) {
    stop("The 'topo' and 'dist.rec' objects must have the same dimensions.",
         call. = FALSE)
  }

  # To avoid problems, we are going to creare validNames for receivers
  rec.unique <- unique(track.data$rec.id)
  rec.valid <- raster::validNames(rec.unique)

  track.data$rec.id <- rec.valid[match(track.data$rec.id, rec.unique)]
  names(dist.rec) <- raster::validNames(names(dist.rec))

  if (any(!unique(track.data$rec.id) %in% names(dist.rec))) {
    stop(paste("The 'dist.rec' object must have one matching layer for each",
               "receiver ID in the 'rec.id' column of 'track.data'."),
         call. = FALSE)
  }

  if (class(ac.range.mod)[1] != "glm") {
    stop("The 'ac.range.mod' object must be a 'glm' object.", call. = FALSE)
  }

  if (is.null(depth.cost.list) | is.null(depth.cost.list[["0"]])) {
    # If the 'depth.cost.list' is no provided, a transition layer that only
    # excludes land zones is be created.
    depth.cost.land <- leastCostMap(topo)
  } else {
    if (any(dim(topo)[1:2] != dim(raster::raster(depth.cost.list[[1]]))[1:2])) {
      stop(paste("The 'topo' object and the layers of 'depth.cost.list'",
                 "objects must have the same dimensions."), call. = FALSE)
    }
    depth.cost.land <- depth.cost.list[["0"]]
  }

  # Set control parameters =====================================================

  # Counter of the location being processed
  loc <- 1
  # Control parameter to indicate if the current location should be treated as
  # an error
  error <- FALSE
  # Number of the last Location that gave an error
  loc.error <- 0
  # Vector with locations that should be handled as errors
  error.locs <- numeric()
  # Number of trials used to obtain the current location
  trial <- 1
  # Maximum number of trials allowed to sample the current location, before
  # jumpic back to sample the previous one.
  max.trial <- 10
  # Number of locations that are being resampled back if 'max.trials' have been
  # reached
  back.step <- 1
  # Maximum number of locations to resample back before considering the
  # location as an error (in total there will be 10 * 3 = 30 trials).
  max.back.step <- 3


  # Start the loop =============================================================

  while (loc <= nrow(track.data)) {

    # Print current location (just for control)
    if (check) {
      print(paste0("loc = ", loc, "; trial = ", trial, "; back step = ",
                   back.step, "; loc.error = ", loc.error))
    }

    # Pick the first set of coordinates ========================================

    if (loc == 1) {

      prob.raster <- probRaster(dist.rast = dist.rec[[track.data$rec.id[1]]],
                                tag.depth = track.data$depth[1], topo = topo,
                                ac.range.mod = ac.range.mod)

      coord1 <- sampleCoord(prob.raster, 1)

      # Generate the structure of the output table
      out <- data.frame(loc = loc, rec.id = track.data$rec.id[1],
                        time.stamp = track.data$time.stamp[1],
                        x = coord1[1], y = coord1[2],
                        depth = track.data$depth[1], type = "original",
                        stringsAsFactors = FALSE)

      loc <- loc + 1
      error <- FALSE

      next

    }

    # Next iterations ==========================================================

    # If this step has been previously considered as problematic (after
    # expending all the trials), 'error' will be set to 'TRUE' and therefore,
    # the maximum distance will not be taken into account when sampling the
    # next location
    error <- ifelse(loc %in% error.locs, TRUE, FALSE)

    # Number of rows of the output table
    n <- nrow(out)

    # Coordinates of the previous sampled location
    coord.from <- as.numeric(out[n, c("x", "y")])

    # Receiver of the location that is being sampled
    rec.to <- track.data$rec.id[loc]

    # Maximum distance that the fish is able to swim during the time interval
    time.lag <- track.data$time.stamp[loc] - track.data$time.stamp[loc - 1]
    units(time.lag) <- "secs"
    max.dist <- as.numeric(time.lag) * max.vel


    #===========================================================================
    # We will first approximate the lineal location between the previous
    # sampled location and the position of the receiver if the current
    # location. If the distance is relativelly small compared to the maximum
    # distance (max.vel * elapsed.time), we are not going to limit the maximum
    # distance to the next coordinates. This will make the code a bit faster by
    # avoding calculating the distances between the previous location and all
    # the raster cells.

    rec.to.pos <- which.min(raster::values(dist.rec[[rec.to]]))
    rec.to.coord <- sp::coordinates(dist.rec[[rec.to]])[rec.to.pos, ]

    # Approximate lineal distance between previous location and the new one
    rec.to.dist <- sqrt(sum((rec.to.coord - coord.from)^2))

    # Check if the receiver is closer than half of the maximum distance
    proxim <- rec.to.dist < (max.dist / 2)

    #===========================================================================


    # If the current location has been considered as problematic, or if the
    # distance between the previous location and the current receiver is small,
    # we are not going to take into account the distance to sample the next
    # location

    if (error | proxim) {

      max.dist <- NULL
      bott.dist.r <- NULL

    } else {

      bott.dist.r <- raster::raster(topo)
      dist.val <- gdistance::costDistance(depth.cost.land,
                                          fromCoords = coord.from,
                                          toCoords =
                                            sp::coordinates(bott.dist.r))
      raster::values(bott.dist.r) <- as.numeric(dist.val)

    }

    # Obtain the probability raster to sample the coordinates
    raster.to <- probRaster(dist.rast = dist.rec[[rec.to]],
                            tag.depth = track.data$depth[loc],
                            ac.range.mod = ac.range.mod,
                            topo = topo, max.dist = max.dist,
                            dist.bottom = bott.dist.r)

    # Control structure ========================================================

    # If the receiver of the current location is too far from the previously
    # sampled location, the probabilities of the 'raster.to' will be too low to
    # be sampled. In this case, the 'raster.to' object returned by the
    # 'probRaster' function will be 'NULL', and the code will start to jump
    # back a progressive number of locations to try to find new coordintes that fit
    # better (until the maximum number of trials runs out).

    if (is.null(raster.to)) {

      loc.error <- ifelse(loc > loc.error, loc, loc.error)

      # We set the number of locations to go back and delete the selected
      # points from the outoput dataframe ('out')
      loc <- loc - back.step

      if (loc < 1) {
        # If the number of locations goes below 0, we set it to 1
        loc = 1
      } else {
        out <- out[1:(which(out$loc == loc) - 1), ]
      }

      trial <- trial + 1

      # If current number of trials is bigger than the maximum allowed
      # ('max.trial'), we make the code jump another location back
      if (trial > max.trial) {
        trial <- 1
        back.step <- back.step + 1

        # If the total number of trials runs out, we mark this location as
        # problematic (it will later give 'error = TRUE')
        if (back.step > max.back.step) {
          # Reset the control parameters
          back.step <- 1
          # We include the location in the problematic ones
          error.locs <- c(error.locs, loc.error)
        }
      }

      next
    }


    # Sample the next set of coordinates =======================================
    coord.to <- sampleCoord(prob.raster = raster.to, n = 1)


    # Get the shortest path between consecutive locations ======================

    # If the sampled coordinates are the same than the previous ones, we will
    # just add them to the output data.frame. If not, we will compute the
    # shortest path between the coordinates taking into account the emerged
    # landmasses

    if (!any(round(coord.from, 1) != round(coord.to, 1))) {

      path.coord <- data.frame(x = c(coord.from[1], coord.to[1]),
                               y = c(coord.from[2], coord.to[2]))

    } else {

      if (is.null(depth.cost.list)) {
        depth.cost.d <- depth.cost.land
      } else {
        # Choose the shallowest depth among the locations to join
        min.depth <- floor(min(c(track.data$depth[loc], out$depth[n])))
        depth.cost.d <- depth.cost.list[[as.character(min.depth)]]

        # depth.cost.d <- leastCostMap(topo = topo, min.depth = min.depth)
      }

      path.coord <- shortPath(from = coord.from, to = coord.to,
                              depth.cost = depth.cost.d)
    }

    # Interpolate time and depth for the path ==================================

    # Only if the path consists in more than two coordinates (is not a linear
    # segment)

    if (nrow(path.coord) == 2) {
      time.tmp <- track.data$time.stamp[loc]
      depth.tmp <- track.data$depth[loc]

    } else {

      trans.dist <- sapply(2:nrow(path.coord), function(i){
        sqrt((path.coord[i, 1] - path.coord[i - 1, 1]) ^ 2 +
               (path.coord[i, 2] - path.coord[i - 1, 2]) ^ 2)
      })


      trans.t <- cumsum(as.numeric(time.lag) * (trans.dist / sum(trans.dist)))

      time.tmp <- track.data$time.stamp[loc - 1] + trans.t

      depth.tmp <- out$depth[n] + (trans.t *
                                     (track.data$depth[loc] -
                                        out$depth[n]) / as.numeric(time.lag))
    }


    # Output table =============================================================

    out.tmp <- data.frame(loc = c(rep(NA, nrow(path.coord) - 2), loc),
                          rec.id = c(rep(NA, nrow(path.coord) - 2), rec.to),
                          time.stamp = time.tmp,
                          x = path.coord[-1, 1], y = path.coord[-1, 2],
                          depth = round(depth.tmp, 1),
                          type = c(rep("interp", nrow(path.coord) - 2),
                                   ifelse(error, "error", "original")),
                          stringsAsFactors = FALSE)

    out <- rbind(out, out.tmp)

    # Add 1 to the location counter
    loc <- loc + 1

    # Reset control parameters if the problematic location has been superated
    if (loc > loc.error) {
      back.step <- 1
      trial <- 1
    }
  }

  row.names(out) <- NULL

  # Original name of receivers
  out$rec.id <- as.character(rec.unique[match(out$rec.id, rec.valid)])

  # If 'tag.id' is available in the provided dataset, add it to the output
  if (!is.null(track.data$tag.id)) {
    out <- cbind(loc = out$loc, tag.id = id, out[, -1])
  }

  return(out)

}



#' Generate a least cost transition matrix from a topographical raster
#'
#' This function generates a \code{`TransitionLayer`} (\code{gdistance}
#' package) that excludes the cells that are outside a depth range, which is
#' used later to calculate the shortest path between two points that avoids
#' the excluded cells.
#'
#' @param topo raster dataset (\code{RasterLayer} object) with the topographic
#'     information of the study area (bathymetry or elevation).
#' @param min.depth,max.depth Minimum and maximum depths that the path is
#'     allowed to cross. If not provided, only areas that are marked with
#'     \code{NA} in the topographic raster will be avoided.
#'
#' @return A \code{`TransitionLayer`} object (\code{gdistance} package).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' dist.cost <- leastCostMap(bathymetry, min.depth = 30)
#' plot(raster(dist.cost))
#' }
#'
leastCostMap <- function(topo, min.depth = 0, max.depth = NULL) {

  if (is.null(topo) | class(topo) != "RasterLayer") {
    stop("'topo' must be a 'RasterLayer' object")
  }

  min.depth <- -min(abs(min.depth), na.rm = TRUE)

  # Generate a suitability raster
  # A very low suitability value (1e-08) will be assigned to no available
  # raster cells (cells outside the depth range), and a value of 1 to the
  # available cells
  suit <- raster::raster(topo)
  suit[topo <= min.depth] <- 1
  suit[topo > min.depth] <- 1e-08
  suit[is.na(topo)] <- 1e-08

  if (!is.null(max.depth)) {
    suit[topo < -abs(max.depth)] <- 1e-08
  }

  # Generate and geocorrect the transition layer
  cost <- gdistance::transition(suit, mean, directions = 16)
  cost <- gdistance::geoCorrection(cost, type = "c", multpl = FALSE)

  return(cost)

}



#' Create a raster of detection probabilities around a receiver
#'
#' Combines a distance raster for a receiver, the depth of the detection, and
#' an acoustic range model to generate a new \code{Rasterlayer} with the
#' probabilities of being detected around a receiver. It also requires a
#' topographic raster to exclude shallow cells. Optionally, if a raster of
#' distances from a point and a maximum distance is provided, excludes the
#' cells beyond that distance.
#'
#' @param dist.rast \code{RasterLayer} object with the distances from the
#'     location of one of the receivers to each of the raster cells. Acoustic
#'     shadows and land areas must be identified with \code{NA} values.
#' @param tag.depth single depth value at which the fish was observed. The
#'     probability of being detected in shallower cells will be set to 0.
#' @param ac.range.mod logistic regression model of the relationship between
#'     the detection probability and the distance from the receiver. It must be
#'     a \code{glm} object.
#' @param topo raster dataset (\code{RasterLayer} object) with the topographic
#'     information of the study area (bathymetry or elevation).
#' @param dist.bottom \code{RasterLayer} object with distances from a point
#'     (the last sampled coordinate), taking into account and avoiding emerged
#'     land areas. It is an optional argument, but it must be provided together
#'     with the \code{max.dist} argument.
#' @param max.dist maximum distance that the fish is allowed to swim from the
#'     previous location to the current one. It is calculated depending on the
#'     maximum speed of the fish and the time elapsed between detections. It is
#'     an optional argument, but must be provided together with the
#'     \code{dist.bottom} argument.
#' @param rec.depth depth at which the receiver is placed. It is used to
#'     incorporate the vertical distance between the transmitter and the
#'     receiver before applying the acoustic range model.
#' @param max.depth.diff optional argument to establish a maximum depth (below
#'     the \code{depth} value), below which exclude the coordinate sampling.
#'
#' @return A \code{RasterLayer} object with the spatial probability of being
#'     detected by a receiver.
#'
#' @export
#'
#' @examples
#' library(raster)
#'
#' # Create the acoustic range model
#' data(range_test)
#' det.range <- glm(det.ratio ~ dist.m, data = range_test,
#'                  family = quasibinomial(logit))
#'
#' # Probability raster for the receiver 'X1'
#' rec <- "X1"
#' prob.rast <- probRaster(dist.rast = viewshed[[rec]], tag.depth = 10,
#'                         topo = bathymetry, ac.range.mod = det.range)
#' plot(prob.rast)
#'
#'
probRaster <- function(dist.rast, tag.depth, ac.range.mod, topo,
                       dist.bottom = NULL, max.dist = NULL,
                       rec.depth = 5, max.depth.diff = NULL) {

  # Check if arguments are correct =============================================
  if (is.null(dist.rast) | class(dist.rast) != "RasterLayer") {
    stop("'dist.rast' must be a 'RasterLayer' object.", call. = FALSE)
  }

  if (is.null(topo) | class(topo) != "RasterLayer") {
    stop("'topo' must be a 'RasterLayer' object.", call. = FALSE)
  }

  if (is.null(ac.range.mod) | class(ac.range.mod)[1] != "glm") {
    stop("'ac.range.mod' object must be a 'glm' object.", call = FALSE)
  }

  if (is.null(dist.bottom) & !is.null(max.dist) |
      !is.null(dist.bottom) & class(dist.bottom) != "RasterLayer") {
    stop("'dist.bottom' object must be a 'RasterLayer' object.", call. = FALSE)
  }

  if (is.null(max.dist) & !is.null(dist.bottom) |
      !is.null(max.dist) & !is.numeric(max.dist)) {
    stop("'max.dist' must be a numeric value.", call. = FALSE)
  }


  # Remove shallow cells
  raster::values(dist.rast)[raster::values(topo) > -abs(tag.depth)] <- NA

  # Remove distanc cells if 'dist.bottom' has been provided
  if (!is.null(dist.bottom)) {
    raster::values(dist.rast)[raster::values(dist.bottom) > max.dist] <- NA
  }

  # Filter by maximum depth difference (if 'max.depth.diff' has been provided)
  if (!is.null(max.depth.diff)) {
    indx <- raster::values(topo) < -abs(tag.depth) - max.depth.diff
    raster::values(dist.rast)[indx] <- NA
  }

  # Add depth difference to the horizontal distance.
  dist.tmp <- sqrt(raster::values(dist.rast) ^ 2 +
                     (abs(tag.depth)- rec.depth) ^ 2)
  raster::values(dist.rast) <- dist.tmp

  # If there is no any suitable cell in the raster (or there is only 1),
  # return NULL
  if (sum(!is.na(raster::values(dist.rast))) <= 1) {
    return(NULL)
  }

  # Predict the detection probability using the 'glm' model
  dist <- data.frame(raster::values(dist.rast)
                     [!is.na(raster::values(dist.rast))])
  colnames(dist) <- as.character(ac.range.mod$terms[[3]])
  predicted <- predict(ac.range.mod, dist, type = "response")

  raster::values(dist.rast)[!is.na(raster::values(dist.rast))] <- predicted

  # Probabilities must sum 1
  raster::values(dist.rast) <-
    raster::values(dist.rast) / raster::cellStats(dist.rast, sum)

  return(dist.rast)

}


#' Sample pairs of coordinates from a probability raster
#'
#' This function takes a raster with the detection probabilities around a
#' receiver and samples \code{n} number of geografic coordinates according to
#' to the probabilities.
#'
#' @param prob.raster \code{RasterLayer} object with the probabilities of being
#'     detected by one receiver, as given by the \code{\link{probRaster}}
#'     function.
#' @param n number of pair of coordinates to return.
#'
#' @return A data frame with the \code{x} and \code{y} coordinates of the
#'     sampled points, in the same geographic reference system as the
#'     \code{prob.rast} object.
#'
#' @export
#'
#' @examples
#' library(raster)
#'
#' # Create the acoustic range model
#' data(range_test)
#' det.range <- glm(det.ratio ~ dist.m, data = range_test,
#'                  family = quasibinomial(logit))
#'
#' # Probability raster for the receiver 'X1'
#' rec <- "X1"
#' prob.rast <- probRaster(dist.rast = viewshed[[rec]], tag.depth = 10,
#'                         topo = bathymetry, ac.range.mod = det.range)
#' plot(prob.rast)
#'
#' # Sample coordinates
#' sampled.points <- sampleCoord(prob.rast, 10)
#' points(sampled.points, pch = "+")
#'
#'
sampleCoord <- function(prob.raster, n = 1) {

  indx <- !is.na(raster::values(prob.raster))

  point.indx <- sample(1:sum(indx), size = n,
                       prob = raster::values(prob.raster)[indx])
  points <- sp::coordinates(prob.raster)[indx, ][point.indx, ]

  return(points)

}



#' Get the shortest path between two points
#'
#' Calculates and returns the coordinates of the shortest path between two
#' points, taking into account a \code{TransitionLayer} generated by the
#' \code{leastCostMap} function.
#'
#' @param from,to vectors with the (x, y) coordinates of the initial and final
#'     points
#' @param depth.cost \code{TransitionLayer} returned by \link{leastCostMap}
#'     function.
#' @param simp.tol Tolerance (\code{tol}) argument for the
#'     \link[rgeos]{gSimplify} function.
#'
#' @return A data frame with the \code{x} and \code{y} coordinates defining the
#'     shortest path between the two points, in the same geographic reference
#'     system as the \code{depth.cost} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#'
#' depth.cost <- leastCostMap(bathymetry, min.depth = 20, max.depth = 30)
#' plot(raster(depth.cost))
#'
#' from <- c(518201, 4655442)
#' to <-  c(518290, 4654591)
#'
#' # Points can be also selected by directly clicking in the map
#' # from <- locator(1)
#' # to <- locator(1)
#'
#' path <- shortPath(from = from, to = to, depth.cost = depth.cost)
#'
#' points(rbind(from, to))
#' lines(path)
#' }
#'
#'
shortPath <- function(from, to, depth.cost, simp.tol = 10) {

  s.path <- gdistance::shortestPath(depth.cost, origin = as.numeric(from),
                                    goal = as.numeric(to), "SpatialLines")

  # Simplify the path to remove points within linear transects
  s.path <- rgeos::gSimplify(s.path, tol = simp.tol)

  path.coordinates <- data.frame(sp::coordinates(s.path)[[1]][[1]])

  return(path.coordinates)

}


