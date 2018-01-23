#' Thin acoustic telemetry data
#'
#' This function thins acoustic telemetry data into specified time intervals.
#' For each interval, a receiver ID an a depth value are assigned depending on
#' their probability distribution in the original data.
#'
#' @param track.data \code{data.frame} frame with acoustic telemetry data
#'     containing the following columns: \code{tag.id}, \code{rec.id},
#'     \code{time.stamp}, and \code{depth}. These arguments can be provided
#'     separately.
#' @param tag.id vector with the ID codes of tagged individuals.
#' @param rec.id vector with the ID codes of the receivers for each detection.
#' @param time.stamp vector with the date and time of each detection in
#'     \code{POSIXt} format.
#' @param depth vector with the depth values corresponding to each detection.
#' @param time.int time interval to which data will be thinned, as given to the
#'     \code{units} argument of the \code{\link[lubridate]{round_date}} function
#'     in the \code{lubridate} package.
#' @param depth.range vector with the minimum and maximum depth values,
#'     respectively, between wich the sampling of depths will occur.
#' @param parallel option to apply the function in parallel (if \code{TRUE}).
#'     This will be passed to the \code{.parallel} argument of the
#'     \code{\link[plyr]{ldply}} function in the \code{plyr} package.
#'
#' @return Data frame with 4 columns: tag.id, time.stamps, rec.id, and depth.
#'
#' @export
#'
#'
thinData <- function(track.data = NULL, tag.id, rec.id, time.stamp,
                     depth = NULL, time.int = "30min", depth.range = NULL,
                     parallel = FALSE) {

  # Check if the arguments are correct =========================================

  if (!is.null(track.data)) {

    if (class(track.data) != "data.frame" |
        any(!c("tag.id", "rec.id", "time.stamp") %in% colnames(track.data))) {
      stop(paste("'track.data' must be a 'data.frame' at least containing the",
                 "columns 'tag.id', 'rec.id' and 'time.stamp'."),
           call. = FALSE)
    }

    if (!class(track.data$time.stamp)[2] == "POSIXt") {
      stop("Time stamps must be in POSIXlt or POSIXct format.", call. = FALSE)
    }

  } else {

    if (is.null(tag.id)) {
      stop("A vector with the ID codes of tagged individuals must be provided.",
           call = FALSE)
    }

    if (is.null(rec.id)) {
      stop("A vector with the ID codes of the receivers must be provided.",
           call. = FALSE)
    }

    if (is.null(time.stamp) | class(time.stamp)[2] != "POSIXt") {
      stop("Time stamps must be in POSIXlt or POSIXct format.", call. = FALSE)
    }

    if (length(tag.id) != length(time.stamp) |
        length(tag.id) != length(rec.id) |
        length(rec.id) != length(time.stamp)) {
      stop ("'tag.id', 'rec.id' and 'time.stamps' must have the same length.",
            call. = FALSE)
    }

    track.data <- data.frame(tag.id, rec.id, time.stamp,
                             stringsAsFactors = FALSE)

    if (!is.null(depth)) {
      if (length(depth) != length(tag.id)) {
        stop("'depth' and 'tag.id' must have the same length")
      }

      track.data <- cbind(track.data, depth)

    }
  }


  # Loop for each unique 'tag.id' ==============================================
  data.tmp1 <- plyr::ldply(split(track.data, track.data$tag.id),
                           .parallel = parallel, .id = NULL, function(x) {

    x$trunc.time <- as.character(lubridate::floor_date(x$time.stamp, time.int))

    # Sample receivers
    rec.tmp <- tapply(x$rec.id, x$trunc.time, function(r) {
      rec.prob <- table(r)
      return(sample(names(rec.prob), 1, prob = rec.prob))
    })

    data.tmp2 <- data.frame(tag.id = x$tag.id[1],
                            rec.id = as.character(rec.tmp),
                            time.stamp = unique(x$trunc.time),
                            stringsAsFactors = FALSE)

    # Sample depths
    if (!is.null(x$depth)) {

      if (is.null(depth.range)) {
        depth.range <- c(floor(min(x$depth) - 10), ceiling(max(x$depth) + 10))
        depth.range <- ifelse(depth.range < 0, 0, depth.range)
      }

      depth.tmp <- tapply(x$depth, x$trunc.time, function(d) {

        if (length(d) == 1) {

          return(d)

        } else {

          depth.prob <- density(d, from = depth.range[1], to = depth.range[2])
          approx <- approx(depth.prob, xout = seq(depth.range[1],
                                                  depth.range[2], 0.1))
          value <- ifelse(sum(approx$y) != 0, sample(approx$x, size = 1,
                                                     prob = approx$y), NA)
          return(value)

        }
      })

      data.tmp2 <- cbind(data.tmp2, depth = as.numeric(depth.tmp))

    }

    return(data.tmp2)

  })

  data.tmp1$time.stamp <- strptime(data.tmp1$time.stamp,
                                   format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

  return(data.tmp1)

}

