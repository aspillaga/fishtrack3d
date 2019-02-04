#' Identify false or spurious detections in acoustic telemetry data
#'
#' False detections are defined as single detections within a predefined time
#' interval (usually 24h). They can be identified among all the detections of
#' the whole acoustic array or individually (providing the \code{receiver}
#' argument), in which case the the detections of each single acoustic receiver
#' are considered independently.
#'
#' @param tag.id vector with the ID codes of tagged individuals.
#' @param time.stamp vector with the date and time of each detection in
#'     \code{POSIXt} format.
#' @param rec.id vector with the ID codes of the receivers for each detection.
#'     Optional argument, if provided, the fuction will also identify the false
#'     detections within each receiver.
#' @param time.int time interval (in hours) that define false detections. It is
#'     set to 24h by default, as it is the most commonly used value.
#'
#' @return The function returns a data.frame with one or two logical vectors
#'     indicating if each detection has been identified as good (\code{TRUE}) or
#'     false (\code{FALSE}), taking into account the whole detection
#'     \code{array} (first column) or each \code{receiver} separately (optional
#'     second column, only if \code{receiver} is provided).
#'
#' @examples
#'
#' # Create a random dataset
#' set.seed(900)
#' time.stamp <- Sys.time() + c(1:24, 70:71, 200, 300:320, 500, 701:720) * 3600
#' tag.id <- factor(sample(c("i1", "i2"), length(time.stamp), replace = TRUE))
#' rec.id <- factor(sample(c("r1", "r2"), length(time.stamp), replace = TRUE))
#'
#' # False detections for the whole receiver array
#' f1 <- falseDetect(tag.id, time.stamp)
#' plot(time.stamp, tag.id, pch = (1:2)[rec.id])
#' points(time.stamp[!f1], tag.id[!f1], col = 2, cex = 2)
#'
#' # False detections within individual receivers
#' f2 <- falseDetect(tag.id, time.stamp, rec.id)
#' plot(time.stamp, tag.id, pch = (1:2)[rec.id])
#' points(time.stamp[!f2$receiver], tag.id[!f2$receiver], col = 4, cex = 2.4)
#' points(time.stamp[!f2$array], tag.id[!f2$array], col = 2, cex = 2)
#'
#' @export
#'
#'
#'
falseDetect <- function(tag.id, time.stamp, rec.id = NULL, time.int = 24) {

  # Check if the arguments are correct =========================================

  if (is.null(tag.id)) {
    stop("A vector with the ID codes of tagged individuals must be provided.",
         call. = FALSE)
  }

  if (is.null(time.stamp) | !class(time.stamp)[2] == "POSIXt") {
    stop("Time stamps must be in POSIXlt or POSIXct format.", call. = FALSE)
  }

  if (length(tag.id) != length(time.stamp)) {
    stop ("'tag.id' and 'time.stamp' must have the same length.",
          call. = FALSE)
  }

  if (!is.null(rec.id) & length(rec.id) != length(tag.id)) {
    stop ("'tag.id' and 'rec.id' must have the same length.", call. = FALSE)
  }


  # Loop over each individual ==================================================
  false.detect <- lapply(unique(tag.id), function(i) {

    # Subset data
    indx1 <- which(tag.id == i)

    # Detect false detections in the subset
    false.array <- detectSubset(time.stamp[indx1], time.int = time.int)

    false.array <- data.frame(indx = indx1, array = false.array)

    # Second loop over the receivers ===========================================
    # Only if the 'receiver' argument is provided

    if (!is.null(rec.id)) {

      false.receiver <- lapply(unique(rec.id[indx1]),function (r) {

        # Subset the data
        indx2 <- indx1[rec.id[indx1] == r]

        # Identify false detections
        false.receiver <- detectSubset(time.stamp[indx2], time.int = time.int)

        false.receiver <- cbind(subset(false.array, indx1 %in% indx2),
                                receiver = false.receiver)

        return(false.receiver)

      })

      false.array <- plyr::ldply(false.receiver)

    }

    return(false.array)

  })

  false.detect <- plyr::ldply(false.detect)
  false.detect <- false.detect[order(false.detect$indx), ]

  return(false.detect[, -1])

}


# UTILITY FUNCTIONS ############################################################
#
#
# Identify false or spurious detections in a subset of data
#
# Utility function to identify false detections in a subset of data. It is
# applied later within the function \code{falseDetect} in loops for individuals
# and receivers.
#
# @param time.subset Vector with the date and time of each detection in
#     \code{POSIXt} format.
# @param time.int Time interval (in hours) that define false detections. It is
#     set to 24h by default, as it is the most commonly used value.
#
#
detectSubset <- function(time.subset, time.int = 24) {

  ordered <- order(time.subset)

  # Time intervals between ordered detections
  diff.detect <- diff(time.subset[ordered])
  units(diff.detect) <- "mins"

  # Identify detections separated more than 'time' hours from the previous and
  # the next ones
  indx <- which(c(10^4, diff.detect) > (time.int * 60) &
                  c(diff.detect, 10^4) > (time.int * 60))

  true.det <- rep(TRUE, length(ordered))
  true.det[ordered[indx]] <- FALSE

  return(true.det)

}

