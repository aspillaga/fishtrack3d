#' Extract information from the names of movement bursts
#'
#' This function takes the names that separate the different bursts within
#' individual trajectories (created by the \code{burstTrack} function), and
#' returns a data frame with the information about each burst (individual id,
#' date, day-night period, etc.). If the \code{time} argument is provided, the
#' function creates a new vector of burst-names grouping them depending on the
#' intervals specified by the argument.
#'
#'
#' @param burst.names Vector with the burst names, as created by the
#' \code{burstTrack} function.
#' @param time Vector of factors to separte the old bursts in new ones.
#'
#' @export
#'
#'
burstInfo <- function(burst.names, time = NULL) {

  variables <- data.frame(code = c("i", "d", "p", "n", "y", "m", "w"),
                          names = c("individual", "day", "phase", "n", "year",
                                    "month", "week"))

  # This part of the code deletes the prefix 'index_' that the stackApply'
  # function in 'raster' package sometimes adds to the resulting layer
  if(any(grepl("index_", x = burst.names))) {
    burst.names <- gsub("(index_)(.*)", replacement = "\\2", x = burst.names)
  }


  ncol <- length(gregexpr(pattern = "_", text = burst.names[1])[[1]]) + 1

  names <- matrix(unlist(strsplit(burst.names, "_")), ncol = ncol,
                  byrow = TRUE)
  colnames(names) <- substr(names[1, ], start = 1, stop = 1)
  names <- substr(names, start = 2, stop = 50)

  data.f <- data.frame(names, stringsAsFactors = FALSE)
  colnames(data.f) <- variables$names[match(colnames(data.f), variables$code)]

  if ("day" %in% colnames(data.f)) {
    # If the column 'day' exists (date in julian format), extracts the year,
    # the month, and the weekday

    data.f$date <- as.Date(as.numeric(data.f$day),
                           origin = as.Date("1970-01-01"))

    data.f$year <- as.numeric(format(data.f$date, format  ="%Y"))
    data.f$month <- as.numeric(format(data.f$date, format  ="%m"))
    data.f$week <- as.numeric(format(data.f$date, format  ="%W"))

  }

  if (is.null(time)) {

    return(data.f)

  } else {

    indx <- character()

    for (r in 1:length(time)) {

      indx <- paste0(indx, substr(time[r], 1, 1), data.f[, time[r]])

      if (r != length(time)) {

        indx <- paste0(indx, "_")

      }
    }

    return(indx)

  }
}
