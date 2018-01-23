#' Make spatial chronogram plots with acoustic telemetry data
#'
#' This function creates spatial chronogram plots to visualize passive acoustic
#' telemetry data. Spatial chronogram plots represent the receivers with the
#' largest number of receptions within predefined time intervals (e.g. 30 min)
#' in each day of the tracking period. They are an effective way to visualize,
#' on a fine temporal scale, presences and absences of an individual among
#' different zones of the receiver array.
#'
#' @param rec.id vector with the ID codes of the receivers for each detection.
#' @param time.stamp vector with the date and time of each detection in
#'     \code{POSIXt} format.
#' @param time.int Time interval for the Y axis of the plot as given to the
#'     \code{units} argument of the \code{\link[lubridate]{round_date}}
#'     function in the \code{lubridate} package. Default is set to
#'     \code{'30min'}.
#' @param colors optional \code{character} vector with the names of the colors
#'     to plot each receiver. The names of the elements in the vector must
#'     match with the receiver IDs in \code{rec.id}. If this argument is not
#'     provided, colors will be automatically assigned using a color palette,
#'     which can be provided in the \code{palette} argument.
#' @param palette if \code{colors} is not provided, a color palette can be
#'     specified to extract the colors. If \code{colors} or \code{palette} are
#'     not provided, a predefined color palette will be used.
#' @param include.all.rec logical, if \code{TRUE}, all the levels of the
#'     receiver IDs (in \code{rec.id} or \code{names(colors)}) will be included
#'     in the assignation of colors and when plotting the scale bar. If
#'     \code{FALSE}, only the receivers that appear in \code{rec.id} will
#'     appear in the scale bar.
#' @param xlim minimum and maximum date limits for the X axis, in \code{Date}
#'     format.
#' @param xaxs,yaxs,scale Logical arguments to plot (if \code{TRUE}) the X and
#'     Y axis and the lateral scale bar, respectively.
#' @param cex.lab the magnification to be used for the labels of the scale bar.
#'
#' @return The funtion makes a spatial chronogram plot.
#'
#' @references
#'     Aspillaga, E., F. Bartumeus, C. Linares, R.M. Starr, À. López-Sanz,
#'     D. Díaz, M. Zabala, and B. Hereu. 2016. Ordinary and extraordinary
#'     movement behaviour of small resident fish within a Mediterranean marine
#'     protected area. \emph{PLoS ONE}, 11: e0159813.
#'
#' @importFrom graphics box
#'
#' @export
#'
#' @examples
#'
#' data(tracking)
#' tracking$rec.id <- factor(tracking$rec.id)
#' tracking <- split(tracking, tracking$tag.id)
#'
#' par(mfrow = c(2, 1), mar = c(3.1, 4.1, 2.1, 4.1))
#' spatChronPlot(rec.id = tracking[[1]]$rec.id,
#'               time.stamp = tracking[[1]]$time.stamp)
#' title(main = names(tracking)[1])
#' spatChronPlot(rec.id = tracking[[2]]$rec.id,
#'               time.stamp = tracking[[2]]$time.stamp)
#' title(main = names(tracking)[1])
#'
#'
spatChronPlot <- function(rec.id, time.stamp, time.int = "30min",
                          colors = NULL, include.all.rec = TRUE,
                          palette = NULL, xlim = NULL, xaxs = TRUE,
                          yaxs = TRUE, scale = TRUE,
                          cex.lab = 0.9) {

  # Check if arguments are correct =============================================

  if (is.null(time.stamp) | !class(time.stamp)[2] == "POSIXt") {
    stop("Time stamps must be in POSIXlt or POSIXct format.", call. = FALSE)
  }

  if (length(rec.id) != length(time.stamp)) {
    stop("'rec.id' and 'time.stamp' must have the same length.", call. = FALSE)
  }

  if (!is.null(colors)) {
    if (is.null(names(colors))) {
      stop(paste("All the receiver IDs in 'rec.id' must be included in",
                 "'names(colors)'."), call. = FALSE)
    }
  }

  if (!is.factor(rec.id)) {

    if (!is.null(colors) & include.all.rec) {
      if (any(!unique(rec.id) %in% names(colors))) {
        stop (paste("All the receiver IDs in 'rec.id' must be included in",
                    "'names(colors)'"))
      }
      rec.id <- factor(rec.id, levels = names(colors))
    } else {
      rec.id <- factor(rec.id)
    }

  } else {

    if (!include.all.rec) {
      rec.id <- factor(rec.id)
    }

    if (!is.null(colors)) {
      if (any(!levels(rec.id) %in% names(colors))) {
        stop (paste("All the receiver IDs in 'rec.id' must be included in",
                    "'names(colors)'."), call. = FALSE)
      }
      if (include.all.rec & length(names(colors)) != length(levels(rec.id))) {
        warning(paste("The number of colours does not match the number of",
                      "receivers. Only receivers within levels(ref.id) will",
                      "be plotted."), call. = FALSE)
      }
    }

  }


  # Prepare the data matrix ====================================================
  m <- detect2matrix(time.stamp = time.stamp, rec.id = rec.id,
                     time.int = time.int, xlim = xlim)

  # Set the colors (if not provided)
  if (is.null(colors)) {
    colors <- recColor(unique.rec.id = levels(rec.id), palette = palette)
  }

  # Matrix with color names
  m.color <- apply(m, c(1, 2), function(x) {match(x, names(colors))})

  # Start the plot =============================================================
  image(m.color, col = colors,
        axes = FALSE, zlim = c(1, length(colors)), useRaster = TRUE)

  box()

  # X axis
  if(xaxs) {
    dateAxis(xlim = range(as.Date(row.names(m))))
  }

  # Y axis
  if (yaxs) {
    timeAxis(m.color)
  }

  # Scale Bar
  if (scale) {
    x <- levels(rec.id)
    colorScale(labels = x, colors = colors[x], title = "Receivers",
               cex.lab = cex.lab)
  }

}



# UTILITY FUNCTIONS ############################################################
#
#' Convert detection data into a matrix
#'
#' This function takes the original detections from acoustic telemetry data and
#' chooses the receiver with the largest amount of detections for each day and
#' a given time interval (e.g. 30 min).
#'
#' @param time.stamp vector with the date and time of each detection in
#'     \code{POSIXt} format.
#' @param rec.id vector with the ID codes of the receivers for each detection.
#' @param time.int time interval for the Y axis of the plot, as given to the
#'     \code{units} argument of the \code{\link[lubridate]{round_date}}
#'     function in the \code{lubridate} package.
#' @param xlim date limits for the x axis in \code{Date} format.
#'
#' @return A matrix with the IDs of the receivers for each day and time
#'     interval.
#'
#' @export
#'
#'
detect2matrix <- function(time.stamp, rec.id, time.int = "30min", xlim = NULL) {

  time.stamp <- lubridate::floor_date(time.stamp, time.int)
  time <- factor(format(time.stamp, "%H:%M:%S"))

  # Set date limits
  if (is.null(xlim)) {
    xlim <- range(as.Date(time.stamp))
  }

  date <- factor(seq(xlim[1], xlim[2], by = "days"))

  # Generate all the date and time combinations
  levels <- expand.grid(levels(time), levels(date))
  levels <- paste(levels[, 2], levels[, 1])
  time.stamp <- factor(time.stamp, levels = levels)

  # Table with the amount of detections in each receiver in each time interval
  table.rec <- table(time.stamp, rec.id, dnn = c("date", "receiver"))

  # Date and time values for the matrix
  tmp <- unlist(strsplit(rownames(table.rec), " "))
  date.matrix <- tmp[rep(1:2, length.out = length(tmp)) == 1]
  time.matrix <- tmp[rep(1:2, length.out = length(tmp)) == 2]

  # Select the receiver with the largest amount of detections. In case of
  # having the same maximum amount in two receivers, selects one haphazardly
  rec.tmp <- apply(table.rec, 1, function(x) {
    ifelse(sum(x) == 0, NA, sample(names(x)[x == max(x)]))
  })

  # Convert to a matrix
  mat <- matrix(rec.tmp, ncol = length(unique(time.matrix)), byrow = TRUE,
                dimnames = list(as.character(unique(date.matrix)),
                                unique(time.matrix)))

  return(mat)

}



#' Assign colors to receivers
#'
#' This function assigns a color to each of the unique receiver values using a
#' provided or predefined color palette.
#'
#' @param unique.rec.id vector with the unique names of the receivers.
#' @param palette a color palette to extract the colors. If not provided,
#'     a default palette is generated and used.
#'
#' @return A vector of color names, where the name of each element corresponds
#'     to the name of a receiver.
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
#'
#'
recColor <- function(unique.rec.id = NULL, palette = NULL) {

  # Chek if arguments are correct
  if (any(duplicated(unique.rec.id))) {
    warning ("Duplicated receiver IDs. Unique names will be used instead.",
             call. = FALSE)
    unique.rec.id <- unique.rec.id[!duplicated(unique.rec.id)]
  }

  if (is.null(palette)) {
    # Default palette
    palette <- colorRampPalette(c("firebrick", "orange", "yellow2", "green3",
                                  "cadetblue1", "dodgerblue", "darkorchid2"))
  }

  colors <- palette(length(unique.rec.id))
  names(colors) <- unique.rec.id

  return(colors)

}



#' Plot the X axis of a spatial chronogram plot (dates)
#'
#' Plots the date in the X axis of an spatial chronogram plot.
#'
#' @param xlim the minimum and maximum values for the axis in \code{Date}
#'     format.
#' @param spacing tick spacing. If \code{NULL}, it will be automatically
#'     choosen depending on the length of the period to plot. It can be set to
#'     'day' or 'month'.
#'
#' @return Plots the X axis in a spatial chronogram plot.
#'
#' @importFrom graphics par
#' @importFrom graphics axis
#'
#' @export
#'
#'
dateAxis <- function(xlim, spacing = c("auto", "month", "day")) {

  Sys.setlocale("LC_TIME", "C") # Month names in English

  date <- seq(xlim[1], xlim[2], "days")

  spacing <- match.arg(spacing)

  # Set the spacing of between ticks

  if (spacing == "auto" & diff(xlim) < 60 | spacing == "day") {

    # Primary label - Days
    sel.dat <- seq(xlim[1] + 1, xlim[2], by = round(length(date) / 10))
    sel.dat <- sel.dat[lubridate::day(sel.dat) != 1]
    main.lab <- lubridate::day(sel.dat)

    # Secondary label - Month
    sec.lab <- which(lubridate::day(date) == 1)
    sec.lab.format = "%b %Y"

    middle.tick <- TRUE

    hor = 0 # Adjustment of horizontal position for main labs (below the tick)
    ver = -0.5 # Adjustment of vertical position for main lab

  } else {

    # Primary label - Months
    sel.dat <- seq(lubridate::ceiling_date(xlim[1] - 10, "month"),
                   lubridate::floor_date(xlim[2], "month"), "month")
    main.lab <- format(sel.dat, "%b")

    # Secondary label - Years
    sec.lab <- which(lubridate::month(as.Date(date)) == 1 &
                       lubridate::day(as.Date(date)) == 1)
    sec.lab.format = "%Y"

    middle.tick <- FALSE

    hor = 1 # Adjustment position for main labs (between ticks)
    ver = -1

  }

  # All the date positions (depending on the 'middle.tick argument will be
  # placed before or in the middle of the column)
  all.pos <- (1:length(date) - 1) / (length(date) - 1) +
    ((1-middle.tick) * par("usr")[1])

  # Main tick and label positions
  main.tick <- all.pos[which(as.character(date) %in% as.character(sel.dat))]
  main.lab.pos <- main.tick + hor * diff(c(main.tick, par("usr")[2])) / 2

  # Secondary label positions
  second.tick <- all.pos[sec.lab]

  f = 0
  if (lubridate::month(xlim[1]) != 12) {
    second.tick <- c(par("usr")[1], second.tick)
    f = 1
  }

  # Plot the axis
  axis(1, at = main.tick, labels = FALSE, lwd = 0, lwd.ticks = 1)
  axis(1, at = main.lab.pos, labels = main.lab, lwd = 0, line = ver)

  axis(1, at = second.tick, labels = FALSE, lwd = 0,
       lwd.ticks = 1, tcl = -2.5)

  axis(1, at = second.tick,
       labels = format(as.Date(date)[c(f, sec.lab)], sec.lab.format),
       line = 0.6, lwd = 0, hadj = -0.1)

}



#' Plot the Y axis of a spatial chronogram plot (times)
#'
#' Plots the time of the day in the Y axis of an spatial chronogram plot.
#'
#' @param m matrix with the data to plot in the spatial chronogram plot.
#'
#' @return Plots the Y axis in a spatial chronogram plot.
#'
#' @importFrom graphics title
#'
#' @export
#'
#'
timeAxis <- function(m) {

  yp <- par("usr")[4] - 1

  a <- dim(m)[2] / 24

  ylabs <- seq(4, 22, by = 4)
  ypos <- ylabs * 2 * a * yp - yp
  ylabs <- strftime(strptime(ylabs, format = "%H"), format = "%Hh")

  axis(2, at = ypos, labels = ylabs, las = 2, lwd = 0, lwd.ticks = 1)

  title(ylab = "Hour", line = 2.6)

}



#' Plot the color scale in a spatial chronogram plot
#'
#' Plots a color scale bar in the right of the plot. It usually requires to
#' increase the right margin to fit.
#'
#' @param labels labels (receiver IDs) for each color cathegory.
#' @param colors vector with the color for each receiver.
#' @param title title for the scale bar.
#' @param title.adj ajustment of the title of the scale bar.
#' @param size Height of the scale from the bottom to the top, as percentage of
#'     the total height.
#' @param cex.lab the magnification to be used for the labels.
#'
#'
#' @return Plots the scale bar in the right of a spatial chronogram plot.
#'
#' @importFrom graphics par
#' @importFrom graphics rect
#'
#' @export
#'
#'
colorScale <- function(labels, colors, title, title.adj = -2.8, size = 1,
                       cex.lab = 0.9) {

  par(xpd = NA)

  bx <- par("usr")
  n.col <- length(colors)

  # Position in X axis
  bx.x <- c(bx[2] + (bx[2] - bx[1]) / 50,
            bx[2] + (bx[2] - bx[1]) / 50 + (bx[2] - bx[1]) / 30)

  # Position in Y axis
  bx.y <- c(bx[3], bx[3] + (bx[4]-bx[3]) * size)

  pos.rec <- seq(bx.y[1], bx.y[2], length.out = n.col + 1)
  height <- pos.rec[2] - pos.rec[1]
  pos.lab <- seq(bx.y[1] + (height/2), bx.y[2] - (height/2),
                 length.out = n.col)

  rect(xleft = bx.x[1], xright = bx.x[2],
       ybottom = pos.rec[-length(pos.rec)],
       ytop = pos.rec[-1], border = "black",
       col = as.character(rev(colors)))

  graphics::text(x = bx.x[2], y = pos.lab, pos = 4, labels = rev(labels),
                 offset = 0.3, cex = cex.lab)

  graphics::text(x = bx.x[2], y =  (bx.y[2] - bx.y[1])/2, labels = title,
                 srt = 270, adj = c(0.5, title.adj), cex = 1)

  par(xpd = FALSE)

}


