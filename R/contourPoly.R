#' Compute UD probability contours from 2D utilization distribution volumes
#'
#' This function takes a \code{RasterLayer}, \code{RasterBrick} or
#' \code{RasterStack} objects and generates the polygons that define the
#' given probability contours for each layer.
#'
#' @param ud.vol a single \code{RasterLayer} object or a \code{RasterBrick} or
#'     \code{RasterStack} object with distinct UD probability volumes.
#' @param levels vector with the probabilities of the contours to generate.
#'
#' @return a \code{SpatialPolygonsDataFrame} with all the computed UD contours.
#'
#' @importFrom grDevices contourLines
#'
#' @export
#'
#'
contourPoly <- function(ud.vol, levels = c(0.5, 0.75, 0.9, 0.95)) {

  # Check if arguments are correct =============================================

  if (is.null(ud.vol) | !class(ud.vol) %in% c("RasterLayer", "RasterBrick",
                                              "RasterStack")) {
    stop(paste("Utilization distribution volumes must be provided as a",
               "'Raster' object."), call. = FALSE)
  }


  indlist <- lapply(names(ud.vol), function(i) {

    tmp <- ud.vol[[i]]

    # Get the contours
    # First generate a matrix
    coord <- sp::coordinates(tmp)
    xyl <- list(x = unique(coord[, 1]), y = rev(unique(coord[, 2])))
    ud <- matrix(raster::values(tmp), ncol = length(xyl$y), byrow = FALSE)
    ud <- ud[, ncol(ud):1]

    re <- contourLines(x = xyl$x, y = xyl$y, z = ud, nlevels = 1,
                       levels = levels)
    cont.levels <- unlist(lapply(re, function(x) x$level))

    # Convert contours to polygons
    polygons <- lapply(re, function(l) {
      coord <- cbind(l$x, l$y)
      pol <- sp::Polygon(coord)
    })

    # Merge polygons of the same level
    polygons.merge1 <- lapply(levels, function(cl) {
      indx <- which(cont.levels == cl)
      pol.tmp <- polygons[indx]
      pol.tmp <- sp::Polygons(pol.tmp, paste0(i, "_", cl))
      return(pol.tmp)
    })

    polygons.merge2 <- sp::SpatialPolygons(polygons.merge1,
                                           proj4string = CRS(proj4string(tmp)))
    data <- data.frame(code = rep(i, length(levels)), level = levels,
                       row.names = paste0(i, "_", levels))
    polygons.merge2 <- sp::SpatialPolygonsDataFrame(polygons.merge2,
                                                    data = data)

    return(polygons.merge2)

  })

  polygons <- indlist[[1]]

  if (length(indlist) > 1) {

    for (i in 2:length(indlist)) {
      polygons <- rbind(polygons, indlist[[i]])
    }
  }

  return(polygons)

}
