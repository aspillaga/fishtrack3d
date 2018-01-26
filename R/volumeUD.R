#' Calculate utilization distribution probability volumes
#'
#' This function calculates the utilization distribution probability volumes
#' from 2D or 3D UD values.
#'
#' @param ud a \code{RasterLayer} (2D), \code{RasterStack} or
#'     \code{RasterBrick} (3D) object with UD values.
#'
#' @return a \code{RasterLayer} or a \code{RasterStack} object with the UD
#'     probability volumes.
#'
#' @export
#'
#'
volumeUD <- function(ud) {

  # Check if arguments are correct =============================================
  if (is.null(ud) | !class(ud) %in% c("RasterLayer", "RasterStack",
                                      "RasterBrick")) {
    stop(paste("Utilization distributions ('ud') must be in a 'RasterLayer',",
               "'RasterStack' or 'RasterBrick' object."), call. = FALSE)
  }

  ud <- ud / sum(raster::values(ud))
  rank <- (1:length(raster::values(ud)))[rank(raster::values(ud))]
  raster::values(ud) <- 1 - cumsum(sort(raster::values(ud)))[rank]

  return(ud)

}



#' Predict utilization distribution (UD) values from a \code{kde} object
#'
#' This function takes a \code{kde} object and predicts the utilization
#' distribution (UD) values for a 3D grid defined by a raster (horizontal
#' resolution) and a sequence of depths (vertical resolution.)
#'
#' @param kde a kde object.
#' @param raster raster to extract the horizontal coordinates of the 3D grid.
#' @param depths vector with depths values defining the vertical resolution of
#'     the 3D grid.
#'
#' @return A \code{RasterStack} object with the UD volumes of a different depth
#'     interval in each layer.
#'
#' @import raster
#' @import sp
#'
#' @export
#'
#'
predictKde <- function(kde, raster, depths) {

  # Check if arguments are correct =============================================
  if (is.null(kde) | class(kde) != "kde") {
    stop("The 'kde' object must be a 'kde' object from the 'ks' package.",
         call. = FALSE)
  }

  if (is.null(raster) | class(raster) != "RasterLayer") {
    stop("The 'raster' object must be a 'RasterLayer' object.", call. = FALSE)
  }


  pred <- lapply(depths, function(d) {
    rast.t <- raster::raster(raster)
    raster::values(rast.t) <- predict(kde,
                                      x = data.frame(sp::coordinates(rast.t),
                                                     z = -d))
    return(rast.t)
  })

  pred <- raster::stack(pred)
  names(pred) <- paste0("d", depths)

  return(pred)

}
