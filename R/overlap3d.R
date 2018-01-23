#' Spatial overlap between several utilization distributions
#'
#' This function takes a list of \code{RasterLayer}, \code{RasterStack} or
#' \code{RasterBrick} with utilization distribution (UDs) volumes (calculated
#' with \link{volumeUD}) and calculates the spatial overlap within a probability
#' contour between each pair of them.
#'
#' @param ud.vol.list list of \code{RasterLayer}, \code{RasterStack} or
#'     \code{RasterBrick} objects with the UDs volumes to overlap. If the
#'     elements of the list are \code{RasterStack} or \code{RasterBrick}
#'     objects, the number and name of the layers must coincide.
#' @param level UD volume probability contour to be used to calculate
#'     volume overlaps.
#' @param symmetric logical. If \code{TRUE}, the overlap indexes are calculated
#'     referred to the total joint volume of each pair of UD volumes. If
#'     \code{FALSE}, two overlap indexes are calculated for each pair of UDs,
#'     each one referred to the volume of one of the UDs to compare (see the
#'     \link{volOverlap} function.
#'
#' @return The function returns a matrix with the overlap indexes between each
#'     pair of UD volumes. The matrix will be symmetric if
#'     \code{symmetric == TRUE} or assymetric if \code{symmetric == FALSE}.
#'
#' @importFrom utils combn
#'
#' @export
#'
#'
overlap3d <- function(ud.vol.list, level, symmetric = TRUE) {

  # Check if arguments are correct =============================================

  if (is.null(ud.vol.list) | class(ud.vol.list) != "list") {
    stop("UDs must be provided on a list.", call. = FALSE)
  }

  # All the combinations between UDs
  names <- names(ud.vol.list)
  all.comb <- t(combn(names, 2))

  # Pair-wise overlapa
  over <- lapply(1:nrow(all.comb), function(i) {
    volOverlap(ud1 = ud.vol.list[[all.comb[i, 1]]],
               ud2 = ud.vol.list[[all.comb[i, 2]]],
               level = level, symmetric = symmetric)
  })

  # Create a distance matrix
  dist <- plyr::laply(names, function(x) {

    indx1 <- apply(all.comb, 1, function(i) x %in% i)

    dist.tmp <- plyr::laply(names, function(y) {

      if (x == y) {
        return(1)
      }

      indx2 <- which(indx1 & apply(all.comb, 1, function(i) y %in% i))

      if (symmetric) {
        return(over[[indx2]])
      } else {
        return(over[[indx2]][match(c(x, y), all.comb[indx2, ])][1])
      }

    })
  })

  colnames(dist) <- names
  rownames(dist) <- names
  return(dist)

}



#' Spatial overlap between two utilization distributions
#'
#' This function takes two \code{RasterLayer}, \code{RasterStack} or
#' \code{RasterBrick} objects with UD volumes and calculates the proportion
#' of the volume that is overlapped within a probability contour.
#'
#' @param ud1,ud2  \code{RasterLayer}, \code{RasterStack} or \code{RasterBrick}
#'     objects with the UD volumes to overlap. If it is a \code{RasterStack} or
#'     \code{RasterBrick} object, the number and name of the layers must
#'     coincide.
#' @param level UD volume probability contour to be used to calculate the
#'     volume overlap.
#' @param symmetric logical. If \code{TRUE}, the overlapped index is calculated
#'     referred to the total joint volume of the two UDs (volume(overlapped) /
#'     volume(\code{ud1}) + volume(\code{ud2})). If \code{FALSE}, two overlap
#'     indexes are calculated, the first one referred to the volume of
#'     \code{ud1} (volume(overlapped) / volume(\code{ud1})), and the second one
#'     referred to the volume of \code{ud2} (volume(overlapped) /
#'     volume(\code{ud2})).
#'
#' @return A vector with one (if \code{symmetric == TRUE} or two
#'     (\code{symmetric == FALSE}) overlap indexes.
#'
#' @export
#'
#'
volOverlap <- function(ud1, ud2, level, symmetric = TRUE) {

  # Check if arguments are correct =============================================
  if (is.null(ud1) | is.null(ud2) | class(ud1) != class(ud2) |
      any(!c(class(ud1), class(ud2)) %in% c("RasterLayer", "RasterBrick",
                                            "RasterStack"))) {
    stop(paste("Both UDs ('ud1' and 'ud2') must be provided as a ",
               "'RasterLayer', 'RasterBrick' or 'RasterStack' object."),
         call. = FALSE)
  }

  if (any(names(ud1) != names(ud2)) |
      any(raster::res(ud1) != raster::res(ud2))) {
    stop("The two UDs must have the same resolution and extension.",
         call. = FALSE)
  }

  if (is.null(level) | class(level) != "numeric" | level < 0 | level > 1) {
    stop("The probability contour ('level') must be a value between 0 and 1.",
         call. = FALSE)
  }

  data1 <- raster::values(ud1) <= level
  data2 <- raster::values(ud2) <= level

  overlap <- sum(data1 & data2)

  if (symmetric) {
    total <- sum(data1 | data2)
    return(round(overlap / total, 3))
  } else {
    return(round(c(overlap / sum(data1), overlap / sum(data2)), 3))
  }

}


