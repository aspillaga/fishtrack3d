#' Create colors with transparency
#'
#' This function easily creates transparent colors providing a color name and
#' the desired proportion of transparency
#'
#' @param color character vector with the names or codes of the original
#'     colors.
#' @param trans percentage of transparency to apply to the colors (0 to 100).
#'     If a unique value is provided, the same percentage of transparency will
#'     be applied to all the colors. If it is a vector of the same length as
#'     \code{color}, a given transparency will be applied to each color.
#' @param name optional name to give to the resulting colors.
#'
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
#'
#' @export
#'
#' @examples
#' plot(rnorm(1:100), rnorm(1:100), pch = 16, cex = 2, col = tCol("black", 70))
#' points(rnorm(1:100), rnorm(1:100), pch = 16, cex = 2, col = tCol("red", 40))
#' points(rnorm(1:100), rnorm(1:100), pch = 16, cex = 2, col = tCol("blue", 60))
#'
#'
tCol <- function(color, trans = 50, name = NULL) {

  # Check if arguments are correct =============================================

  if (is.null(color) | class(color) != "character") {
    stop("Color names or codes must be provided as 'character'.",
         call. = FALSE)
  }

  if (length(trans) > 1 & length(trans) != length(color)) {
    stop(paste("Transparency values must be of length 1 or equal to the",
               "length of colors."), call. = FALSE)
  }

  if (any(trans < 0) | any(trans > 100)) {
    stop("Transparency values must be between 0 and 100.", call. = FALSE)
  }

  if (!is.null(name) & length(name) != length(color)) {
    stop("A unique name must be provided for each color.", call. = FALSE)
  }


  t.colors <- lapply(seq_along(color), function(c) {

    # Get the RGB values of the original color
    rgb.val <- col2rgb(color[c])

    # Make a new color by setting the transparency with the alpha value
    t <- ifelse(length(trans) > 1, trans[c], trans)

    if (is.null(name)) {
      n <- NULL
    } else {
      n <- name[c]
    }

    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 maxColorValue = 255,
                 alpha = (100 - t) * 255 / 100,
                 names = n)

    invisible(t.col)

  })

  invisible(unlist(t.colors))

}
