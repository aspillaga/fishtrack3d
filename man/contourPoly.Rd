% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contourPoly.R
\name{contourPoly}
\alias{contourPoly}
\title{Compute UD probability contours from 2D utilization distribution volumes}
\usage{
contourPoly(ud.vol, levels = c(0.5, 0.75, 0.9, 0.95))
}
\arguments{
\item{ud.vol}{a single \code{RasterLayer} object or a \code{RasterBrick} or
\code{RasterStack} object with distinct UD probability volumes.}

\item{levels}{vector with the probabilities of the contours to generate.}
}
\value{
a \code{SpatialPolygonsDataFrame} with all the computed UD contours.
}
\description{
This function takes a \code{RasterLayer}, \code{RasterBrick} or
\code{RasterStack} objects and generates the polygons that define the
given probability contours for each layer.
}
