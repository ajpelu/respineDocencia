#' Distance to Natural forests
#'
#' Compute the distance between each pixel and the edges of all natural forests.
#' Then get the minimun distance for each pixel.
#'
#' @param x A \code{raster} object
#'
#' @param nf_value The value of "Natural Forests" class within the raster (Default value = 2)
#'
#' @return A \code{raster} object with the minimun distance for each raster cell
#'
#'
#' @import raster
#' @import rgeos
#' @author Antonio J Perez-Luque


dist2nf <- function(x, nf_value){

  # Get boundary limits of NF, and save as shapefile
  nf_edges <- rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)

  # get distance between each cell(as points) and nf_edges
  dd = gDistance(nf_edges, as(x,"SpatialPoints"), byid=TRUE)

  # Get minimun distance
  dist_r <- x
  dist_r[] = apply(dd,1,min)

  return(dist_r)
}
