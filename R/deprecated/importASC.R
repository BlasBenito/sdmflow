#' Imports raster files in .asc format to a raster stack or brick
#'
#' @description Wrapper of the functions \code{\link[raster]{stack}}, \code{\link[raster]{brick}}, and \code{\link[sp]{CRS}} that facilitates loading .asc raster files into R as a stack or a brick.
#'
#' @usage importASC(
#' folder,
#' crs = "+init=epsg:4326",
#' to.memory = TRUE
#' )
#'
#' @param folder Folder storing the .asc raster files.
#' @param crs Character string defining a coordinate reference system as per the PROJ.4 standard. The default crs is \emph{"+init=epsg:4326"}, valid for data using latitude and longitude degrees as coordinates, and the global datum WGS84. Check the help file of \code{\link[raster]{crs}} for further details. The argument can be set to NULL, or "unknown" if the coordinate reference system is unknown.
#' @param to.memory If TRUE (default option), the function returns a raster brick residing in memory. Otherwise it returns a stack, which resides in the hard disk, and slows down most operations. If the computer has enough RAM memory, use TRUE to speed up any operation on the outcome of this function.
#'
#' @return A raster brick or stack.
#'
#' @author Blas Benito <blasbenito@gmail.com>. The functions \code{\link[raster]{stack}}, \code{\link[raster]{brick}}, and \code{\link[raster]{crs}} are authored by Robert J. Hijmans.
#' @export
importASC <- function(folder, crs = "+init=epsg:4326", to.memory = TRUE){

  #generating stack
  x <- raster::stack(
    x  = list.files(
      path = folder,
      pattern = '*.asc',
      full.names = TRUE
    )
  )

  #to brick
  if(to.memory){
    x <- raster::brick(x)
    }

  #adding crs
  if(is.null(crs) == FALSE){
    if(is.na(crs) == FALSE){
      if(crs != "unknown"){
        raster::projection(x) <- raster::crs(crs)
        }
      }
    }

  #returning object
  return(x)

}
