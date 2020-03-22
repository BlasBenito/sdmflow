#' Transforms 4D object into a data frame
#'
#' @description Generates a data frame from a 4D object produced by \code{\link{import4D}}.
#'
#' @usage dfFrom4D(x)
#'
#' @param x Named list, 4D object produced by \code{\link{import4D}}.
#'
#' @return A data frame with all the valid cells of the raster bricks contained in \code{x}. The slot names of \code{x} are written in the column "time".
#'
#' @author Blas Benito <blasbenito@gmail.com>
#'
#' @export
dfFrom4D <- function(x){

  #gets names
  times <- names(x)

  #iterates through bricks in output.list
  for(time.step.i in times){

    #to df
    x[[time.step.i]] <- raster::as.data.frame(
      x = x[[time.step.i]],
      xy = TRUE,
      na.rm = TRUE
    )

    #time.step column
    x[[time.step.i]]$time <- time.step.i

  }

  #to data.frame
  output.df <- dplyr::bind_rows(x)

  return(output.df)

}
