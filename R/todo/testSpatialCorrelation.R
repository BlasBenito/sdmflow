#' Spatial correlation of presence records over predictive variables
#'
#' @description Takes as input a set of coordinates and a brick or stack of environmental variables and applies \code{\link[ape]{Moran.I}} (Gittleman and Kot 1990) to analyze the spatial correlation of each variable for the given points
#'
#' @usage testSpatialCorrelation(xy, variables)
#'
#' @param xy A data frame with coordinates x and y. Column names are irrelevant as long as the first column represents the x coordinate, and the second column represents the y coordinate.
#' @param variables A raster brick or stack with environmental variables. Must be in the same reference system of \code{xy}.
#'
#' @return A data frame with the following columns:
#' \itemize{
#' \item \emph{variable}: variable name.
#' \item \emph{observed}: observed autocorrelation, Moran I. Values higher than the random expectation are considered significant.
#' \item \emph{expected}: random expectation of Moran I for the given data.
#' \item \emph{p.value}: p-value of the Moran I analysis.
#' }
#'
#' @examples
#' data("virtualSpecies")
#' data(europe2000)
#' sp.cor <- testSpatialCorrelation(
#'   xy = virtualSpecies$observed.presence,
#'   variables = europe2000
#' )
#' sp.cor
#'
#' @author Blas Benito <blasbenito@gmail.com>. The function \code{\link[ape]{Moran.I}} is authored by Julien Dutheil <julien.dutheil@univ-montp2.fr> and Emmanuel Paradis.
#' @references Gittleman, J. L. and Kot, M. (1990) Adaptation: statistics and a null model for estimating phylogenetic effects. Systematic Zoology, 39, 227–241.
#' @export
testSpatialCorrelation <- function(xy, variables){

  #removes scientific notation
  options(scipen = 9999)

  #extracts values of xy over variables
  xy.variables <- na.omit(
    data.frame(
      xy,
      raster::extract(
        x = variables,
        y = xy,
        df = TRUE,
        cellnumbers = FALSE
      )
    )
  )

  #removes a superfluous column
  xy.variables$ID <- NULL

  #inverse distance matrix between records
  xy.distancias <- 1/ (geosphere::distm(
    x = xy.variables[, c("x", "y")],
    fun = geosphere::distGeo
  ) / 1000)

  #replaces Inf with 0
  xy.distancias[!is.finite(xy.distancias)] <- 0

  #sets diagonal to 0
  diag(xy.distancias) <- 0

  #removes x and y columns
  xy.variables$x <- NULL
  xy.variables$y <- NULL

  #computes Moran I with ape::Moran.I
  moran.i <- apply(
    xy.variables,
    2,
    FUN = function(x) ape::Moran.I(x, xy.distancias, na.rm = TRUE)
  )

  #list to dataframe
  output.df <- as.data.frame(
    data.table::rbindlist(moran.i)
    )

  #rownames as column
  output.df <- data.frame(
    variable = colnames(xy.variables),
    output.df
  )

  #reseting rownames
  rownames(output.df) <- 1:nrow(output.df)

  #ordering by correlation
  output.df <- output.df[order(output.df$observed, decreasing = TRUE), ]

  #removes an ierrelevant column
  output.df$sd <- NULL

  return(output.df)

}
