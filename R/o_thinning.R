#' Applies "thinning" to a set of coordinates to reduce their spatial correlation
#'
#' @description This function reduces the spatial clustering of a set of presence records. It is intended to reduce spatial autocorrelation, and reduce sampling bias, particularly at larger geographical scales. It takes as input a set of coordinates, a brick or stack of environmental variables, and a minimum distance, and returns a new set of coordinates in which the distance between adjacent points is equal or higher than the established minimum distance. This operation is named "thinning", and helps to reduce the spatial correlation of a presence dataset. This function applies thinning, but preserves the presence records representing the extremes of any of the predictive variables provided.
#'
#' @usage o_thinning(
#' xy,
#' variables,
#' minimum.distance = NULL,
#' random.start = FALSE,
#' seed = NULL,
#' verbose = FALSE
#' )
#'
#' @param xy A data frame with two columns with coordinates x and y. Column names are irrelevant as long as the first column represents the x coordinate, and the second column represents the y coordinate.
#' @param variables A raster brick or stack with environmental variables. Must be in the same reference system of \code{xy}.
#' @param minimum.distance Numeric, minimum distance between consecutive points in the output dataset. Defaults to the resolution of \code{variables}. The minimum distance can be extracted from the resolution of \code{variables}, as in  \code{minimum.distance <- xres(variables)}.
#' @param random.start Boolean, defaults to \code{FALSE}. If \code{TRUE}, the \code{xy} dataset is randomly reshuffled so the start of the thinning changes, and with that, the output dataset.
#' @param seed Integer determining a random seed. Only relevant when \code{random.start = TRUE}. Added to allow reproducibility in the generation of datasets with a random start.
#' @param verbose Boolean. If \code{FALSE} (default), all messages are supressed.
#'
#' @return A data frame with the same columns as \code{xy}, but a lower number of records.
#'
#' @examples
#' data("virtual.species")
#' data(europe2000)
#' xy.thinned <- o_thinning(
#'   xy = virtual.species$observed.presence,
#'   variables = europe2000,
#'   minimum.distance = 4
#' )
#' xy.thinned
#'
#' #generating datasets with different starting points
#' #generates a different dataset with different nrow on each run
#' xy.thinned <- o_thinning(
#'   xy = virtual.species$observed.presence,
#'   variables = europe2000,
#'   minimum.distance = 4,
#    random.start = TRUE
#' )
#' nrow(virtual.species$observed.presence)
#' nrow(xy.thinned)
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
o_thinning = function(
  o.coordinates,
  variables,
  minimum.distance = NULL,
  random.start = FALSE,
  seed = NULL,
  verbose = FALSE
  ){

  #gets only two columns of the input dataset
  if(ncol(o.coordinates) > 2){o.coordinates <- o.coordinates[, 1:2]}

  #change column names
  old.column.names <- names(o.coordinates)
  names(o.coordinates) <- c("x", "y")

  #removes duplicates
  o.coordinates <- o.coordinates[!duplicated(o.coordinates), ]

  #computes minimum distance between points
  if(is.null(minimum.distance) == TRUE){
    minimum.distance <- raster::xres(variables)
    }

  #extracts variable values for xy
  o.coordinates <- na.omit(
    data.frame(
      o.coordinates,
      raster::extract(
        x = variables,
        y = o.coordinates,
        df = TRUE
      )
    )
  )

  #remove column
  o.coordinates$ID <- NULL

  #generates a data frame with the extreme values of variables
  indices.extreme.values <- vector()
  for(variable in names(o.coordinates)){
    indices.extreme.values <- c(indices.extreme.values, which.min(o.coordinates[, variable]), which.max(o.coordinates[, variable]))
  }

  #removes repeated records
  indices.extreme.values <- unique(indices.extreme.values)

  #generates a dataframe with the extreme values
  xy.extreme.values <- o.coordinates[indices.extreme.values, ]

  #removes those from xy
  o.coordinates <- o.coordinates[-indices.extreme.values, ]

  #applies random reshuffling of xy
  if(random.start == TRUE){
    if(is.null(seed) == FALSE){set.seed(seed)}
    o.coordinates <- o.coordinates[sample(nrow(o.coordinates)), ]
  }

  #count rows
  row<-1

  #loops through records
  repeat{

    #gets the current record
    f <- o.coordinates[row, ]

    #generates a bounding box around the record
    ymax <- f$y + minimum.distance
    ymin <- f$y - minimum.distance
    xmax <- f$x + minimum.distance
    xmin <- f$x - minimum.distance

    #selects other records within the bounding box and removes them
    o.coordinates <- o.coordinates[!((o.coordinates$y <= ymax) & (o.coordinates$y >= ymin) & (o.coordinates$x <= xmax) & (o.coordinates$x >= xmin) & (o.coordinates$y != f$y | o.coordinates$x != f$x)), ]

    #writes message
    if(verbose == TRUE){
      print(paste("Processed rows: ", row, " out of ", nrow(o.coordinates), sep=""))
    }

    #advances the counter one position
    row <- row+1

    #stops when there are no more records left
    if(row >= nrow(o.coordinates)){break}
  }

  #adds the records with extreme values
  o.coordinates <- rbind(xy.extreme.values, o.coordinates)[, c("x", "y")]

  return(o.coordinates)

}
