#' Prepares training data for SDMs
#'
#' @description Produces "presence-only", "background" and "restricted background" data to fit species distribution models. The background is selected at random within the provided raster brick or stack, while the restricted background is selected within a buffer (usually based on the maximum dispersal distance of the target species) around the presence records. The selection of background points is made by the function \code{\link[dismo]{randomPoints}}, from the \code{dismo} package (Hijmans et al. 2017). The function can also apply thinning to the presence coordinates to reduce spatial autocorrelation through the function \code{\link{reduceSpatialCorrelation}}. To produce pseudo-absences instead of background data, just reduce the argument \code{n} to a number between the number and presences and twice the number of presences.
#'
#' @usage o_make_training(
#'   xy,
#'   variables,
#'   n = 10000,
#'   presence.only = FALSE,
#'   background = TRUE,
#'   restricted.background = FALSE,
#'   restricted.background.buffer = 200,
#'   plot = TRUE,
#'   thinning = FALSE,
#'   minimum.distance = raster::xres(variables)
#'   )
#'
#' @param xy A data frame with two columns with coordinates x and y representing presence records. Column names are irrelevant as long as the first column represents the x coordinate, and the second column represents the y coordinate.
#' @param variables A raster brick or stack with environmental variables. Must be in the same reference system of \code{xy}.
#' @param n Integer, number of background points to generate. To generate pseudo-absences just define a low n (i.e. twice the number of presences). If \code{n} is larger than the number of valid cells in \code{variables}, then all cells are selected as background.
#' @param presence.only Boolean. If \code{TRUE}, all other options are set to \code{FALSE}, and the function returns presence-only data.
#' @param background Boolean. If \code{TRUE}, all other options are set to \code{FALSE}, and the function returns background data generated to the extension of \code{variables}.
#' @param restricted.background Boolean. If \code{TRUE}, all other options are set to \code{FALSE}, and the function returns restricted background data generated within a buffer of width equeal to \code{restricted.background.buffer} around the presence records \code{xy}.
#' @param restricted.background.buffer Integer, buffer in kilometres around \code{xy} over which to define the buffer delimiting the restricted background.
#' @param plot Boolean. If \code{TRUE}, the output data is plotted.
#' @param thinning Boolean. If \code{TRUE}, the function \code{\link{reduceSpatialCorrelation}} is applied to \code{xy} to reduce the spatial aggregation of the data.
#' @param minimum.distance Numeric, minimum distance between consecutive points in the output dataset. Defaults to the resolution of \code{variables}. The minimum distance can be extracted from the resolution of \code{variables}, as in  \code{minimum.distance <- xres(variables)}.
#'
#' @return A data frame ready to fit an species distribution model.
#'
#' @examples
#' \dontrun{
#' data(virtual.species)
#' data(europe2000)
#'
#' #presence-only data
#' presence.only <- o_make_training(
#'   xy = virtual.species$observed.presence,
#'   variables = europe2000,
#'   presence.only = TRUE,
#'   plot = TRUE
#' )
#'
#' #background
#' background <- o_make_training(
#'   xy = virtual.species$observed.presence,
#'   variables = europe2000,
#'   n,
#'   background = TRUE,
#'   plot = TRUE
#' )
#'
#' #restricted background
#' restricted.background <- o_make_training(
#'   xy = virtual.species$observed.presence,
#'   variables = europe2000,
#'   n,
#'   restricted.background = TRUE,
#'   restricted.background.buffer = 100,
#'   plot = TRUE
#' )
#'
#' #applying thinning
#' restricted.background <- o_make_training(
#'   xy = virtual.species$observed.presence,
#'   variables = europe2000,
#'   n = 1000,
#'   restricted.background = TRUE,
#'   restricted.background.buffer = 100,
#'   plot = TRUE,
#'   thinning = TRUE,
#'   minimum.distance = raster::xres(europe2000)
#')
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>. The function \code{\link[dismo]{randomPoints}} is authored by Robert J. Hijmans.
#' @references Robert J. Hijmans, Steven Phillips, John Leathwick and Jane Elith (2017). dismo: Species Distribution Modeling. R package version 1.1-4. https://CRAN.R-project.org/package=dismo
#' @export
o_make_training <- function(
  o.coordinates,
  variables,
  n = 10000,
  presence.only = FALSE,
  background = TRUE,
  restricted.background = FALSE,
  restricted.background.buffer = 200,
  plot = TRUE,
  thinning = FALSE,
  minimum.distance = raster::xres(variables)
  ){

  #setting switches
  #--------------
  if(presence.only == TRUE){
    background <- restricted.background <- FALSE
  }
  if(restricted.background == TRUE){
    background <- FALSE
  }
  if(background == TRUE){
    restricted.background <- FALSE
  }

  #setting n
  #----------------
  #coercing to integer
  n <- as.integer(n)

  #removing duplicates
  #------------------
  o.coordinates <- o.coordinates[!duplicated(o.coordinates), ]

  #doing thinning
  #---------------
  if(thinning == TRUE){
    o.coordinates <- o_thinning(
      o.coordinates = o.coordinates,
      variables = variables,
      minimum.distance = minimum.distance
    )
  }

  #preparing presence
  #------------------
  presence <- data.frame(
    o.coordinates,
    raster::extract(
      x = variables,
      y = o.coordinates,
      df = TRUE,
      cellnumbers = FALSE
    )
  )
  presence$ID <- NULL

  #añadimos columna de presencia (presencia = 1)
  presence$presence <- 1


  #preparing background
  #-------------------
  if(background == TRUE){

    #number of valid cells
    n.valid <- sum(!is.na(as.vector(variables[[1]])))

    #if n > n.valid
    if(n > n.valid){

      #getting complete background
      background <- na.omit(
        raster::as.data.frame(
          x = variables,
          xy = TRUE
        )
      )

      #getting coordinates only
      background <- background[, c("x", "y")]


    } else {

      #getting background coordinates
      background <- data.frame(
        dismo::randomPoints(
          mask = variables,
          n = n
        )
      )

    }

    #keeping points with extreme values of the variables
    for(variable in names(variables)){

      #finding coordinates of cell with minimum value
      xy.min <- raster::xyFromCell(
        object = variables,
        cell = raster::which.min(variables[[variable]])[1]
      )

      #finding coordinates of cell with maximum value
      xy.max <- raster::xyFromCell(
        object = variables,
        cell = raster::which.max(variables[[variable]])[1]
      )

      #joining with background
      background <- rbind(background, xy.min, xy.max)
    }

    #removing duplicates
    background <- background[!duplicated(background), ]

    #adding variable values
    background <- data.frame(
      background,
      raster::extract(
        x = variables,
        y = background,
        df = TRUE,
        cellnumbers = FALSE
      )
    )
    background$ID <- NULL

    #adding presence column
    background$presence <- 0

    #merging data
    presence <- rbind(presence, background)

  }

  #preparing restricted background
  #-------------------------------
  if(restricted.background == TRUE){

    #coercing to integer
    restricted.background.buffer <- as.integer(restricted.background.buffer)

    #changing names of xy
    colnames(o.coordinates) <- c("x", "y")

    #xy to sp
    sp::coordinates(o.coordinates) <- c("x", "y")
    raster::crs(o.coordinates) <- raster::crs(variables)

    #generates buffer
    buffer <- dismo::circles(
      p = o.coordinates,
      d = restricted.background.buffer * 1000
      )
    buffer.dissolve <- rgeos::gUnaryUnion(buffer@polygons)

    #gets IDs of cells within the buffer
    buffer.cells <- unlist(
      raster::cellFromPolygon(
        object = variables,
        p = buffer.dissolve
        )
      )

    #generates mask from buffer cells
    buffer.mask <- raster::raster(variables[[1]])
    buffer.values <- rep(NaN, raster::ncell(buffer.mask))
    buffer.values[buffer.cells] <- 1
    buffer.mask <- raster::setValues(buffer.mask, values = buffer.values)
    buffer.mask <- raster::mask(buffer.mask, mask = variables[[1]])

    #generates background
    restricted.background <- data.frame(
      dismo::randomPoints(
        mask = buffer.mask,
        n = n
        )
      )

    #removing duplicates
    restricted.background <- restricted.background[!duplicated(restricted.background), ]

    #adding variable values
    restricted.background <- data.frame(
      restricted.background,
      raster::extract(
        x = variables,
        y = restricted.background,
        df = TRUE,
        cellnumbers = FALSE
      )
    )
    restricted.background$ID <- NULL

    #adding presence column
    restricted.background$presence <- 0

    #merging data
    presence <- rbind(presence, restricted.background)

  }

  #plotting presence pattern
  if(plot == TRUE){
   x <- SDMworkshop::plotVariable(
      variable = variables[[1]],
      points.x = rev(presence$x),
      points.y = rev(presence$y),
      points.groups = rev(presence$presence),
      points.size = 3
    )
   print(x)
  }

  return(presence)

}
