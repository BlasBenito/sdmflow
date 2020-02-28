#' Imports predictive variables defined for different times
#'
#' @description Imports raster layers representing different variables at different times, and applies to them a common mask considering all their respective empty cells. The raster layers should be stored as individual files with any extension readable by the \code{\link[raster]{stack}} function, including ".asc", ".tif", and many others.
#'
#' \strong{Requirements}
#'
#' The raster layers to import must fulfill the following requirements:
#'
#' \itemize{
#' \item Same extension and resolution.
#' \item Same coordinate reference system (crs). It it is not included in the files (usually the case with .asc files), it can be defined through the argument \code{vars.crs}.
#' \item Variable names must not be nested. For example, "temp" is nested in "temperature", and therefore the function will be unable to separate both. Howeve, non-nested names, such as "temp" and "prec" will work totally fine.
#' \item The variable name must contain a string representing the time step. Valid examples are "var_name_2010", "varName_January". Please note that string matching is case INSENSITIVE, and therefore time-step names such as "january" and "January" will be grouped together automatically.
#' \item All variables must be available for every time-step. The function does not specifically check this assumption, and errors may arise from its violation.
#' }
#'
#'\strong{Data organization}
#'
#'When working with SDMs across time, a set of variables are \emph{dynamic} and change over time (i.e. climate), while others are \emph{static} do not change significantly over time (i.e. topography). This function allows to select static and dynamic variables through the arguments \code{static.vars} and \code{dynamic.vars}. This way the user does not need to have the static variables duplicated as many times as time steps (years, months, etc) are considered in the analysis.
#'
#'The data can be organized as desired by the user, as long as all files are inside the folder defined in the \code{folder} argument. Any of these methods is recommended:
#'
#'\itemize{
#'\item All files stored in the same folder, with a name of the type \code{name_time} for the dynamic variables, and \code{name} for the static ones. For example, the folder "variables" could have inside the files "temperature_2010.tif", "temperature_2011.tif", and "slope.tif".
#'\item Files stored in folders with the variable names, and files named with the time name. For example, the folder "dynamic/temperature" could have the files "2010.tif", "2011.tif", and the folder "static" could have the file "slope.tif"
#'\item Files stored in variables named after the time steps. For example, the folder "2010" would have the file "temperature.tif" for 2010, and the folder "2011" would have the file "temperature.tif" for 2011, while the "static" folder would have the file "slope.tif".
#'}
#'
#'
#'\strong{Using a raster template}
#'
#'The argument \code{raster.template} allows to select a given raster layer with a particular resolution and a given coordinate reference system (that can alternatively be provided through the argument \code{raster.template.crs}). If selected, all imported variables will be reprojected to the coordinate reference system of the template, and rescaled to the extension and resolution of the template. Take in mind that for large datasets this operation might take a while, and may require a large amount of RAM memory.
#'
#' @usage import4D(
#' raster.template = NULL,
#' raster.template.crs = NULL,
#' folder = here::here(),
#' dynamic.vars = NULL,
#' static.vars = NULL,
#' vars.crs = NULL,
#' times = NULL,
#' to.data.frame = FALSE
#' )
#'
#' @param raster.template Complete path to a raster file or raster object to be used as a template to reproject and rescale all imported variables.
#' @param raster.template.crs Character string defining a coordinate reference system of the template as per the PROJ.4 standard. Only required when the raster format does not contain this information (as it happens with .asc files). The default crs is \emph{"+init=epsg:4326"}, valid for data using latitude and longitude degrees as coordinates, and the global datum WGS84. Check the help file of \code{\link[raster]{crs}} for further details. The argument can be set to NULL, or "unknown" if the coordinate reference system is unknown.
#' @param folder Character string, path (without final slash) to the folder where the raster files, or folder containing the raster files, are stored. Defaults to the given working folder of the R session.
#' @param dynamic.vars Character vector, names of the variables that change over time. For example \code{c("temperature", "rainfall")}.
#'
#' @param static.vars Character vector, names of the variables that change over time. For example \code{c("slope", "aspect")}.
#'
#' @param vars.crs Character string, definition of the coordinate reference system of the variables. If not provided, and not included in the file formats, the function cannot reproject the static and dynamic variables to the coordinate reference system of \code{raster.template}.
#' @param times Character vector with the names of the times represented by the raster layers. For example \code{c("2010", "2020")} or \code{c("january", "february")}. These names must be available either in the names of the files, or the names of the folders where the files are. Names are matched as lower-case.
#' @param to.data.frame Boolean. If TRUE, the function returns a large data frame were cases represent raster cells over space and time, and columns represent environmental variables. The values of the \code{times} argument are included in the \code{time} column.
#'
#' @return If no \code{raster.template} is provided, a named list, with slots named after \code{times} containing raster bricks with the given variables. If \code{raster.template} is provided, a named list with raster bricks. If \code{to.data.frame = TRUE}, a single dataframe with the values of all the valid cells available in the raster layers.
#'
#' @author Blas Benito <blasbenito@gmail.com>. The functions \code{\link[raster]{stack}}, \code{\link[raster]{brick}}, \code{\link[raster]{crs}}, \code{\link[raster]{projectRaster}} and \code{\link[raster]{resample}} are authored by Robert J. Hijmans.
#' @export
import4D <- function(
  raster.template = NULL,
  raster.template.crs = NULL,
  folder = here::here(),
  dynamic.vars = NULL,
  static.vars = NULL,
  vars.crs = NULL,
  times = NULL,
  to.data.frame = FALSE
  ){

  #begins cluster
  raster::beginCluster()

  #listing files in folder
  file.paths <- list.files(
    path = folder,
    recursive = TRUE,
    full.names = TRUE
  )

  #getting file names
  file.names <- list.files(
    path = folder,
    recursive = TRUE,
    full.names = FALSE
  )

  #object to store results
  output.list <- list()

  #subset file.names and file.paths by
  #static and dynamic variables
  selected.files <- grep(
    pattern = paste(dynamic.vars, collapse = "|"),
    x = file.names,
    ignore.case = TRUE
  )
  if(is.null(static.vars) == FALSE){
    selected.files <- c(
      selected.files,
      grep(
        pattern = paste(static.vars, collapse = "|"),
        x = file.names,
        ignore.case = TRUE
      )
    )
  }
  file.names <- file.names[selected.files]
  file.paths <- file.paths[selected.files]

  #indices of static vars
  if(is.null(static.vars) == FALSE){
    static.vars.indices <- grep(
      pattern = paste(static.vars, collapse = "|"),
      x = file.names
    )
  } else {
    static.vars.indices <- NA
  }

  #iterating through time
  for(time.step.i in times){

    #get indices with time.step.i and static vars in file.names
    file.indices.i <- na.omit(
        c(
        grep(
          pattern = time.step.i,
          x = file.names
          ),
        static.vars.indices
      )
    )

    #get paths
    file.paths.i <- file.paths[file.indices.i]

    #generates stack in output.list
    output.list[[time.step.i]] <- raster::stack(x = file.paths.i)

    #if there is no crs
    if(is.null(vars.crs) == FALSE){
      if(is.na(raster::crs(output.list[[time.step.i]])) == TRUE){
        raster::crs(output.list[[time.step.i]]) <- vars.crs
      }
    }

  } #end of iterations

  #if a reference raster is provided
  if(is.null(raster.template) == FALSE){

    #imports the reference raster
    if(is.character(raster.template) == TRUE){
      raster.template.path <- raster.template
      raster.template <- raster::raster(raster.template.path)
    }

    #if the crs of the template is empty, adds crs
    if(is.na(raster::crs(raster.template)) == TRUE){
      raster::crs(raster.template) <- raster.template.crs
    }

    #iterates through stacks in output.list
    for(time.step.i in times){

      #equalizing crs
      if(is.na(raster::crs(raster.template)) == FALSE & is.na(raster::crs(output.list[[time.step.i]])) == FALSE){

        #tests if the crss are equal
        equal.crs <- raster::compareRaster(
          raster.template,
          output.list[[time.step.i]],
          extent = FALSE,
          rowcol = FALSE,
          crs = TRUE,
          res = FALSE,
          orig = FALSE,
          rotation = FALSE,
          values = FALSE,
          stopiffalse = FALSE
        )

        #applies reprojection if they are not
        if(equal.crs == FALSE){

          #applies projection
          output.list[[time.step.i]] <- raster::projectRaster(
            from = output.list[[time.step.i]],
            to = raster.template,
            method = "bilinear"
          )

        }

      } else {

        #compares ext and res of the template and the stack
        equal.dimensions <- raster::compareRaster(
          raster.template,
          output.list[[time.step.i]],
          extent = TRUE,
          rowcol = TRUE,
          crs = FALSE,
          res = TRUE,
          orig = FALSE,
          rotation = FALSE,
          values = FALSE,
          stopiffalse = FALSE
        )

        #resamples stack if equal.dimensions is FALSE
        if(equal.dimensions == FALSE){

          output.list[[time.step.i]] <- raster::resample(
            x = output.list[[time.step.i]],
            y = raster.template,
            method = "bilinear"
          )

        }

      } #end of else

    }#end of iterations

    }#end of raster.template is provided

    #applies mask
    for(time.step.i in times){

      #get mask
      mask <- raster::calc(output.list[[time.step.i]], fun = sum)

      #adds mask of raster.template if available
      # if(class(raster.template) == "RasterLayer"){
      #   mask <- mask + raster.template
      # }

      #applies mask
      output.list[[time.step.i]] <- raster::mask(
        x =  output.list[[time.step.i]],
        mask = mask
      )

      #replaces names
      names(output.list[[time.step.i]]) <- c(dynamic.vars, static.vars)

    }

    #to data.frame
    if(to.data.frame == TRUE){

      output.list <- dfFrom4D(x = output.list)

    }

    #closing cluster
    raster::endCluster()

    return(output.list)

}
