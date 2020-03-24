#' Importing 4D or 5D raster data.
#'
#' @description Imports \strong{4D} (x, y, variable values, and variable names) or \strong{5D} (x, y, variable values, variable names, and times) raster data into an object of the class "environmental.data".
#'
#' @usage v_import_rasters(
#'  matched.rasters = NULL,
#'  input.folder = NULL,
#'  default.crs = "+init=epsg:4326",
#'  to.brick = FALSE
#'  )
#'
#' @param matched.rasters Object of the class "environmental.data" produced either with \code{\link{v_match_rasters}} (but also by \code{v_import_rasters}, see below).
#' @param input.folder Character string, path (without final slash) to the folder containing the raster files to be matched, or folder containing subfolders, each one containing raster files representing data from different times.
#' @param default.crs Default coordinate system for those files in \code{input.folder} that don't have one. The default value is \code{"+init=epsg:4326"}, as in \code{raster.template.crs}. Use this argument with care!
#' @param to.brick Boolean. Defaults to FALSE. If TRUE, the raster data is read into memory with \code{\link[raster]{brick}}. Otherwise the data is imported as a stack (the data resides in the hard disk) with \code{\link[raster]{stack}}.
#'
#' @details TODO
#'
#' @return Returns an object of the class "environmental.data" with a slot named "data".
#'
#' \itemize{
#' \item If \code{to.brick = FALSE} and input data is \code{4D}: the slot "data" contains a slot named "stack" with the raster.data.
#' \item If \code{to.brick = FALSE} and input data is \code{5D}: the slot "data" contains a slot named "stack" with slots named after the subfolders defined in \code{input.folder} or \code{matched.rasters}.
#' \item If \code{to.brick = TRUE} and input data is \code{4D}: the slot "data" contains a slot named "brick" with the raster.data. The slot "stack" is preserved.
#' \item If \code{to.brick = TRUE} and input data is \code{5D}: the slot "data" contains a slot named "brick" with slots named after the subfolders defined in \code{input.folder} or \code{matched.rasters}. The slot "stack" is preserved.
#' }
#'
#' @author Blas Benito <blasbenito@gmail.com>. The functions \code{\link[raster]{brick}} and \code{\link[raster]{stack}} are authored by Robert J. Hijmans.
#' @export
v_import_rasters <- function(
  matched.rasters = NULL,
  input.folder = NULL,
  default.crs = "+init=epsg:4326",
  to.brick = FALSE
){

  #if input is a folder
  #----------------------------------
  if(is.null(input.folder) == FALSE){

    #creates output object
    matched.rasters <- list()

    #managing input folder and subfolders
    #------------------------------------

    #gets subfolder names
    subfolders <- list.dirs(
      input.folder,
      full.names = FALSE
    )

    #if there are no subfolders
    if(length(subfolders) == 0 | length(subfolders) == 1){

      #only one input folder
      subfolders <- "none"

    } else {

      #gets all input folders but the first one
      subfolders <- subfolders[2:length(subfolders)]

    }


    #looping through subfolders
    #--------------------------------
    for(subfolder in subfolders){

      #getting file paths (this should get after the loop)
      #--------------------------------
      #if there are no subfolders
      if(subfolder == "none"){

        file.paths <- list.files(
          path = input.folder,
          full.names = TRUE
        )

      } else {

        file.paths <- list.files(
          path = paste(input.folder, "/", subfolder, sep = ""),
          full.names = TRUE
        )

      }

      #remove cases with a particular extension
      #remove .grd
      to.remove <- grep(
        pattern = ".grd",
        x = file.paths
      )

      if(length(to.remove) != 0){

        file.paths <- file.paths[-to.remove]

      }

      #creating report.df
      report.df <- data.frame(
        new.path = file.paths,
        stringsAsFactors = FALSE
      )

      if(subfolder != "none"){

        matched.rasters$meta[[subfolder]] <- report.df

      } else {

        matched.rasters$meta <- report.df
      }

    }#end of loop through subfolders

    #writing object class
    if(subfolder == "none"){

      class(matched.rasters) <- c("list", "environmental.data", "4D")

    } else {

      class(matched.rasters) <- c("list", "environmental.data", "5D")

    }

  }#end of if(dir.exists(input.folder) == TRUE)

  #now matched.rasters moves onto the next section to generate the stack or brick


  #if input is "environmental.data"
  #--------------------------------
  if(inherits(matched.rasters, "environmental.data") == TRUE){

    #4D option
    #-----------------------------
    if(inherits(matched.rasters, "4D") == TRUE){

      #only if it is not a stack already
      if(inherits(matched.rasters, "stack") == FALSE){

        #adding stack to matched rasters
        matched.rasters$data$stack <- raster::stack(
          x = matched.rasters$meta$new.path,
          quick = TRUE
        )

        #checking if the stack has a raster template
        if(is.na(raster::crs(matched.rasters$data$stack)) == TRUE){

          #assign raster.template.crs to it
          raster::crs(matched.rasters$data$stack) <- default.crs

        }

        #adds "stack" to class
        class(matched.rasters) <- c(class(matched.rasters), "stack")

        #removing attribute "brick"
        class(matched.rasters) <- class(matched.rasters)[!(class(matched.rasters) %in% "brick")]

      }#end of if(inherits(matched.rasters, "stack") == FALSE)

      #to brick
      if(to.brick == TRUE){

        #only if it is not a brick already
        if(inherits(matched.rasters, "brick") == FALSE){

          #transforming stack to brick
          matched.rasters$data$brick <- raster::brick(
            matched.rasters$data$stack
          )

          #removing attribute "stack"
          class(matched.rasters) <- class(matched.rasters)[!(class(matched.rasters) %in% "stack")]

          #adding brick attribute
          class(matched.rasters) <- c(class(matched.rasters), "brick")

        }

      #NULL otherwise
      } else {

        #setting brick to null (or removing it if it was in matched rasters)
        matched.rasters$data$brick <- NULL

        #garbage collection
        invisible(gc())

      }#end of to.brick

    }#end of 4D option


    #5D option
    #-------------------------------------------
    if(inherits(matched.rasters, "5D") == TRUE){

      #only if it is not a stack already
      if(inherits(matched.rasters, "stack") == FALSE){

        #gets names of subfolders
        subfolders <- names(matched.rasters$meta)

        #creates sublist
        matched.rasters$data$stack <- list()

        #iterating through subfolders to generate stacks
        for(subfolder in subfolders){

          #fills with stacks
          matched.rasters$data$stack[[subfolder]] <- raster::stack(
            x = matched.rasters$meta[[subfolder]]$new.path,
            quick = TRUE
          )

          #checking if the stack has a raster template
          if(is.na(raster::crs(matched.rasters$data$stack[[subfolder]])) == TRUE){

            #assign raster.template.crs to it
            raster::crs(matched.rasters$data$stack[[subfolder]]) <- default.crs

          }

        }#end of iteration through subfolders

        #adding stack attribute
        class(matched.rasters) <- c(class(matched.rasters), "stack")

        #remove brick attribute
        class(matched.rasters) <- class(matched.rasters)[!(class(matched.rasters) %in% "brick")]

      }#end of if(inherits(matched.rasters, "stack") == FALSE)

      #to brick
      if(to.brick == TRUE){

        #only if it is not a brick already
        if(inherits(matched.rasters, "brick") == FALSE){

          #creates sublist
          matched.rasters$data$brick <- list()

          #iterating through subfolders to generate stacks
          for(subfolder in subfolders){

            #fills with bricks
            matched.rasters$data$brick[[subfolder]] <- raster::brick(
              matched.rasters$data$stack[[subfolder]]
            )

          }

          #remove stack attribute
          class(matched.rasters) <- class(matched.rasters)[!(class(matched.rasters) %in% "stack")]

          #adding brick attribute
          class(matched.rasters) <- c(class(matched.rasters), "brick")

        }#end of if(inherits(matched.rasters, "brick") == FALSE)

      } else {

        #setting brick to null (or removing it if it was in matched rasters)
        matched.rasters$data$brick <- NULL

        #garbage collection
        invisible(gc())

      }#end of to.brick

    }#end of 5D option

  }#end of matched rasters

  return(matched.rasters)

}#end of function

