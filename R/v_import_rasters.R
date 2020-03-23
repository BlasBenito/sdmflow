v_import_rasters <- function(
  matched.rasters = NULL,
  input.folder = NULL,
  to.brick = FALSE
){

  #if input is "environmental.data"
  #--------------------------------
  if(inherits(matched.rasters, "matched.rasters") == TRUE){

    #removing attribute "matched rasters"
    # class(matched.rasters) <- class(matched.rasters)[!(class(matched.rasters) %in% "matched.rasters")]

    #4D option
    if(inherits(matched.rasters, "4D") == TRUE){

      #adding stack to matched rasters
      matched.rasters$data$stack <- raster::stack(
        x = matched.rasters$meta$new.path,
        quick = TRUE
      )

      #adding stack attribute
      class(matched.rasters) <- c(class(matched.rasters), "stack")

      #to brick
      if(to.brick == TRUE){

        #transforming stack to brick
        matched.rasters$data$brick <- raster::brick(
          matched.rasters$data$stack
        )

        #removing attribute "stack"
        class(matched.rasters) <- class(matched.rasters)[!(class(matched.rasters) %in% "stack")]

        #adding stack attribute
        class(matched.rasters) <- c(class(matched.rasters), "brick")

      #NULL otherwise
      } else {

        matched.rasters$data$brick <- NULL

      }

    }

    #5D option
    if(inherits(matched.rasters, "5D") == TRUE){

      #TODO

    }

    return(matched.rasters)

  }#end of matched rasters


  #if input is a folder
  #----------------------------------
  #TODO



}
