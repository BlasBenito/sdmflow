#' TODO: the function doesn't understand the differences between the units of latlong and utm crss
#' TODO: the function must crop all layers using as template the layer with the smaller extent, or the layer that fits within the extents of all the other layers
#' TODO: the function cannot handle rasters with two or more files (as in x.gri and x.grd, which is R's raster format). Checks and exceptions need to be created for those files!
#' @export
v_match_rasters <- function(
 raster.template = NULL,
 raster.template.crs = "+init=epsg:4326",
 input.folder,
 output.folder,
 default.crs = "+init=epsg:4326",
 n.cores = NULL,
 to.brick = FALSE
){

  #managing output.folder
  if(is.null(output.folder) == TRUE){
    output.folder = "match_rasters_out"
  }
  if(dir.exists(output.folder) == FALSE){
    dir.create(output.folder)
  }

  #loading template and adding crs
  #--------------------------------

  #if it is a raster file
  if(inherits(raster.template, "RasterLayer") == TRUE){

    #checking the layer has a raster template
    if(is.na(raster::crs(raster.template)) == TRUE){

      #assign raster.template.crs to it
      raster::crs(raster.template) <- raster.template.crs

    } else {

      raster.template.crs <- raster::crs(raster.template)
    }

  } else {

    if(is.character(raster.template) == TRUE){

      #check if it's a path
      if(file.exists(raster.template) == TRUE){

        #imports raster
        raster.template <- raster::raster(raster.template)

        #checking the layer has a raster template
        if(is.na(raster::crs(raster.template)) == TRUE){

          #assign raster.template.crs to it
          raster::crs(raster.template) <- raster.template.crs

        } else {

          raster.template.crs <- raster::crs(raster.template)

        }
      }
    } else {

      stop("raster.template is not a RasterLayer object or a path to a raster file..")

    }
  }

  #generating report.df
  #--------------------------------
  report.df <- data.frame(
    name = tools::file_path_sans_ext(
      list.files(path = input.folder)
      ),
    old.path = list.files(
      path = input.folder,
      full.names = TRUE
    ),
    new.path = NA,
    stringsAsFactors = FALSE
  )

  #preparing parallelized iterations
  #----------------------------------

  #setting number of cores and iterator
  if(is.null(n.cores) == TRUE){
    n.cores <- parallel::detectCores() - 1
  } else {
    if(n.cores > 1){ #several cores
      `%dopar%` <- foreach::`%dopar%`
    } else { #only one core
      `%dopar%` <- foreach::`%do%`
      on.exit(`%dopar%` <- foreach::`%dopar%`)
    }
  }

  #selecting forking method by platform
  if(.Platform$OS.type == "windows"){
    my.cluster <- parallel::makeCluster(n.cores, type="PSOCK")
  } else {
    my.cluster <- parallel::makeCluster(n.cores, type="FORK")
  }

  #register cluster
  doParallel::registerDoParallel(my.cluster)

  #exporting cluster variables
  parallel::clusterExport(cl = my.cluster,
                          varlist = c('raster.template',
                                      'report.df',
                                      'default.crs',
                                      'output.folder'
                                      ),
                          envir = environment()
                          )

  #parallelised loop
  #-------------------------
  report.df.temp <- foreach::foreach(
    i = 1:nrow(report.df),
    .combine = 'rbind',
    .packages = "raster",
    .errorhandling = "pass"
    ) %dopar% {

      #getting raster.i
      raster.i <- raster::raster(report.df[i, "old.path"])
      raster.i.name <- report.df[i, "name"]

      #getting crs
      raster.i.crs <- raster::crs(raster.i)
      if(is.na(raster.i.crs) == TRUE){
        raster::crs(raster.i) <- default.crs
      }

      #getting "old" data
      old.crs <- as.character(raster::crs(raster.i))
      old.res <- paste(raster::res(raster.i), collapse = ", ")
      old.extent <- paste(as.vector(raster::extent(raster.i)), collapse = ", ")
      old.valid.cells <- length(na.omit(raster.i))

      #first part of the resolution change factor
      resolution.old <- mean(raster::res(raster.i))

      #converts resolution to km if it is lonlat
      if(raster::isLonLat(raster.i) == TRUE){
        resolution.old <- resolution.old * 111
      }

      #if crss are different
      if(raster::compareCRS(x = raster.i, y = raster.template) == FALSE){

        #reproject
        raster.i <- raster::projectRaster(
          from = raster.i,
          to = raster.template,
          method = "bilinear"
        )

        #operation type
        operation <- "raster::projectRaster()"

      } else {
      #crss are equal

        #rasters don't have the same resolution
        if(sum(raster::res(raster.i) == raster::res(raster.template)) != 2){

          #resampling raster.i to the resolution of raster.template
          raster.i <- raster::resample(
            x = raster.i,
            y = raster.template,
            method = "bilinear"
          )

          #operation type
          operation <- "raster::resample()"

        } else {
        #rasters have the same resolution

          #the extents are different
          if((raster::extent(raster.i) == raster::extent(raster.template)) == FALSE){

            #cropping raster
            raster.i <- raster::crop(
              x = raster.i,
              y = raster.template,
              snap = "near"
            )

          #operation type
          operation <- "raster::crop()"

          } else {
          #rasters are equal

            operation <- "none"

        }#end of rasters are equal

        }#end of rasters have the same resolution

      }#end of crss are equal

      #naming raster
      names(raster.i) <- raster.i.name

      #saving
      saveRDS(
        object = raster.i,
        file = paste(
          output.folder,
          "/",
          raster.i.name,
          ".rds",
          sep = ""
          ),
        compress = FALSE
        )

      #getting "new" data
      new.crs <- as.character(raster::crs(raster.i))
      new.res <- paste(raster::res(raster.i), collapse = ", ")
      new.extent <- paste(as.vector(raster::extent(raster.i)), collapse = ", ")
      new.valid.cells <- length(na.omit(raster.i))

      #second part of the resolution change factor
      resolution.new <- mean(raster::res(raster.i))

      #resolution to km
      if(raster::isLonLat(raster.i) == TRUE){
        resolution.new <- resolution.new * 111
      }

      #putting values together
      resolution.change.factor <- c(resolution.new, resolution.old)

      #computing fraction max / min
      resolution.change.factor <- max(resolution.change.factor)[1] / min(resolution.change.factor)[1]

      #changing sign if the new resolution is higher than the old
      #if new res is lower than old res, then the result is negative
      #if new res is higher than old res, then the result is positive
      if(resolution.new > resolution.old){
        resolution.change.factor <- - resolution.change.factor
      }

      #preparing data for report.df
      output.vector <- c(old.crs, new.crs, old.res, new.res, resolution.change.factor, old.extent, new.extent, old.valid.cells, new.valid.cells, operation)
      names(output.vector) <- c("old.crs", "new.crs", "old.res", "new.res", "resolution.change.factor", "old.extent", "new.extent", "old.valid.cells", "new.valid.cells", "operation")

      return(output.vector)

    }# end of parallelised loop

  #stopping cluster
  parallel::stopCluster(my.cluster)

  #getting report together
  report.df <- cbind(report.df, report.df.temp)


  #preparing mask
  #--------------------

  #initializing mask
  mask <- raster.template

  #function to propagate nulls
  propagate.nulls <- function(x){x * mask}

  #starting raster cluster
  #selecting forking method by platform
  if(.Platform$OS.type == "windows"){
    cluster.type <- "PSOCK"
  } else {
    cluster.type <- "FORK"
  }
  raster::beginCluster(
    n = n.cores,
    type = cluster.type
    )


  #iterating through raster files
  #this loop propagates the null cells of all layers into a single mask object
  for(i in 1:nrow(report.df)){

    #getting file name
    raster.i.name <- report.df[i, "name"]

    #reading RDS
    x <- readRDS(
      file = paste(
        output.folder,
        "/",
        raster.i.name,
        ".rds",
        sep = ""
        )
      )

    #computing mask
    mask <- raster::clusterR(x, propagate.nulls)
    #mask <- x * mask #faster for small rasters

  }#end of loop


  #applying mask
  #---------------

  #iterating through raster files
  #this loop propagates the null cells of all layers into a single mask object
  for(i in 1:nrow(report.df)){

    #getting file name
    raster.i.name <- report.df[i, "name"]

    #getting path (used twice)
    raster.i.path <- paste(
      output.folder,
      "/",
      raster.i.name,
      ".rds",
      sep = ""
    )

    #reading RDS
    x <- readRDS(
      file = raster.i.path
    )

    #removing RDS
    # file.remove(raster.i.path)

    #applying mask
    x <- raster::mask(
      x = x,
      mask = mask
      )

    #getting new path
    new.path <- paste(
      output.folder,
      "/",
      raster.i.name,
      sep = ""
    )

    #saving new path in report
    report.df[i, "new.path"] <- new.path

    #saving to raster format
    raster::writeRaster(
      x = x,
      filename = new.path,
      overwrite = TRUE
    )

  }#end of loop

  #stopping raster cluster
  raster::endCluster()

  #generating raster stack
  output.raster <- raster::stack(report.df$new.path)

  #to brick
  if(to.brick == TRUE){
    output.raster <- raster::brick(output.raster)
  }

  #preparing output list
  output.list <- list()
  class(output.list) <- "sdmflow.environment"
  output.list$metadata$v_match_rasters <- report.df
  output.list$data <- output.raster

  return(output.list)

}#end of function


