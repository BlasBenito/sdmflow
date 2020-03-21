#' TODO: the function doesn't understand the differences between the units of latlong and utm crss
#' TODO: the function must crop all layers using as template the layer with the smaller extent, or the layer that fits within the extents of all the other layers
#' TODO: the function cannot handle rasters with two or more files (as in x.gri and x.grd, which is R's raster format). Checks and exceptions need to be created for those files!
#' TODO: the cluster preparation could go in a helper function, something like h_cluster_setup()
#' @export
v_match_rasters <- function(
 raster.template = NULL,
 raster.template.crs = "+init=epsg:4326",
 input.folder,
 output.folder,
 default.crs = "+init=epsg:4326",
 n.cores = NULL
){

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


  #managing output folder and subfolders
  #--------------------------------
  #creates default output.folder
  if(is.null(output.folder) == TRUE){

    output.folder <- "matched_rasters"
  }

  if(dir.exists(output.folder) == FALSE){

    dir.create(output.folder)

  }

  #if there is more than one subfolder
  if(length(subfolders) > 1){

    #creates each subfolder
    for(subfolder in subfolders){

      #creates subfolser
      dir.create(
        paste(
          output.folder,
          "/",
          subfolder,
          sep = ""
          )
        )

    }

  }


  #setting cluster type
  #---------------------
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
  #--------------------
  if(.Platform$OS.type == "windows"){

    my.cluster <- parallel::makeCluster(n.cores, type="PSOCK")

  } else {

    my.cluster <- parallel::makeCluster(n.cores, type="FORK")

  }

  #register cluster
  doParallel::registerDoParallel(my.cluster)


  #output list
  #--------------------
  output.list <- list()


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


    #generating report.df
    #--------------------------------
    report.df <- data.frame(
      name = basename(tools::file_path_sans_ext(file.paths)),
      old.path = file.paths,
      new.path = NA,
      stringsAsFactors = FALSE
    )


    #exporting cluster variables
    #----------------------------------
    parallel::clusterExport(cl = my.cluster,
                            varlist = c('raster.template',
                                        'report.df',
                                        'default.crs',
                                        'output.folder',
                                        'subfolder',
                                        'output.list'
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

        #first part of the resolution change factor
        old.res <- raster::res(raster.i)
        old.res.mean <- mean(old.res)

        #converts resolution to km if it is lonlat
        if(raster::isLonLat(raster.i) == TRUE){
          old.res.mean <- old.res.mean * 111
          old.res <- old.res * 111
        } else {
          #resolution from m to km in projected maps
          old.res.mean <- old.res.mean / 1000
          old.res <- old.res / 1000
        }

        #getting "old" data
        old.crs <- as.character(raster::crs(raster.i))
        old.extent <- paste(round(as.vector(raster::extent(raster.i)), 2), collapse = ", ")
        old.valid.cells <- length(na.omit(raster.i))

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

              #operation type
              operation <- "none"

          }#end of rasters are equal

          }#end of rasters have the same resolution

        }#end of crss are equal

        #naming raster
        names(raster.i) <- raster.i.name

        #building file.path
        if(subfolder == "none"){
          file.path <- paste(
            output.folder,
            "/",
            raster.i.name,
            ".rds",
            sep = ""
          )
        } else {
          file.path <- paste(
            output.folder,
            "/",
            subfolder,
            "/",
            raster.i.name,
            ".rds",
            sep = ""
          )
        }

        #saving rds
        saveRDS(
          object = raster.i,
          file = file.path,
          compress = FALSE
          )

        #getting "new" data
        new.crs <- as.character(raster::crs(raster.i))
        new.extent <- paste(round(as.vector(raster::extent(raster.i)), 3), collapse = ", ")
        new.valid.cells <- length(na.omit(raster.i))

        #converts resolution to km if it is lonlat
        if(raster::isLonLat(raster.i) == TRUE){

          new.res <- raster::res(raster.i) * 111

        } else {

          #resolution from m to km in projected maps
          new.res <- raster::res(raster.i) / 1000

        }

        #second part of the resolution change factor
        new.res.mean <- mean(new.res)

        #putting values together
        res.change.factor <- c(new.res.mean, old.res.mean)

        #computing fraction max / min
        res.change.factor <- max(res.change.factor)[1] / min(res.change.factor)[1]
        res.change.factor <- res.change.factor

        #changing sign if the new resolution is higher than the old
        #if new res is lower than old res, then the result is negative
        #if new res is higher than old res, then the result is positive
        if(new.res.mean > old.res.mean){
          res.change.factor <- - res.change.factor
        }

        #preparing data for report.df
        output.vector <- c(
          old.crs,
          new.crs,
          paste(round(old.res, 1), collapse = ", "),
          paste(round(new.res, 1), collapse = ", "),
          round(res.change.factor, 1),
          old.extent,
          NA,
          old.valid.cells,
          NA,
          operation
          )
        names(output.vector) <- c(
          "old.crs",
          "new.crs",
          "old.res",
          "new.res",
          "res.change.factor",
          "old.extent",
          "new.extent",
          "old.valid.cells",
          "new.valid.cells",
          "operation"
          )

        return(output.vector)

      }# end of parallelised loop

    #stopping cluster
    # parallel::stopCluster(my.cluster)

    #getting report together
    report.df <- cbind(
      report.df,
      report.df.temp,
      stringsAsFactors = FALSE
      )


    #preparing mask
    #--------------------

    #initializing mask
    mask <- raster.template

    #iterating through raster files
    #this loop propagates the null cells of all layers into a single mask object
    for(i in 1:nrow(report.df)){

      #getting file name
      raster.i.name <- report.df[i, "name"]

      #building file path
      if(subfolder == "none"){
        file.path <- paste(
          output.folder,
          "/",
          raster.i.name,
          ".rds",
          sep = ""
        )
      } else {
        file.path <- paste(
          output.folder,
          "/",
          subfolder,
          "/",
          raster.i.name,
          ".rds",
          sep = ""
        )
      }

      #reading RDS
      x <- readRDS(
        file = file.path
        )

      #computing mask
      mask <- x * mask #faster for small rasters

    }#end of loop


    #applying mask
    #---------------
    #register cluster
    doParallel::registerDoParallel(my.cluster)

    #exporting cluster variables
    parallel::clusterExport(cl = my.cluster,
                            varlist = c('mask',
                                        'report.df',
                                        'output.folder',
                                        'subfolder',
                                        'output.list'
                            ),
                            envir = environment()
    )

    #parallelised iterations through raster files
    output.df <- foreach::foreach(
      i = 1:nrow(report.df),
      .combine = 'rbind',
      .packages = "raster",
      .errorhandling = "pass"
    ) %dopar% {

      #getting file name
      raster.i.name <- report.df[i, "name"]

      #getting path (used twice)
      #building file path
      if(subfolder == "none"){
        file.path <- paste(
          output.folder,
          "/",
          raster.i.name,
          ".rds",
          sep = ""
        )
      } else {
        file.path <- paste(
          output.folder,
          "/",
          subfolder,
          "/",
          raster.i.name,
          ".rds",
          sep = ""
        )
      }

      #reading RDS
      raster.i <- readRDS(
        file = file.path
      )

      #applying mask
      raster.i <- raster::mask(
        x = raster.i,
        mask = mask
        )

      #trimming to remove NA borders
      raster.i <- trim(raster.i)

      #getting new path
      if(subfolder == "none"){
        output.raster.path <- paste(
          output.folder,
          "/",
          raster.i.name,
          sep = ""
        )
      } else {
        output.raster.path <- paste(
          output.folder,
          "/",
          subfolder,
          "/",
          raster.i.name,
          sep = ""
        )
      }


      #saving to raster format
      raster::writeRaster(
        x = raster.i,
        filename = output.raster.path,
        overwrite = TRUE
      )

      #removing RDS
      file.remove(file.path)

      #preparing output
      temp.df <- data.frame(
      new.extent = paste(
        round(as.vector(raster::extent(raster.i)), 3),
        collapse = ", "
        ),
      new.valid.cells = length(na.omit(raster.i)),
      new.path = paste(
        new.path,
        ".gri",
        sep = ""
      ),
      stringsAsFactors = FALSE
      )

      #loop output
      return(temp.df)

    }#end of parallelised loop


    #adding output.paths to report.df
    #---------------------------
    report.df$new.path <- output.df$new.pat
    report.df$new.extent <- output.df$new.extent
    report.df$new.valid.cells <- output.df$new.valid.cells
    rownames(report.df) <- NULL


    #saving partial result to output.list
    #------------------------
    if(subfolder != "none"){
      output.list[[subfolder]] <- report.df
    } else {
      output.list <- report.df
    }

  }#end of loop through subfolders

  #stopping cluster
  parallel::stopCluster(my.cluster)

  if(subfolder == "none"){
    class(output.list) <- c("data.frame", "matched.rasters.4D")
  } else {
    class(output.list) <- c("list", "matched.rasters.5D")
  }

  return(output.list)

}#end of function
