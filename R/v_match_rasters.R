#' @export
v_match_rasters <- function(
 raster.template = NULL,
 raster.template.crs = "+init=epsg:4326",
 input.folder,
 output.folder,
 default.crs = "+init=epsg:4326",
 n.cores = NULL,
 verbose = TRUE
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
    old.crs = NA,
    new.crs = NA,
    old.x.res = NA,
    new.x.res = NA,
    old.y.res = NA,
    new.y.res = NA,
    old.extent = NA,
    new.extent = NA,
    old.valid.cells = NA,
    new.valid.cells = NA,
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
  loop.out <- foreach::foreach(
    i = 1:nrow(report.df),
    .combine = 'c',
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

      #if crss are different
      if(raster::compareCRS(x = raster.i, y = raster.template) == FALSE){

        #reproject
        raster.i <- raster::projectRaster(
          from = raster.i,
          to = raster.template,
          method = "bilinear"
        )

      } else { #resample

        raster.i <- raster::resample(
          x = raster.i,
          y = raster.template,
          method = "bilinear"
        )

      }

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

    }

  #stopping cluster
  parallel::stopCluster(my.cluster)


  #preparing mask
  #--------------------

  #initializing mask
  mask <- raster.template

  #starting raster cluster
  raster::beginCluster(n = n.cores)

  #iterating through raster files
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
    system.time(mask <- calc(x, function(x){x * mask}))
    system.time(mask <- x * mask)

  }

  raster::endCluster()


}

