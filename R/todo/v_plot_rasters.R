#METHOD TO PLOT MATCHED RASTERS (implement this as a plot() method!)
plot = TRUE
breaks = 10
margins = c(0,0,2.5,1)  #margins: bottom, left, top, right

#plotting output
#-------------------
if(plot == TRUE){

  #saving user's par
  opar <- par()

  #setting plot window
  par(
    mfrow = c(nrow(report.df), 2),
    mar = margins,
    oma = c(3, 3, 1, 0.5)
  )

  #looping through rasters
  for(i in 1:nrow(report.df)){

    #get rasters
    raster.old <- raster::raster(x = report.df[i, "old.path"])
    raster.new <- raster::raster(x = report.df[i, "new.path"])

    #set names
    names(raster.old) <- paste(report.df[i, "name"], ".old", sep = "")
    names(raster.new) <- paste(report.df[i, "name"], ".new", sep = "")

    #set crs
    if(is.na(raster::crs(raster.old))){
      raster::crs(raster.old) <- report.df[i, "old.crs"]
    }
    if(is.na(raster::crs(raster.new))){
      raster::crs(raster.new) <- report.df[i, "new.crs"]
    }

    #getting break values from raster.old
    raster.old <- raster::setMinMax(raster.old)
    breaks.seq <- labeling::extended(
      min(raster::minValue(raster.old)),
      max(raster::maxValue(raster.old)),
      m = breaks
    )

    #plot
    plot(
      raster.old,
      main = names(raster.old),
      legend = FALSE,
      breaks = breaks.seq,
      col = viridis::viridis(breaks)
    )
    plot(raster.new,
         main = names(raster.new),
         legend = TRUE,
         breaks = breaks.seq,
         col = viridis::viridis(breaks)
    )

  }

  #restore par settings
  par(opar)

}
