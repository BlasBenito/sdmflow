plotEnsamblado = function(media, desviacion, plot.presencias, presencias){

  #This code is derived from the one written by Tomislav Hengl (available here: http://spatial-analyst.net/wiki/index.php?title=Uncertainty_visualization). The main difference is that my version doesn't rely on a spatial dataframe, but on raster maps (library raster).

  #average: raster map representing the average of a few spatial models
  #deviation: raster map representing the standard deviation of a few spatial models
  #points = two columns in x - y order to plot point coordinates
  #name = name of the analysis
  #path, without the final slash

  #required libraries
  require(colorspace)
  require(plotrix)
  #require(SGDF2PCT)
  require(rgdal)

  #stacking the layers together
  ensemble <- stack(media, desviacion)
  names(ensemble) <- c("average", "deviation")

  #STRECH THE AVERAGE VALUES ONLY IF THE MAXIMUM VALUE OF THE AVERAGE IS HIGHER THAN 1
  #   if (max(as.vector(ensemble[["average"]]), na.rm=TRUE) > 1){
  ensemble[["average"]] <- setValues(ensemble[["average"]], plotrix::rescale(as.vector(ensemble[["average"]]), c(0,1)))
  #   }

  #STRECH THE VALUES OF THE NORMALIZED DEVIATION
  ensemble[["deviation"]] <- setValues(ensemble[["deviation"]], plotrix::rescale(as.vector(ensemble[["deviation"]]), c(0,1)))

  #DERIVE HUE
  H <- -90-as.vector(ensemble[["average"]])*300
  H <- ifelse(as.vector(H)<=-360, as.vector(H)+360, as.vector(H))
  H <- ifelse(as.vector(H)>=0, as.vector(H), (as.vector(H)+360))

  #DERIVE SATURATION
  S <- 1-as.vector(ensemble[["deviation"]])
  V <- 0.5*(1+as.vector(ensemble[["deviation"]]))

  #CONVERT TO RGB
  RGB <- as(HSV(H, S, V), "RGB")

  #CREATES THE RGB LAYERS
  R <- setValues(ensemble[["deviation"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,1]*255)))
  G <- setValues(ensemble[["deviation"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,2]*255)))
  B <- setValues(ensemble[["deviation"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,3]*255)))
  #stack
  RGB <- stack(R,G,B)
  names(RGB) <- c("R", "G", "B")

  #PLOTTING THE MAP
  # layout(matrix(c(1,1,1,1,1, 2,2,2, 1,1,1,1,1, 2,3,2, 1,1,1,1,1, 2,2,2), nrow = 3, ncol = 8, byrow = TRUE))
  layout(matrix(c(1,1,1,1,1, 2,1,1,1,1,1,3,1,1,1,1,1,2), nrow = 3, ncol = 6, byrow = TRUE))

  par(mar = c(2,2,2,2))
  plotRGB(RGB, 1, 2, 3)

  plot(0,type='n',axes=FALSE,ann=FALSE)

  #LEGEND (taken from Tomislav Hengl's code as it)
  #########
  legend.2D <- expand.grid(x=seq(.01,1,.01),y=seq(.01,1,.01))
  # Hues
  legend.2D$tmpf1 <- -90-legend.2D$y*300
  legend.2D$tmpf2 <- ifelse(legend.2D$tmpf1 <= -360, legend.2D$tmpf1 + 360, legend.2D$tmpf1)
  legend.2D$H <- ifelse(legend.2D$tmpf2 >= 0, legend.2D$tmpf2, (legend.2D$tmpf2 + 360))
  # Saturation
  legend.2D$S <- 1-legend.2D$x
  # Intensity
  legend.2D$V <- 0.5+legend.2D$x/2

  gridded(legend.2D) <- ~x+y
  legend.2D <- as(legend.2D, "SpatialGridDataFrame")
  spplot(legend.2D["H"], col.regions=rev(gray(0:20/20)))
  spplot(legend.2D["S"], col.regions=rev(gray(0:20/20)))
  spplot(legend.2D["V"], col.regions=rev(gray(0:20/20)))

  legendimg <- as(HSV(legend.2D$H, legend.2D$S, legend.2D$V), "RGB")
  #   plot(legendimg)
  legend.2D$red <- as.integer(legendimg@coords[,1]*255)
  legend.2D$green <- as.integer(legendimg@coords[,2]*255)
  legend.2D$blue <- as.integer(legendimg@coords[,3]*255)

  #Display as a RGB image:
  legend.2Dimg <- SGDF2PCT(legend.2D[c("red", "green", "blue")], ncolors=256, adjust.bands=FALSE)
  legend.2D$idx <- legend.2Dimg$idx

  #
  image(legend.2D, "idx", col=legend.2Dimg$ct, main="Legend")
  axis(side=2, at=c(0, 0.25, 0.50, 0.75, 1), line=0, lwd=2)
  axis(side=1, at=c(0, 0.25, 0.50, 0.75, 1), line=-3, lwd=2)
  mtext("Habitat suitability", side=2, line=3, cex=1.5)
  mtext("Standard deviation", side=1, line= 0, cex=1.5)

}
