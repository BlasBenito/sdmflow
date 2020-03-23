


#THIS VERSION INCLUDES MODEL COMPLEXITY AND COMPOSITION (VARIABLE NAMES) OF EACH MODEL
LeaveOneOut=function(data, models, pseudoabsences, name, exclusion.radius){
  
  #PARAMETERS
  #   data=modeling.data.reliable
  #   models=model definitions written like formulas (BUT NOT AS FORMULAS!)
  #   pseudoabsences=pseudo.absence.variables
  #   name="nada"exclusion.radius
  #   path="/home/blas/Dropbox/BIOCULTURAL_HISTORY/NEANDERTHAL/EEMIAN/niche_modeling/5_modeling_results/glm/background_sensitivity/2_single_factors"
  #   exclude.radius radius in degrees used to remove data surrounding the excluded presence. It excludes both background and presence
  
  #required libraries
  require(formula.tools)
  require(ggplot2)
  #   require(grid)
  require(Hmisc)
  library(stringr)
  
  #counts total number of iterations to perform
  total.iterations=nrow(data[data$presence==1, ])*length(models)
  
 #extract unique elements
  modeling.variables=unique(ParsePolynomialFormula(models))
  
  #table to store the results
  results=data.frame(model=character(), excluded_presence=numeric(), deviance=numeric(), explained_deviance=numeric(), suitability_presence=numeric(), AUC=numeric(), name=character(), n_variables=numeric(), stringsAsFactors=FALSE)
  
  #creating a dataframe from the names of the modeling variables
  results2=data.frame(matrix(1:length(modeling.variables), 1))
  names(results2)=modeling.variables
  
  #joining dataframes
  results=cbind(results, results2[0, ])
  
  #identify the indexes of the presence records in the modeling.data.5km table
  presence.data.indexes=data[data$presence==1, ]
  
  #counter for the results row
  results.row=1
  
  #iterates through candidate models
  for (selected.model in 1:length(models)){
    
    #formula
    formula=as.formula(models[[selected.model]])
    
    #iterates through each presence record
    for (excluded.record in 1:nrow(presence.data.indexes)){
      
      #table without the excluded record
      data.subset=data[-excluded.record, ]
      
      #removing background (and presences!, I think it is better this way) around the excluded record (FIXED TO 100km)
      #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
      f<-data.subset[excluded.record, ]
      
      #genera los límites de la cuadrícula de búsqueda
      ymax<-f$y + exclusion.radius
      ymin<-f$y - exclusion.radius
      xmax<-f$x + exclusion.radius
      xmin<-f$x - exclusion.radius
      
      #data selection
      data.subset=data.subset[!((data.subset$y <= ymax) & (data.subset$y >= ymin) & (data.subset$x <= xmax) & (data.subset$x >= xmin) & (data.subset$y != f$y | data.subset$x != f$x)), ]
      
      #weighting the presence and background data
      n.presencias=nrow(data.subset[data.subset$presence==1, ])
      
      #¿cuantos puntos de background hay?
      n.background=nrow(data.subset[data.subset$presence==0, ])
      
      #introduce weights
      data.subset$weight.values=c(rep(1/n.presencias, n.presencias), rep(1/n.background, n.background))
      
      #fits a GLM
      glm.temp=glm(formula, family=quasibinomial(link=logit), data=data.subset, weights=weight.values)
      
      #measuring values to compute explained deviance
      d2=(glm.temp$null.deviance - glm.temp$deviance) / glm.temp$null.deviance
      n=length(glm.temp$fitted.values)
      p=length(glm.temp$coefficients)
      explained.deviance=1-((n-1)/(n-p))*(1-d2)
      
      #predicts the suitability for the excluded presence
      suitability.excluded.presence=predict(glm.temp, newdata=data[excluded.record, ], type="response")
      
      #predicts the suitability of the pseudo-absences
      suitability.pseudoabsences=predict(glm.temp, newdata=pseudoabsences, type="response")
      
      #WRITING RESULTS
      #model definition
#       results[results.row, "model"]=as.character.formula(formula[[3]])
      results[results.row, "model"]=as.character(formula)
      #excluded presence
      results[results.row, "excluded_presence"]=excluded.record
      #deviance
      results[results.row, "deviance"]=round(deviance(glm.temp), 4)
      #deviance
      results[results.row, "explained_deviance"]=round(explained.deviance, 4)
      #suitability presence
      results[results.row, "suitability_presence"]=round(suitability.excluded.presence, 2)
      #measure of commision error (proportion of pseudoabsences with suitabiliy values higher than the excluded presence)
      results[results.row, "AUC"]=round(length(which(suitability.pseudoabsences < suitability.excluded.presence)) / nrow(pseudoabsences), 4)
      results[results.row, "name"]=name
      #number of parameters
      results[results.row, "n_variables"]=(p-1)/2 #counting variables instead of polynomial terms
      
      #filling the variables with zeros
      results[results.row, modeling.variables]=0
      #filling the fields of the used variables with ones
      results[results.row, ParsePolynomialFormula(formula)]=1
      
      #number of iterations remaining
      print(paste(results.row, " ouf of ", total.iterations, " iterations performed", sep=""))
      
      #sums 1 to the results row
      results.row=results.row + 1
      
    }#end of iteration through records
  }#end of iteration through models
  
  #plot
  pdf(paste(name, ".pdf", sep=""), width=30, height=length(models)+5, pointsize=30)
  
  #explained deviance
  bp=ggplot(data=results, aes(x=reorder(factor(model), explained_deviance, FUN=median, order=TRUE), y=explained_deviance), environment=environment()) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(notch=FALSE, color="gray20") +
    stat_summary(fun.data='mean_cl_boot', geom="linerange", colour="tomato", size = 5) +
    coord_flip() +
    scale_y_continuous(limits = quantile(results$explained_deviance, c(0.05, 0.95))) +
    theme(axis.text=element_text(size=28), 
          axis.title=element_text(size=32), 
          title=element_text(size=36, face="bold"),
          legend.title=element_text(size=32, face="bold"), 
          legend.text=element_text(size=18), 
          plot.margin=unit(c(2,2,2,2), "cm")) +
    xlab("Model") +
    ylab("Explained deviance") +
    labs(title=paste("Explained deviance (", name, ")", sep="" ) )
  print(bp)
  
  #AUC
  bp=ggplot(data=results, aes(x=reorder(factor(model), AUC, FUN=median, order=TRUE), y=AUC), environment=environment()) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(notch=FALSE, color="gray20") +
    stat_summary(fun.data='mean_cl_boot', geom="linerange", colour="tomato", size = 5) +
    coord_flip() +
    scale_y_continuous(limits = quantile(results$AUC, c(0.05, 0.95))) +
    theme(axis.text=element_text(size=28), 
          axis.title=element_text(size=32), 
          title=element_text(size=36, face="bold"),
          legend.title=element_text(size=32, face="bold"), 
          legend.text=element_text(size=18), 
          plot.margin=unit(c(2,2,2,2), "cm")) +
    xlab("Model") +
    ylab("AUC") +
    labs(title=paste("AUC (", name, ")", sep="") )
  print(bp)
  
  #suitability presences
  bp=ggplot(data=results, aes(x=reorder(factor(model), suitability_presence, FUN=median, order=TRUE), y=suitability_presence), environment=environment()) +
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot(notch=FALSE, color="gray20") +
    stat_summary(fun.data='mean_cl_boot', geom="linerange", colour="tomato", size = 5) +
    coord_flip() +
    scale_y_continuous(limits = quantile(results$suitability_presence, c(0.05, 0.95))) +
    theme(axis.text=element_text(size=28), 
          axis.title=element_text(size=32), 
          title=element_text(size=36, face="bold"),
          legend.title=element_text(size=32, face="bold"), 
          legend.text=element_text(size=18), 
          plot.margin=unit(c(2,2,2,2), "cm")) +
    xlab("Model") +
    ylab("Habitat suitability of presences") +
    labs(title=paste("Habitat suitability of presences (", name, ")", sep="" ) )
  print(bp)
  
  dev.off()
  
  #SAVE TABLE
  #write.table(results, file=paste(name, ".csv", sep=""), row.names=TRUE, col.names=TRUE, quote=TRUE)
  
  return(results)
  
}



#############################################################################
#ReduceSpatialClustering
#This function reduces the spatial clustering of a set of presence records. It is intended to reduce spatial autocorrelation, and reduce sampling bias, specially at larger geographical scales.

#It requires two different arguments:
#data.table: a table with two fields representing latitude (named 'y') and longitude (named 'x')

#a minimum.distance value, provided in the same units as the coordinates, that will define the search radius when looking for pairs of coordinates within search distance to get rid of. Hint: the minimum distance can be extracted from the resolution of a raster containint the environmental factors, like "minimum.distance<-xres(v.brick.20km)"
ReduceSpatialClustering = function(data, minimum.distance){

#count rows
row<-1


#repite la operación hasta que se cumple la condición de salida
repeat{
  
  #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
  f<-data[row, ]
  
  #genera los límites de la cuadrícula de búsqueda
  ymax<-f$y + minimum.distance
  ymin<-f$y - minimum.distance
  xmax<-f$x + minimum.distance
  xmin<-f$x - minimum.distance
  
  #selecciona de la tabla los datos con coordenadas dentro del rectángulo que no tienen las mismas coordenadas que la fila con la que estamos trabajando, y las elimina de la tabla
  data<-data[!((data$y <= ymax) & (data$y >= ymin) & (data$x <= xmax) & (data$x >= xmin) & (data$y != f$y | data$x != f$x)), ]
  
  #estima de filas por procesar
  print(paste("Processed rows: ", row, " out of ", nrow(data), sep=""))
  
  #suma 1 al contador de la fila
  row<-row+1
  
  #condición de salida cuando llega a la última fila
  if(row>=nrow(data))break
}

return(data)

}



#REMOVE PSEUDO ABSENCES AROUND PRESENCES
RemovePseudoabsencesAroundPresences = function(presences, pseudoabsences, minimum.distance){
  
  #count rows
  row<-1
  
  
  #repite la operación hasta que se cumple la condición de salida
  repeat{
    
    #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
    f<-presences[row, ]
    
    #genera los límites de la cuadrícula de búsqueda
    ymax<-f$y + minimum.distance
    ymin<-f$y - minimum.distance
    xmax<-f$x + minimum.distance
    xmin<-f$x - minimum.distance
    
    #selecciona de la tabla los datos con coordenadas dentro del rectángulo que no tienen las mismas coordenadas que la fila con la que estamos trabajando, y las elimina de la tabla
    pseudoabsences=pseudoabsences[!((pseudoabsences$y <= ymax) & (pseudoabsences$y >= ymin) & (pseudoabsences$x <= xmax) & (pseudoabsences$x >= xmin)), ]
    
    #estima de filas por procesar
    print(paste("Processed rows: ", row, " out of ", nrow(presences), sep=""))
    
    #suma 1 al contador de la fila
    row<-row+1
    
    #condición de salida cuando llega a la última fila
    if(row>=nrow(presences))break
  }
  
  return(pseudoabsences)
  
}


#############################################################################
#WHITENING
WhiteningEnsemble = function(average, deviation, name, plot.points, points){
  
  #This code is derived from the one written by Tomislav Hengl (available here: http://spatial-analyst.net/wiki/index.php?title=Uncertainty_visualization). The main difference is that my version doesn't rely on a spatial dataframe, but on raster maps (library raster).
  
  #average: raster map representing the average of a few spatial models
  #deviation: raster map representing the standard deviation of a few spatial models
  #points = two columns in x - y order to plot point coordinates
  #name = name of the analysis
  #path, without the final slash
  
  #required libraries
  require(colorspace)
  require(plotrix)
#   require(SGDF2PCT)
  require(rgdal)
  
  #stacking the layers together
  ensemble=stack(average, deviation)
  names(ensemble)=c("average", "deviation")
  
  #STRECH THE AVERAGE VALUES ONLY IF THE MAXIMUM VALUE OF THE AVERAGE IS HIGHER THAN 1
#   if (max(as.vector(ensemble[["average"]]), na.rm=TRUE) > 1){
    ensemble[["average"]]=setValues(ensemble[["average"]], plotrix::rescale(as.vector(ensemble[["average"]]), c(0,1)))
#   }
  
  #STRECH THE VALUES OF THE NORMALIZED DEVIATION
  ensemble[["deviation"]]=setValues(ensemble[["deviation"]], plotrix::rescale(as.vector(ensemble[["deviation"]]), c(0,1)))
  
  #DERIVE HUE
  H=-90-as.vector(ensemble[["average"]])*300
  H=ifelse(as.vector(H)<=-360, as.vector(H)+360, as.vector(H))
  H=ifelse(as.vector(H)>=0, as.vector(H), (as.vector(H)+360))
  
  #DERIVE SATURATION
  S=1-as.vector(ensemble[["deviation"]])
  V=0.5*(1+as.vector(ensemble[["deviation"]]))
  
  #CONVERT TO RGB
  RGB <- as(HSV(H, S, V), "RGB")
  
  #CREATES THE RGB LAYERS
  R=setValues(ensemble[["deviation"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,1]*255)))
 G=setValues(ensemble[["deviation"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,2]*255)))
  B=setValues(ensemble[["deviation"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,3]*255)))
  #stack
  RGB=stack(R,G,B)
  names(RGB)=c("R", "G", "B")
  
  #PLOTTING THE MAP
  png(paste(name, "_ensemble.png", sep=""), width=ncol(RGB), height=nrow(RGB), pointsize=40)
  plotRGB(RGB, 1, 2, 3, axes=FALSE)
  if (plot.points==TRUE){points(points[, 1], points[, 2], lwd=3, cex=0.6)}
  title(paste(name, sep=""), cex.main=2)
  dev.off()
  
  #WRITES MAP TO DISK AS A MULTIBAND GEOTIFF
  writeRaster(RGB, filename=paste(name, "_ensemble.tif", sep=""), format="GTiff", options="INTERLEAVE=BAND", overwrite=TRUE)

  #LEGEND (taken from Tomislav Hengl's code as it)
  #########
  legend.2D <- expand.grid(x=seq(.01,1,.01),y=seq(.01,1,.01))
  # Hues
  legend.2D$tmpf1 <- -90-legend.2D$y*300
  legend.2D$tmpf2 <- ifelse(legend.2D$tmpf1<=-360, legend.2D$tmpf1+360, legend.2D$tmpf1)
  legend.2D$H <- ifelse(legend.2D$tmpf2>=0, legend.2D$tmpf2, (legend.2D$tmpf2+360))
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
  
  # Display as a RGB image:
  legend.2Dimg <- SGDF2PCT(legend.2D[c("red", "green", "blue")], ncolors=256, adjust.bands=FALSE)
  legend.2D$idx <- legend.2Dimg$idx
  
  #PLOTTING THE LEGEND
  png(paste(name, "_legend.png", sep=""), width=1000, height=1000, pointsize=35)
  image(legend.2D, "idx", col=legend.2Dimg$ct, main="Legend")
  axis(side=2, at=c(0, 0.25, 0.50, 0.75, 1), line=-1.5, lwd=2)
  axis(side=1, at=c(0, 0.25, 0.50, 0.75, 1), line=0, lwd=2)
  mtext("Habitat suitability", side=2, line=1.5, cex=1.5)
  mtext("Standard deviation", side=1, line=3, cex=1.5)
  title("Legend", cex.main=2)
  dev.off()
  
}


#############################################################################
#WHITENING
WhiteningVariableImportance = function(average, importance, name, points){
  
  #This code is derived from the one written by Tomislav Hengl (available here: http://spatial-analyst.net/wiki/index.php?title=Uncertainty_visualization). The main difference is that my version doesn't rely on a spatial dataframe, but on raster maps (library raster).
  
  #average: raster map representing the average of a few spatial models
  #deviation: raster map representing the standard deviation of a few spatial models
  #points = two columns in x - y order to plot point coordinates
  #name = name of the analysis
  #path, without the final slash
  
  #required libraries
  require(colorspace)
  require(plotrix)
  
  #changing the scale of the importance layer
  importance=abs(1-importance)
  
  #stacking the layers together
  ensemble=stack(average, importance)
  names(ensemble)=c("average", "importance")
  
  #STRECH THE AVERAGE VALUES ONLY IF THE MAXIMUM VALUE OF THE AVERAGE IS HIGHER THAN 1
  #   if (max(as.vector(ensemble[["average"]]), na.rm=TRUE) > 1){
  ensemble[["average"]]=setValues(ensemble[["average"]], rescale(as.vector(ensemble[["average"]]), c(0,1)))
  #   }
  
  #STRECH THE VALUES OF THE NORMALIZED DEVIATION
#   ensemble[["importance"]]=setValues(ensemble[["importance"]], rescale(as.vector(ensemble[["importance"]]), c(0,1)))
  
  #DERIVE HUE
  H=-90-as.vector(ensemble[["average"]])*300
  H=ifelse(as.vector(H)<=-360, as.vector(H)+360, as.vector(H))
  H=ifelse(as.vector(H)>=0, as.vector(H), (as.vector(H)+360))
  
  #DERIVE SATURATION
  S=1-as.vector(ensemble[["importance"]])
  V=0.5*(1+as.vector(ensemble[["importance"]]))
  
  #CONVERT TO RGB
  RGB <- as(HSV(H, S, V), "RGB")
  
  #CREATES THE RGB LAYERS
  R=setValues(ensemble[["importance"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,1]*255)))
  G=setValues(ensemble[["importance"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,2]*255)))
  B=setValues(ensemble[["importance"]], as.integer(ifelse(is.na(as.vector(ensemble[["average"]])), 255, RGB@coords[,3]*255)))
  #stack
  RGB=stack(R,G,B)
  names(RGB)=c("R", "G", "B")
  
  #PLOTTING THE MAP
  png(paste(name, "_importance.png", sep=""), width=ncol(RGB), height=nrow(RGB), pointsize=40)
  plotRGB(RGB, 1, 2, 3, axes=FALSE)
  points(points[, 1], points[, 2], lwd=4)
  title(paste(name, sep=""), cex.main=2)
  dev.off()
  
  #WRITES MAP TO DISK AS A MULTIBAND GEOTIFF
#   writeRaster(RGB, filename=paste(paste(path, name, sep="/"), "_importance.tif", sep=""), format="GTiff", options="INTERLEAVE=BAND", overwrite=TRUE)
  
  #LEGEND (taken from Tomislav Hengl's code as it)
  #########
  legend.2D <- expand.grid(x=seq(.01,1,.01),y=seq(.01,1,.01))
  # Hues
  legend.2D$tmpf1 <- -90-legend.2D$y*300
  legend.2D$tmpf2 <- ifelse(legend.2D$tmpf1<=-360, legend.2D$tmpf1+360, legend.2D$tmpf1)
  legend.2D$H <- ifelse(legend.2D$tmpf2>=0, legend.2D$tmpf2, (legend.2D$tmpf2+360))
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
  
  # Display as a RGB image:
  legend.2Dimg <- SGDF2PCT(legend.2D[c("red", "green", "blue")], ncolors=256, adjust.bands=FALSE)
  legend.2D$idx <- legend.2Dimg$idx
  
  #PLOTTING THE LEGEND
  png(paste(name, "_legend.png", sep=""), width=500, height=500, pointsize=16)
  image(legend.2D, "idx", col=legend.2Dimg$ct, main="Legend")
  axis(side=2, at=c(0, 0.25, 0.50, 0.75, 1), line=-1.5, lwd=2)
  axis(side=1, at=c(0, 0.25, 0.50, 0.75, 1), labels=rev(c(0, 0.25, 0.50, 0.75, 1)), line=0, lwd=2)
  mtext("Habitat suitability", side=2, line=1.5, cex=1.5)
  mtext("Variable importance", side=1, line=3, cex=1.5)
  title("Legend", cex.main=2)
  dev.off()
  
}

#############################################################################
#WEIGHT PRESENCE/BACKGROUND DATA
WeightPresenceBackground=function(presence.column){
  
  #computing weight for presences
  n.presences=sum(presence.column)
  print(paste("Presence points = ", n.presences, sep=""))
  weight.presences=1/n.presences
  print(paste("Weight for presences = ", weight.presences, sep=""))
  
  n.background=length(presence.column)-n.presences
  print(paste("Background points = ", n.background, sep=""))
  weight.background=1/n.background
  print(paste("Weight for background = ", weight.background, sep=""))
  
  #generamos un vector con los los pesos
  weights<-c(rep(weight.presences, n.presences), rep(weight.background, n.background))
  return(weights)
}


#############################################################################
#http://modtools.wordpress.com/2013/08/14/dsquared/ 
# Linear models come with an R-squared value that measures the proportion of variation that the model accounts for. The R-squared is provided with summary(model) in R. For generalized linear models (GLMs), the equivalent is the amount of deviance accounted for (D-squared; Guisan & Zimmermann 2000), but this value is not normally provided with the model summary. The Dsquared function, now included in the modEvA package (Barbosa et al. 2014), calculates it. There is also an option to calculate the adjusted D-squared, which takes into account the number of observations and the number of predictors, thus allowing direct comparison among different models (Weisberg 1980, Guisan & Zimmermann 2000).
Dsquared <- function(model, adjust = TRUE) {
  # version 1.1 (13 Aug 2013)
  # calculates the explained deviance of a GLM
  # model: a model object of class "glm"
  # adjust: logical, whether or not to use the adjusted deviance taking into acount the nr of observations and parameters (Weisberg 1980; Guisan & Zimmermann 2000)
  d2 <- (model$null.deviance - model$deviance) / model$null.deviance
  if (adjust) {
    n <- length(model$fitted.values)
    p <- length(model$coefficients)
    d2 <- 1 - ((n - 1) / (n - p)) * (1 - d2)
  }
  return(d2)
}  # end Dsquared function


#############################################################################
#AGGREGATE DATA TO PLOT SENSITIVITY
#http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE=function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  # This does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


##############################################################################
#CALIBRATE MODEL
CalibrateSDM=function(model.definition, data, variables, run.name, summary, Rdata, plot.curves, plot.map, gis.map, return.model){
  
#ARGUMENTS
#   model=formula as character
#   data=data frame
#   variables=raster brick
#   path=without last slash
#   run.name=character string
#   summary=TRUE/FALSE
#   Rdata=TRUE/FALSE
#   plot.curves=TRUE/FALSE
#   plot.map=TRUE/FALSE
#   gis.map=TRUE/FALSE
  
#    browser()
  
  #required libraries
  require(plotmo)
  require(RColorBrewer)
#   require(dismo)
  
  #file path
#   file.path=paste(path, run.name, sep="/")
  
  #model formula
  model.to.fit=as.formula(model.definition)

  #weights temp data
  data$weight.values=WeightPresenceBackground(presence.column=data$presence)
  
  #model
  print("Fitting model")
  m.glm=glm(model.to.fit, family=quasibinomial(link=logit), data=data, weights=weight.values)
  
  #summary
  if (summary==TRUE){
  print("Writing summary")
  sink(file=paste(run.name, ".txt", sep=""))
  print(summary(m.glm))
  sink()
  }
  
  #Rdata
  if (Rdata==TRUE){
  print("Saving .Rdata file")
  save(m.glm, file=paste(run.name, ".Rdata", sep=""))
  }
  
  
  #plot curves
  if (plot.curves==TRUE){
  print("Plotting response curves")
  png(paste(run.name, "_curves.png", sep=""), width=2000, height=2000, pointsize=35)
  plotmo(object=m.glm, type="response", all2=TRUE, se=2, caption=paste(run.name, sep=""))
  dev.off()
  }
  
  #map
  if (plot.map==TRUE){
  print("Plotting map")
  m.glm.map=predict(variables, m.glm, type="response")
  breakpoints=seq(0, 1, 0.1)
  colors=rev(colorRampPalette(brewer.pal(9,"RdYlBu"))(10))
  png(paste(run.name, "_map.png", sep=""), width=ncol(variables)+200, height=nrow(variables)+200, pointsize=25)
  plot(m.glm.map, main=paste(run.name), col=colors, breaks=breakpoints) 
  points(data[data$presence==1 , "x"], data[data$presence==1 , "y"])
  dev.off()
  } 
  
  #gis map
  if (plot.map==TRUE & gis.map==TRUE){
  print("Writing GIS map")
  writeRaster(m.glm.map, filename=run.name, format="raster", overwrite=TRUE)
  }
#returns the model object
m.glm
}


##############################################################################
#PARSE POLYNOMIAL FORMULA
ParsePolynomialFormula=function(formula){
  
  #retrieving the unique factors inside "models" to obtain the new columns to create
  temp1=as.character(formula)
  
  #remove characters we don't need
  for(model in 1:length(temp1)){
    temp1[model] = str_replace_all(string=temp1[model], fixed(" "), replacement="")
    temp1[model] = str_replace_all(string=temp1[model], fixed("presence~poly("), replacement="")
    temp1[model] = str_replace_all(string=temp1[model], fixed(",2)"), replacement="")
    temp1[model] = str_replace_all(string=temp1[model], fixed("poly("), replacement="")
  }
  
  #collapse all the factors together
  temp2=paste(temp1, collapse = '+')
  
  #splits them
  temp3=unlist(strsplit(temp2, "+", fixed=TRUE))
  return(temp3)
}

##############################################################################
MapLocalImportance=function(variables.stack, response.raster, other.maps, scale.factor, name){
  
#    browser()
  
  #loading libraries
  require(data.table)
  require(raster)
  
  #starting to create the final stack
  final.stack=stack(response.raster)
  for (other.map in other.maps){
  final.stack=stack(final.stack, other.map)
  }
  final.stack=stack(final.stack, variables.stack)
  
  #name of the response variable and predictors
  name.response.variable=names(response.raster)
  names.variables=names(variables.stack)
  
  #creating vectors to store variable names with "_coef" and "_R2". This vectors will be used to select columns in the data.table
  names.variables.coef=vector()
  
  #creating sample raster
  id.raster=raster(response.raster)
  
  #Loop through scale factors
  for (sf in scale.factor){
    
    #define bigger cells
    id.raster=aggregate(id.raster, fact=sf, expand=TRUE)
    
    #add ID values
    id.raster=setValues(id.raster, seq(1, ncell(id.raster)))
    
    #go back to the previous resolution
    id.raster=disaggregate(id.raster, fact=sf)
    
    #need to use crop here, more cells than expected!
    id.raster=crop(id.raster, extent(response.raster))
    names(id.raster)="id"
    
    #stacking all the data
  names.variables.R2=vector()
  names.variables.pvalue=vector()
  
  #populating vectors
  for (variable in names.variables){
    names.variables.coef[length(names.variables.coef)+1]=paste(variable, "_coef",sep="")
    names.variables.R2[length(names.variables.R2)+1]=paste(variable, "_R2", sep="")
    names.variables.pvalue[length(names.variables.pvalue)+1]=paste(variable, "_pvalue", sep="")
  }
  
  #list of formulas to fit the linear models
  formulas=list()
    data.stack=stack(variables.stack, response.raster, id.raster)
    
    #as data frame
    data.df=as.data.frame(data.stack)
    
    #id as factor
    data.df$id=as.factor(data.df$id)
    
    #ordered row names
    data.df$key=seq(1, nrow(data.df))
    
    #create columns to store coef, R2 and pvalues
    data.df[ , c(names.variables.coef, names.variables.R2, names.variables.pvalue)]=as.numeric(NA)
    
    #fill formulas
    formulas=lapply(names.variables, function(x) as.formula(paste(name.response.variable, " ~ ", x, sep="")))
    
    #names for the formulas list
    names(formulas)=names.variables
    
    #counts the number of non-NA values on each id
    valid.ids=aggregate(x=!is.na(data.df[ , names.variables[1]]), by=list(data.df$id), FUN=sum)
    
    #right column names (setnames is better than names, doesn't copy the whole table again)
    setnames(valid.ids, old=names(valid.ids), new=c("id", "n"))
    
    #selects ids with 30 or more cases
    valid.ids=valid.ids[valid.ids$n>=30, "id"]
    
    #convert data.frame to data.table and set key (subsets are faster this way)
    data.df=data.table(data.df)
    setkey(data.df, id)
    
    #ITERATE THROUGH IDs
    for (valid.id in valid.ids){
      
      #fits a model for each variable
      lm.temp=lapply(names.variables, function(x) lm(formulas[[x]], data=data.df[paste(valid.id)], na.action=na.exclude))
      names(lm.temp)=names.variables
      
      #storing coefficients
      data.df[paste(valid.id), names.variables.coef:=lapply(lm.temp, function(x) summary(x)$coefficients[2]), with=FALSE]
      
      #storing R2
      data.df[paste(valid.id), names.variables.R2:=lapply(lm.temp, function(x) summary(x)$r.squared), with=FALSE]
      
      #storing pvalue
      data.df[paste(valid.id), names.variables.pvalue:=lapply(lm.temp, function(x) anova(x)$'Pr(>F)'[1]), with=FALSE]
      
      #remove results list to start from scratch in the next loop
      rm(lm.temp)
      
    } #end of loop through ids
    
    #Not using data.table anymore, converting to data.frame
    data.df=data.frame(data.df)
    
    #ordering the table
    data.df=data.df[with(data.df, order(data.df$key)), ]
    
    #TURNING THE RESULTS INTO A MAP
    #copy variables stack
    stack.coef=variables.stack
    stack.R2=variables.stack
    stack.pvalue=variables.stack
    
    ##loop through variables to set values and generate values for the resulting stack
    names.stack.coef=vector()
    names.stack.R2=vector()
    names.stack.pvalue=vector()
    
    for (variable.name in names.variables){
      
      #populate vectors with names
      names.stack.coef[length(names.stack.coef)+1]=paste(variable.name, "_coef_", sf, sep="")
      names.stack.R2[length(names.stack.R2)+1]=paste(variable.name, "_R2_", sf, sep="")
      names.stack.pvalue[length(names.stack.pvalue)+1]=paste(variable.name, "_pvalue_", sf, sep="")
      
      #set coef values
      stack.coef[[variable.name]]=setValues(x=stack.coef[[variable.name]], values=data.df[ , paste(variable.name, "_coef", sep="")])
      
      #Set R2 values
      stack.R2[[variable.name]]=setValues(x=stack.R2[[variable.name]], values=data.df[ , paste(variable.name, "_R2", sep="")])
      
      #Set pvalues
      stack.pvalue[[variable.name]]=setValues(x=stack.pvalue[[variable.name]], values=data.df[ , paste(variable.name, "_pvalue", sep="")])
      
    }
    
    #set stacks names
    names(stack.coef)=names.stack.coef
    names(stack.R2)=names.stack.R2
    names(stack.pvalue)=names.stack.pvalue
    
    #mask values outside the coast
    stack.coef=raster::mask(x=stack.coef, mask=response.raster)
    stack.R2=raster::mask(x=stack.R2, mask=response.raster)
    stack.pvalue=raster::mask(x=stack.pvalue, mask=response.raster)
    
    #computes the layer with the maximum R2 value for each cell
    most.important.layer<-which.max(stack.R2)
    names(most.important.layer)=paste("most_important_variable_", sf, sep="")
    
    #stack stacks with the final stack (not kidding)
    final.stack=stack(final.stack, stack.coef, stack.R2, stack.pvalue, most.important.layer)
    
  }
  
  #save R object
  save(final.stack, file=paste("local_importance_", name, ".Rdata", sep=""))
  
  #returning results
  final.stack
  
} #end of function


#########################################################################
#FUNCTION TO PLOT RESULTS
PlotLocalImportance=function(local.importance.stack, scale.factor, names.variables, name, points){
  
  #   local.importance.stack=local.importance.pi
  #   variables.stack=v.brick.5km.pi
  #   scale.factor=c(10)
  #   path="./best_models_analysis"
  #   name="peninsula"
  #   points=presence.data.reliable.separated[, c("x", "y")]
  
  #required libraries
  require(RColorBrewer)
  require(raster)
  require(rgdal)
  
  #PLOTTING R2 AND COEFFICIENTS
  #----------------------------
  
  #breakpoints and colors for R2 and coef
  breaks.R2=seq(0, 1, 0.1)
  colors.R2=colorRampPalette(brewer.pal(10,"YlGnBu"))(10)
  
  #loop through scales and variables
    for (name.variable in names.variables){
      for (sf in scale.factor){
        
        png(paste("local_importance_", name.variable, "_", sf, ".png", sep=""), width=2000, height=1430*2, pointsize=25)
      
      #masking non-significant cells
      R2.temp.map=raster::mask(local.importance.stack[[paste(name.variable, "_R2_", sf, sep="")]], local.importance.stack[[paste(name.variable, "_pvalue_", sf, sep="")]] < 0.05, maskvalue=0)
      
      coef.temp.map=raster::mask(local.importance.stack[[paste(name.variable, "_coef_", sf, sep="")]], local.importance.stack[[paste(name.variable, "_pvalue_", sf, sep="")]] < 0.05, maskvalue=0)
      
      #plot dimensions
      par(mfrow=c(2,1), oma=c(1,1,1,4))
      
      #plot R2
      plot(R2.temp.map, col=colors.R2, breaks=breaks.R2)
      points(points$x, points$y, lwd=4)
      title(paste(5*sf, "km - Variable = ", name.variable, " - R squared", sep=""), cex.main=1.5)
      
      #plot coefficient
      PlotCenteredDivergentPaletteRaster(raster=coef.temp.map)
      points(points$x, points$y, lwd=4)
      title(paste(5*sf, "km - Variable = ", name.variable, " - Coefficient", sep=""), cex.main=1.5)
      
      dev.off()
      
    } #end of iteration through variables
  } #end of iteration through scales


  #BLENDING VARIABLE IMPORTANCE WITH
#loop through variables
for (sf in scale.factor){
for (name.variable in names.variables){
  
  #plot blending
  WhiteningVariableImportance(average=local.importance.stack[["best_models_average"]], importance=local.importance.stack[[paste(name.variable, "_R2_", sf, sep="")]], name=paste(name.variable, "_", sf, sep=""), points=points[, c("x", "y")])
  
  #putting legend and map together
  system(paste("convert  ./", name.variable, "_", sf, "_importance.png  ./", name.variable, "_", sf, "_legend.png -geometry +1500+100 -composite -pointsize 60 -gravity north  -annotate 0 'Variable = ", name.variable, " - Scale = ", sf, "' blend_suitability_importance_", name.variable, "_", sf, ".png", sep=""))
  
  #remove maps we don't need
  system(paste("rm ", name.variable, "_", sf, "_importance.png", sep=""))
  system(paste("rm ", name.variable, "_", sf, "_legend.png", sep=""))
  
  } #end of iteration through variables
} #end of iteration through scales

  
  #PLOTTING MAXIMUM VALUES
  #-----------------------
  #color table for maps of variable importance
  colors.max<-rev(brewer.pal(n=length(names.variables), name="Set2"))
  
  #iterates through scale factors
  for (sf in scale.factor){
    
    #open pdf plot
    png(paste("most_important_factor_", sf, "_", name, ".png", sep=""), width=2000, height=1430, pointsize=25)
    
    #actual plot
    plot(local.importance.stack[[paste("most_important_variable_", sf, sep="")]], col=colors.max, legend=FALSE)
    points(points$x, points$y, lwd=4)
    legend("topright", title="Legend", c(names.variables), col=colors.max, lwd=10, bg="white")
    title(paste(5*sf, "km - Most important factor", sep=""), cex.main=1.5)
    
    dev.off()
    
  } #end of loop through scales
  
} #end of function


##############################################################################
#PLOT MAPS WITH DIVERGENT PALETTE CENTERED IN ZERO
PlotCenteredDivergentPaletteRaster=function(raster){
  
  #loading libraries
  require(RColorBrewer)
  
  #color scale
  colors=rev(colorRampPalette(brewer.pal(10,"RdYlBu"))(22))

  #extreme value for the color table
  max.abs.value=max(abs(maxValue(raster)), abs(minValue(raster)))
    
    #breakpoints to center the color table to 0
    breakpoints=round(seq(from=max.abs.value, to=-max.abs.value, length.out=21), 4)
    
    #actual plot
    plot(raster, breaks=breakpoints, col=colors)

}
