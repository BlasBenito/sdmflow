##############################################################################
mapLocalImportance=function(predictors.brick, response.raster, scale.factor){

  #loading libraries
  require(data.table)
  require(raster)

  if(scale.factor < 5){
    scale.factor <- 5
  }

  #starting to create the final stack
  final.stack <- stack()
  # final.stack <- stack(final.stack, predictors.brick)

  #name of the response variable and predictors
  name.response.variable <- names(response.raster)
  names.variables <- names(predictors.brick)

  #creating vectors to store variable names with "_coef" and "_R2". This vectors will be used to select columns in the data.table
  names.variables.coef <- vector()

  #creating sample raster
  id.raster <- raster(response.raster)

  #Loop through scale factors
  for (sf in scale.factor){

    #define bigger cells
    id.raster <- aggregate(id.raster, fact = sf, expand = TRUE)

    #add ID values
    id.raster <- setValues(id.raster, seq(1, ncell(id.raster)))

    #go back to the previous resolution
    id.raster <- disaggregate(id.raster, fact = sf)

    #need to use crop here, more cells than expected!
    id.raster <- crop(id.raster, extent(response.raster))
    names(id.raster) <- "id"

    #stacking all the data
    names.variables.R2 <- vector()
    names.variables.pvalue <- vector()

    #populating vectors
    for (variable in names.variables){
      names.variables.coef[length(names.variables.coef) + 1] <- paste(variable, "_coef",sep = "")
      names.variables.R2[length(names.variables.R2) + 1] <- paste(variable, "_R2", sep = "")
      names.variables.pvalue[length(names.variables.pvalue) + 1] <- paste(variable, "_pvalue", sep = "")
    }

    #list of formulas to fit the linear models
    formulas <- list()
    data.stack <- stack(predictors.brick, response.raster, id.raster)

    #as data frame
    data.df <- as.data.frame(data.stack)

    #id as factor
    data.df$id <- as.factor(data.df$id)

    #ordered row names
    data.df$key <- seq(1, nrow(data.df))

    #create columns to store coef, R2 and pvalues
    data.df[ , c(names.variables.coef, names.variables.R2, names.variables.pvalue)] <- as.numeric(NA)

    #fill formulas
    formulas <- lapply(names.variables, function(x) as.formula(paste(name.response.variable, " ~ ", x, sep = "")))

    #names for the formulas list
    names(formulas) <- names.variables

    #counts the number of non-NA values on each id
    valid.ids <- aggregate(x = !is.na(data.df[ , names.variables[1]]), by = list(data.df$id), FUN = sum)

    #right column names (setnames is better than names, doesn't copy the whole table again)
    setnames(valid.ids, old = names(valid.ids), new = c("id", "n"))

    #selects ids with 25 or more cases
    valid.ids <- valid.ids[valid.ids$n >= 25, "id"]

    #convert data.frame to data.table and set key (subsets are faster this way)
    data.df <- data.table(data.df)
    setkey(data.df, id)

    #ITERATE THROUGH IDs
    for (valid.id in valid.ids){

      #fits a model for each variable
      lm.temp <- lapply(names.variables, function(x) lm(formulas[[x]], data = data.df[paste(valid.id)], na.action = na.exclude))
      names(lm.temp) <- names.variables

      #storing coefficients
      data.df[paste(valid.id), (names.variables.coef) := lapply(lm.temp, function(x) summary(x)$coefficients[2])]

      #storing R2
      data.df[paste(valid.id), (names.variables.R2) := lapply(lm.temp, function(x) summary(x)$r.squared)]

      #storing pvalue
      data.df[paste(valid.id), (names.variables.pvalue) := lapply(lm.temp, function(x) anova(x)$'Pr(>F)'[1])]

      #remove results list to start from scratch in the next loop
      rm(lm.temp)

    } #end of loop through ids

    #Not using data.table anymore, converting to data.frame
    data.df <- data.frame(data.df)

    #ordering the table
    data.df <- data.df[with(data.df, order(data.df$key)), ]

    #TURNING THE RESULTS INTO A MAP
    #copy variables stack
    stack.coef <- predictors.brick
    stack.R2 <- predictors.brick
    stack.pvalue <- predictors.brick

    ##loop through variables to set values and generate values for the resulting stack
    names.stack.coef <- vector()
    names.stack.R2 <- vector()
    names.stack.pvalue <- vector()

    for (variable.name in names.variables){

      #populate vectors with names
      names.stack.coef[length(names.stack.coef)+1] <- paste(variable.name, "_coef_", sf, sep = "")
      names.stack.R2[length(names.stack.R2)+1] <- paste(variable.name, "_R2_", sf, sep = "")
      names.stack.pvalue[length(names.stack.pvalue)+1] <- paste(variable.name, "_pvalue_", sf, sep = "")

      #set coef values
      stack.coef[[variable.name]] <- setValues(x = stack.coef[[variable.name]], values = data.df[ , paste(variable.name, "_coef", sep = "")])

      #Set R2 values
      stack.R2[[variable.name]] <- setValues(x = stack.R2[[variable.name]], values = data.df[ , paste(variable.name, "_R2", sep = "")])

      #Set pvalues
      stack.pvalue[[variable.name]] <- setValues(x = stack.pvalue[[variable.name]], values = data.df[ , paste(variable.name, "_pvalue", sep = "")])

    }

    #set stacks names
    names(stack.coef) <- names.stack.coef
    names(stack.R2) <- names.stack.R2
    names(stack.pvalue) <- names.stack.pvalue

    #mask values outside the coast
    stack.coef <- raster::mask(x = stack.coef, mask = response.raster)
    stack.R2 <- raster::mask(x = stack.R2, mask = response.raster)
    stack.pvalue <- raster::mask(x = stack.pvalue, mask = response.raster)

    #computes the layer with the maximum R2 value for each cell
    most.important.layer<-which.max(stack.R2)
    names(most.important.layer) <- paste("most_important_variable_", sf, sep = "")

    #stack stacks with the final stack (not kidding)
    final.stack <- stack(final.stack, stack.coef, stack.R2, stack.pvalue, most.important.layer)

  }

  #returning results
  return(final.stack)

} #end of function



#RE-ESCALA PESOS
##################################################################
escalaPesos=function(pesos, new.max, new.min, old.max, old.min){
  result = ((pesos - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min
  return(result)
}


#############################################################################
#BOOTSTRAP
#brick: brick de variables
#presencias: tabla de presencias usada para entrenar el modelo
#respuesta: nombre de la variable respuesta, generalmente "presencia"
#predictores: variables para predecir, por defecto, variables en el brick
#formula: formula si el modelo es de tipo GLM o GAM
#modelo: modelo a evaluar: uno de "bioclim", "glm", "gam", "rpart", "rf", "brt"
#repeticiones: número de veces a repetir la evaluación
#porcentaje.evaluación: porcentaje de las presencias a usar para evaluar, 40 por defecto
#radio.ausencias: radio alrededor de presencias (en número de celdas) en el que se seleccionan las ausencias
#... parámetros específicos del tipo de modelo
bootstrapSDM <- function(
  brick,
  presencias,
  respuesta = "presencia",
  predictores = NULL,
  formula.modelo = NULL,
  modelo.nombre = "bioclim",
  repeticiones = 3,
  porcentaje.evaluacion = 40,
  radio.seleccion.ausencias = 5,
  ...
  ){

  #carga librerías
  suppressMessages(require(dplyr))
  suppressMessages(require(raster))
  suppressMessages(require(svMisc))
  suppressMessages(require(ecospat))

  #FUNCIÓN PARA CALCULAR PESOS
  #---------------------------------------------------------------
  #pesos sin escribir mensajes a pantalla
  weightCasesSilent <- function(presence){

    #peso presencias
    n.presences <- sum(presence)
    weight.presences <- 1/n.presences

    #peso ausencias
    n.background <- length(presence)-n.presences
    weight.background <- 1/n.background

    #genera un vector con los los pesos
    weights <- c(rep(weight.presences, n.presences), rep(weight.background, n.background))

    return(weights)
  }


  #BIOCLIM
  #---------------------------------------------------------------
  if(modelo.nombre %in% c("bioclim", "bio", "Bio", "Bioclim", "BIOCLIM")){

    #define función para ajustar modelo
    fitModel <- function(presencia, respuesta, predictores, pesos = NULL, env = parent.frame()){

      #ajusta modelo
      modelo <- dismo::bioclim(
        x = brick[[predictores]],
        p = presencia[presencia[, respuesta] == 1, c("x", "y")]
      )

      #predice mapa
      mapa <- raster::predict(
        object = brick,
        model = modelo
      )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
        )
      return(output)

    } #fin de la función

  } #fin de la sección bioclim


  #RANDOM FOREST
  #---------------------------------------------------------------
  if(modelo.nombre %in% c("ranger", "Ranger", "RANGER", "randomforest", "rf", "RandomForest", "Random Forest")){

    #carga librería
    suppressMessages(require(ranger))

    #parámetros por defecto
    if(exists("num.trees") == FALSE){num.trees <- 1000}
    if(exists("min.node.size") == FALSE){min.node.size <- 10}
    if(exists("mtry") == FALSE){mtry <- 3}

    #define función para ajustar modelo
    fitModel <- function(presencia, respuesta, predictores, pesos, env = parent.frame(), ...){

      #ajusta modelo
      modelo <- ranger::ranger(
        data = presencia[, c(respuesta, predictores)],
        dependent.variable.name = respuesta,
        case.weights = pesos,
        num.trees = num.trees,
        min.node.size = min.node.size,
        mtry = mtry
      )

      #predice mapa
      mapa <- raster::predict(
          object = brick,
          model = modelo,
          type = 'response',
          progress = "",
          fun = function(modelo, ...){predict(modelo, ...)$predictions}
        )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
      )
      return(output)

    }#final de función

  }#final de sección random forest


  #GAM
  #----------------------------------------------------------------
  if(modelo.nombre %in% c("gam", "Gam", "Gam")){

    #carga librería
    suppressMessages(require(mgcv))

    #nombre del modelo (para seleccionar fórmula más abajo)
    modelo.nombre <- "gam"

    #parámetros
    if(exists("gamma") == FALSE){gamma <- 1}

    #define función para ajustar el modelo
    fitModel <- function(presencia, respuesta, predictores, pesos, env = parent.frame(), ...){

      #ajusta mocelo
      modelo <- mgcv::bam(
        formula.modelo,
        family = quasibinomial(link = logit),
        data = presencia[, c(respuesta, predictores)],
        select = TRUE,
        weights = pesos
      )

      #predice mapa
      mapa <- raster::predict(
        object = brick,
        model = modelo,
        type="response",
        gamma = gamma
      )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
      )
      return(output)

    }#final de función

  }#final de sección


  #GLM
  #----------------------------------------------------------------
  if(modelo.nombre %in% c("glm", "Glm", "GLM")){

    #nombre del modelo para seleccionar la fórmula
    modelo.nombre <- "glm"

    #define función para ajustar el modelo
    fitModel <- function(presencia, respuesta, predictores, pesos, env = parent.frame()){

      #ajusta modelo
      modelo <- glm(
        formula = formula.modelo,
        family = quasibinomial(link = logit),
        data = presencia[, c(respuesta, predictores)],
        weights = pesos
      )

      #predice mapa
      mapa <- raster::predict(
        object = brick,
        model = modelo,
        type="response"
      )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
      )
      return(output)

    }#fin de función

  }#fin de sección


  #MAXENT
  #----------------------------------------------------------------
  if(modelo.nombre %in% c("maxent", "Maxent", "MAXENT", "maxnet", "Maxnet", "MAXNET", "max", "Max", "MAX")){

    #carga librería
    suppressMessages(require(maxnet))

    #define regmult por defecto
    if(exists("regmult") == FALSE){regmult <- 1}

    #define función para ajustar el modelo
    fitModel <- function(presencia, respuesta, predictores, pesos = NULL, env = parent.frame(), ...){

      #ajusta modelo
      modelo <- maxnet::maxnet(
        p = presencia[, respuesta],
        data = presencia[, predictores],
        regmult = regmult
      )

      #predice mapa
      mapa <- raster::predict(
        object = brick,
        model = modelo,
        type="cloglog"
      )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
      )
      return(output)

    }#fin de función

  }#fin de sección


  #RPART
  #----------------------------------------------------------------
  if(modelo.nombre %in% c("rpart", "recursive partition")){

    #carga librería
    suppressMessages(require(rpart))

    #nombre del modelo para elegir fórmula más abajo
    modelo.nombre <- "rpart"

    #parámetros por defecto
    if(exists("minbucket") == FALSE){minbucket <- 10}

    #define función para ajustar el modelo
    fitModel <- function(presencia, respuesta, predictores, pesos, env = parent.frame(), ...){

      #ajusta el modelo
      modelo <- rpart::rpart(
        formula = formula.modelo,
        data = presencia[, c(respuesta, predictores)],
        weights = pesos,
        control = rpart.control(minbucket = minbucket)
      )

      #predice el mapa
      mapa <- raster::predict(
        object = brick,
        model = modelo
      )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
      )
      return(output)

    }#fin de función

  }#fin de sección


  #BRT
  #----------------------------------------------------------------
  if(modelo.nombre %in% c("brt", "BRT", "Brt", "boosted", "Boosted", "BOOSTED")){

    #carga librería requerida
    suppressMessages(require(dismo))

    #parámetros por defecto
    if(exists("tree.complexity") == FALSE){tree.complexity <- 10}
    if(exists("learning.rate") == FALSE){learning.rate <- 0.005}
    if(exists("nfolds") == FALSE){nfolds <- 2}

    #define función para ajustar el modelo
    fitModel <- function(presencia, respuesta, predictores, pesos, env = parent.frame(), ...){

      #ajusta modelo
      modelo <- dismo::gbm.step(
        data = presencia,
        site.weights = pesos,
        gbm.x = predictores,
        gbm.y = respuesta,
        learning.rate = learning.rate,
        tree.complexity = tree.complexity,
        nfolds = nfolds,
        plot.main = FALSE,
        silent = TRUE
      )

      #predice a mapa
      mapa <- raster::predict(
        object = brick,
        model = modelo,
        n.trees = modelo$gbm.call$best.trees,
        type="response"
      )

      #extrae modelo y mapa
      output <- list(
        modelo = modelo,
        mapa = mapa
      )
      return(output)

    }#final de función

  }#final de sección


  #PREPARA FÓRMULA
  #-----------------------------------------------------------

  #preparar fórmula si el modelo es glm y formula = NULL
  if(is.null(formula.modelo) & modelo.nombre == "glm"){
    formula.modelo <- as.formula(
      paste(
        "presencia ~ poly(",
        paste(
          predictores,
          collapse=", 2) + poly("),
        ", 2)",
        collapse=""
      )
    )
  }

  #preparar fórmula si el modelo es gam y formula = NULL
  if(is.null(formula.modelo) & modelo.nombre == "gam"){
    formula.modelo <- as.formula(
      paste(
        "presencia ~ s(",
        paste(
          predictores,
          collapse = ") + s("
        ),
        ")", collapse=""
      )
    )
  }

  #prepara fórmula de modelo aditivo sin interacciones
  if(is.null(formula.modelo) & modelo.nombre == "rpart"){
    formula.modelo <- as.formula(
      paste(
        "presencia ~ ",
        paste(
          predictores,
          collapse = " + "
          )
        )
      )
  }


  #SELECCIONA PUNTOS DE AUSENCIA DENTRO DE UN RADIO DADO
  #-----------------------------------------------------

  #separa presencias de ausencias
  presencias.df <- presencias[presencias[, respuesta] == 1, ]
  ausencias.df <- presencias[presencias[, respuesta] == 0, ]

  #distancia mínima alrededor de cada presencia
  distancia = raster::xres(brick) * radio.seleccion.ausencias

  #vector para guardar los índices de las ausencias que están dentro del radio definido por min.dist
  ausencias.evaluacion.indices <- vector()

  #itera sobre las presencias para seleccionar las ausencias que les quedan dentro del radio establecido
  for(row.i in 1:nrow(presencias.df)){

    #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
    f <- presencias.df[row.i, c("x", "y")]

    #genera los límites de la cuadrícula de búsqueda
    ymax <- f$y + distancia
    ymin <- f$y - distancia
    xmax <- f$x + distancia
    xmin <- f$x - distancia

    #selección de indices las ausencias
    temp <- which(ausencias.df$y <= ymax & ausencias.df$y >= ymin & ausencias.df$x <= xmax & ausencias.df$x >= xmin)

    #guarda los índices
    ausencias.evaluacion.indices <- c(ausencias.evaluacion.indices, temp)

  }

  #selecciona las ausencias de evaluación (eliminando duplicados)
  ausencias.evaluacion.df <- ausencias.df[unique(ausencias.evaluacion.indices), ]
  rm(ausencias.evaluacion.indices)

  #calculando número de presencias y ausencias a guardar en cada iteración
  presencias.df.nrow <- nrow(presencias.df)
  ausencias.df.nrow <- nrow(ausencias.df)
  ausencias.evaluacion.df.nrow <- nrow(ausencias.evaluacion.df)
  evaluacion.n <- floor((porcentaje.evaluacion * presencias.df.nrow)/100)

  #objetos para guardar auc y mapas
  output.auc <- vector()
  output.boyce <- vector()
  output.brick <- raster::stack()
  output.threshold <- vector()
  output.modelos <- list()


  #EJECUTA REPETICIONES DEL MODELO ELEGIDO
  #---------------------------------------
  for (i in 1:repeticiones){

    #barra de progreso
    svMisc::progress(value = i, max.value = repeticiones)

    #asegura resultados reproducibles para distintos modelos con las mismas presencias
    set.seed(i)

    #selecciona presencias y ausencias al azar
    presencias.evaluacion.indices.i <- sample(
      x = presencias.df.nrow,
      size = evaluacion.n
      )
    ausencias.evaluacion.indices.i <- sample(
      x = ausencias.evaluacion.df.nrow,
      size = evaluacion.n
      )

    #tomando las ausencias de evaluacion y entrenamiento
    ausencias.evaluacion.df.i <- ausencias.evaluacion.df[ausencias.evaluacion.indices.i, ]
    #nota: elimina de ausencias.df las seleccionadas para evaluar
    ausencias.entrenamiento.df.i <- suppressMessages(
        dplyr::anti_join(
        x = ausencias.df,
        y = ausencias.evaluacion.df.i
        )
      )

    #tomando las presenciasd e evaluacion y entrenamiento
    presencias.evaluacion.df.i <- presencias.df[presencias.evaluacion.indices.i, ]
    presencias.entrenamiento.df.i <- presencias.df[-presencias.evaluacion.indices.i, ]

    #generando el dataframe de entrenamiento
    entrenamiento.df.i <- rbind(
      presencias.entrenamiento.df.i,
      ausencias.entrenamiento.df.i
      )

    #generando el dataframe de evaluación
    evaluacion.df.i <- rbind(
      presencias.evaluacion.df.i[, c(respuesta, "x", "y")],
      ausencias.evaluacion.df.i[, c(respuesta, "x", "y")]
    )

    #calcula pesos
    pesos <<- weightCasesSilent(presence = entrenamiento.df.i[, respuesta])

    #genera predicción del modelo
    model.i <- fitModel(
      presencia = entrenamiento.df.i,
      respuesta = respuesta,
      predictores = predictores,
      pesos = pesos
    )

    #guarda el mapa
    output.brick <- stack(output.brick, model.i$mapa)

    #guarda el modelo
    output.modelos[[i]] <- model.i$modelo

    #extrae valores del modelo sobre presencias y ausencias de evaluación
    map.i.values <- data.frame(
      presencia = evaluacion.df.i[, respuesta],
      idoneidad = raster::extract(
        x = model.i$mapa,
        y = evaluacion.df.i[, c("x", "y")]
        )
      )

    #evalúa modelo
    evaluacion.i <- dismo::evaluate(
      p = map.i.values[map.i.values$presencia == 1, "idoneidad"],
      a = map.i.values[map.i.values$presencia == 0, "idoneidad"]
      )

    #extrae auc
    output.auc[i] <- evaluacion.i@auc

    #computa boyce
    output.boyce[i] <- ecospat::ecospat.boyce(
      fit = na.omit(as.vector(model.i$mapa)),
      obs = map.i.values[map.i.values$presencia == 1, "idoneidad"],
      PEplot = FALSE
    )$Spearman.cor

    #extrae threshold
    output.threshold[i] <- evaluacion.i@t[which.max(evaluacion.i@TPR + evaluacion.i@TNR)]

  } #final de las iteraciones

  #preparando el dataframe de salida
  output.df <- data.frame(
    modelo = rep(modelo.nombre, repeticiones),
    repeticion = 1:repeticiones,
    id = paste(modelo.nombre, 1:repeticiones, sep = "_"),
    auc = output.auc,
    boyce = output.boyce,
    threshold = output.threshold,
    stringsAsFactors = FALSE
  )

  #nombres del brick
  names(output.brick) <- names(output.modelos) <- paste(modelo.nombre, 1:repeticiones, sep = "_")

  #lista final
  output.list <- list()
  output.list$mapas <- brick(output.brick)
  output.list$modelos <- output.modelos
  output.list$evaluacion <- output.df

  return(output.list)

}


#############################################################################



#############################################################################
#WHITENING
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





#########################################################################
#SENSIBILIDAD RANDOM FOREST
#calcula sensibilidad de la importancia de las variables a los parámetros mtry y minimum.node.size
sensibilidadRF <- function(formula, presencias, tipo, pesos, mtry.list, mns.list, num.trees){

  require(ranger)
  require(ggplot2)

  #lista de resultados
  sensibilidad.list <- list()

  #contador iteraciones
  i <- 0

  #itera sobre todas las combinaciones
  for(mtry.i in mtry.list){
    for(mns.i in mns.list){

      #otra iteración
      i <- i + 1

      #modelo con todas las variables
      if(tipo == "background"){
      temp.rf <- ranger::ranger(
        formula = formula,
        data = presencias,
        num.trees = num.trees,
        min.node.size = mns.i,
        mtry = mtry.i,
        importance = "permutation",
        scale.permutation.importance = TRUE,
        case.weights = pesos
      )
      } else {
        temp.rf <- ranger::ranger(
          formula = formula,
          data = presencias,
          num.trees = num.trees,
          min.node.size = mns.i,
          mtry = mtry.i,
          importance = "permutation",
          scale.permutation.importance = TRUE,
          )
      }

      #importancia de las variables
      temp.importancia <- data.frame(
        importancia = sort(
          temp.rf$variable.importance,
          decreasing = TRUE
        )
      )

      #le añade mtry.i, mnsi y rsquared
      temp.importancia$variable <- rownames(temp.importancia)
      temp.importancia$mtry <- mtry.i
      temp.importancia$min.node.size <- mns.i
      temp.importancia$r.squared <- temp.rf$r.squared

      #lo guarda en la lista
      sensibilidad.list[[i]] <- temp.importancia

    }
  }

  #lista a dataframe
  sensibilidad.df <- do.call("rbind", sensibilidad.list)

  #plotea sensibilidad de la importancia de las variables
  plot.list <- list()
  for(variable in unique(sensibilidad.df$variable)){
    plot.list[[variable]] <- ggplot(
      data = sensibilidad.df[sensibilidad.df$variable == variable, ],
      aes(
        x = mtry,
        y = min.node.size,
        fill = importancia
      )
    ) +
      geom_tile() +
      scale_x_continuous(breaks = mtry.list) +
      viridis::scale_fill_viridis(direction = -1) +
      ggtitle(variable) +
      labs(fill = "Imp.")
  }

  #plotea importancia
  plot.importance <- cowplot::plot_grid(plotlist = plot.list)
  x11(width = 20, height = 15, pointsize = 20)
  print(plot.importance)

  #plotea r cuadrado
  plot.r <- ggplot(
    data = sensibilidad.df[sensibilidad.df$variable == unique(sensibilidad.df$variable)[1], ],
    aes(
      x = mtry,
      y = min.node.size,
      fill = r.squared
    )
  ) +
    geom_tile() +
    scale_x_continuous(breaks = mtry.list) +
    viridis::scale_fill_viridis(direction = -1) +
    ggtitle("R-squared") +
    labs(fill = "R2")
  x11(width = 10, height = 10, pointsize = 20)
  print(plot.r)

  #lista de salida
  output.list <- list()
  output.list$importancia <- plot.importance
  output.list$r <- plot.r

  return(output.list)
}



#########################################################################
#INTERACTION PLOT
#plots the interaction between two predictors in a tree-based model
#model: a ranger, randomForest, or rpart model
#data: dataframe used to fit the model
#x: character, name of variable to plot on the x axis
#y: character, name of variable to plot on the y axis
#grid: numeric, resolution of the plot grid
plotInteraction <- function(model, data, x, y, z, grid = 100, point.size.range=c(0.1, 1.5), print=TRUE){

  require(cowplot)
  require(ggplot2)
  require(viridis)

  #generating grid
  newdata <- expand.grid(
    seq(
      min(data[[x]]),
      max(data[[x]]),
      length.out = grid
      ),
    seq(
      min(data[[y]]),
      max(data[[y]]),
      length.out = grid)
    )
  colnames(newdata) <- c(x, y)

  #setting the other variables to their mean
  other_vars <- setdiff(names(data), c(x, y, z))
  n <- nrow(data)
  for(i in other_vars){
    # newdata[, i] <- data[, i][sample(n, n)]
    newdata[, i] <- mean(data[, i])
  }

  #predicting different types of models
  if("ranger" %in% class(model)){
    require(ranger)
    newdata[, z] <- predict(
      model,
      newdata)$predictions
  }
  if("rpart" %in% class(model)){
    require(rpart)
    newdata[, z] <- predict(
      model,
      newdata,
      type="vector"
      )

  }
  if("glm" %in% class(model)){
    require(mgcv)
    newdata[, z] <- predict(
      model,
      newdata,
      type="response"
      )
  }
  if("maxnet" %in% class(model)){
    require(maxnet)
    newdata[, z] <- predict(
      model,
      newdata,
      type = "cloglog"
      )
  }
  if("gbm" %in% class(model)){
    require(dismo)
    newdata[, z] <- predict(
      model,
      newdata,
      n.trees = temp.brt$gbm.call$best.trees,
      type = "response"
      )
  }

    #preparing z as a factor
    data[, z] <- factor( data[, z], levels = c(0, 1))

    #plot
    p1 <- ggplot(
      newdata,
      aes_string(
        x = x,
        y = y)
      ) +
      geom_raster(aes_string(fill = z)) +
      viridis::scale_fill_viridis(
        direction = -1,
        begin = 0.1
        ) +
      geom_point(
        data = data,
        aes_string(
          x = x,
          y = y,
          size = z
          ),
        shape = 21,
        alpha = 0.5,
        color = "black",
        fill = "white"
        ) +
      scale_size_discrete(range = point.size.range) +
      labs(
        fill = "Predicción",
        size = "Observación"
        )

  if(print == TRUE){
    print(p1)
  }else{
      return(p1)
    }

}



#plotea densidad y curva de respuesta de un glm con una sola variable
#############################################################################
plotRespuesta <- function(modelo, presencias, variable){

  require(ggplot2)
  require(viridis)
  require(cowplot)
  theme_set(theme_cowplot())

  #dataframe para guardar predicción
  pred.df <- data.frame(
    variable = sort(unique(presencias[, variable]))
  )
  colnames(pred.df) <- variable

  #predicción para glm gam
  #nota: no funciona muy bien con earth
  if(sum(class(modelo) %in% c("glm", "gam", "earth")) > 0){
    require(mgcv)
    require(earth)
  pred.df$y <- predict(
    object = modelo,
    newdata = pred.df,
    type = "response"
    )
  }

  #predicción para maxnet
  if(sum(class(modelo) %in% "maxnet") == 1){
    require(maxnet)
    pred.df$y <- predict(
      object = modelo,
      newdata = pred.df,
      type = "cloglog"
    )
  }

  #plotea con ggplot
  p <- ggplot() +
    geom_density(
      data = presencias,
      aes(
        x = get(variable),
        y = ..scaled..,
        fill = factor(presencia),
        group = factor(presencia)
      ),
      alpha = 0.5,
      size = 0.1
    ) +
    viridis::scale_fill_viridis(
      direction = -1,
      discrete = TRUE
    ) +
    geom_line(
      data = pred.df,
      aes(
        x = get(variable),
        y = y
        ),
      color = "gray30",
      size = 2
    ) +
    theme(legend.position = "bottom") +
    labs(fill = "Presencia") +
    xlab(variable) +
    ylab("Idoneidad")

    print(p)

    return(p)
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
#WEIGHT PRESENCE/BACKGROUND DATA
weightCases <- function(presence){

  #computing weight for presences
  n.presences <- sum(presence)
  print(paste("Presence points = ", n.presences, sep=""))
  weight.presences <- 1/n.presences
  print(paste("Weight for presences = ", weight.presences, sep=""))

  n.background <- length(presence)-n.presences
  print(paste("Background points = ", n.background, sep=""))
  weight.background <- 1/n.background
  print(paste("Weight for background = ", weight.background, sep=""))

  #genera un vector con los los pesos
  weights <- c(rep(weight.presences, n.presences), rep(weight.background, n.background))

  return(weights)
}


#Simula ausencias
#############################################################################################
simulaAusencia <- function(xy, brick, pob.muestreadas, buffer.km, distancia.thinning){

  #librerías requeridas
  require(raster)
  require(rgeos)
  require(dismo)
  require(sp)

  #selecciona al azar un porcentaje de poblaciones muestreadas
  pob.muestreadas <- pob.muestreadas / 100
  xy <- xy[sample(nrow(xy), floor(nrow(xy) * pob.muestreadas), replace = FALSE), ]

  #radio a metros
  buffer.m <- buffer.km * 1000

  #convertir la tabla de xy en objeto "sp" (clase SpatialPoints)
  sp::coordinates(xy) <- c("x", "y")

  #hacer buffer
  buffer <- dismo::circles(xy, d = buffer.km*1000, lonlat = TRUE)
  buffer.dissolve <- rgeos::gUnaryUnion(buffer@polygons)

  #extrae los identificadores de las celdas
  celdas <- unlist(raster::cellFromPolygon(object = brick, p = buffer.dissolve))

  #toma uno de los mapas de brick como plantilla para la máscara
  mascara.temp <- raster::raster(brick[[1]])

  #crea vector de valores
  valores <- rep(NaN, raster::ncell(mascara.temp))
  valores[celdas] <- 1

  #mascara a partir de valores
  mascara.temp <- raster::setValues(mascara.temp, values = valores)
  mascara.temp <- raster::mask(mascara.temp, mask = brick[[1]])

  #genera las ausencias
  ausencia <- data.frame(
    dismo::randomPoints(
      mask = mascara.temp,
      n = floor(length(celdas)),
      p = xy,
      excludep = TRUE
    )
  )

  #ausencia thinning a las ausencias también
  ausencia <- thinning(
    xy = ausencia,
    brick = variables$brick,
    separacion = distancia.thinning
  )

  return(ausencia)
}


#Genera background restringido a una distancia alrededor de los puntos conocidos de presencia
#############################################################################################
backgroundRestringido <- function(xy, brick, buffer.km, percent){

  #librerías requeridas
  require(raster)
  require(rgeos)
  require(dismo)
  require(sp)

  #convertir la tabla de xy en objeto "sp" (clase SpatialPoints)
  sp::coordinates(xy) <- c("x", "y")

  #hacer buffer
  buffer <- dismo::circles(xy, d = buffer.km*1000, lonlat = TRUE)
  buffer.dissolve <- rgeos::gUnaryUnion(buffer@polygons)

  #extrae los identificadores de las celdas
  celdas <- unlist(raster::cellFromPolygon(object = brick, p = buffer.dissolve))

  #toma uno de los mapas de brick como plantilla para la máscara
  mascara.temp <- raster::raster(brick[[1]])

  #crea vector de valores
  valores <- rep(NaN, raster::ncell(mascara.temp))
  valores[celdas] <- 1

  #mascara a partir de valores
  mascara.temp <- raster::setValues(mascara.temp, values = valores)
  mascara.temp <- raster::mask(mascara.temp, mask = brick[[1]])

  #genera background
  background <- data.frame(dismo::randomPoints(mask = mascara.temp, n = floor(length(celdas)/(100/percent))))

  return(background)
}



#Calcula la autocorrelación espacial de un conjunto de puntos de presencia para cada una de las variables en un brick
##############################################################################################
##############################################################################################
autocor <- function(brick = NULL, xy = NULL){

  require(ape)
  require(geosphere)

  #remove scientific notation
  options(scipen = 9999)

  #extrae valores de las variables para los puntos de presencia
  xy.variables <- na.omit(
    data.frame(
      xy,
      raster::extract(
        x = brick,
        y = xy,
        df = TRUE,
        cellnumbers = FALSE
      )
    )
  )

  #quitando una variable que no necesitamos
  xy.variables$ID <- NULL

  #matriz de distancias inversas entre presencias
  xy.distancias <- 1/ (geosphere::distm(
    x = xy.variables[, c("x", "y")],
    fun = distGeo
  ) / 1000)

  #reemplazando Inf por cero
  xy.distancias[!is.finite(xy.distancias)] <- 0

  #diagonal a 0
  diag(xy.distancias) <- 0

  #calcula I de Moran para cada columna
  #check: https://cran.r-project.org/web/packages/ape/vignettes/MoranI.pdf
  moran.i <- apply(
    xy.variables,
    2,
    FUN = function(x)  ape::Moran.I(x, xy.distancias, na.rm = TRUE)
    )

  #a dataframe
  output.df <- do.call("rbind", moran.i)

  #eliminando una columna que no es importante
  output.df$sd <- NULL

  return(output.df)

}


#configura servidor de ALA4R siguiendo las instrucciones de esta página
#######################################################################
#######################################################################
#https://www.gbif.es/trabajar-con-info-de-portal-nacional-utilizando-r/
configuraALA4R <- function(){
  require(ALA4R)
  server_config <- list(
    max_occurrence_records = 500000,
    server_max_url_length = 8150,
    brand = "ALA4R",
    notify = "Please use https://github.com/AtlasOfLivingAustralia/ALA4R/issues/ or email to support@ala.org.au",
    support_email = "infogbifspain@gmail.com",
    reasons_function = "ala_reasons",
    fields_function = "ala_fields",
    occurrences_function = "occurrences",
    config_function = "ala_config",
    base_url_spatial = "https://geo-ws.gbif.es/layers-service",
    base_url_bie = "https://especies-ws.gbif.es",
    base_url_biocache = "https://registros-ws.gbif.es/",
    base_url_images = "https://images.gbif.es/",
    base_url_logger = "https://logger.gbif.es/service/logger",
    biocache_version = "2.1.15",
    verbose = TRUE,
    download_reason_id = 10,
    caching = "off"
  )
}



#plotea una variable
#########################################
#########################################
plotVariable <- function(brick, variable){

  require(raster)
  require(leaflet)
  require(viridis)

  x <- brick[[variable]]
  x.values <- na.omit(values(x))
  pal <- leaflet::colorNumeric(
    palette = viridis::viridis(100),
    domain = x.values,
    na.color = "transparent"
    )

  leaflet() %>%
    addTiles() %>%
    addRasterImage(
      x,
      colors = pal,
      opacity = 0.5
      ) %>%
    addLegend(
      pal = pal,
      values = x.values,
      title = variable
      )
}


#plotea las presencias y una variable con leaflet
###################################################
###################################################
plotPresencia <- function(brick = NULL, variable = NULL, lon = NULL, lat = NULL, group = NULL){

  require(leaflet)
  require(raster)
  require(viridis)

  #preparando raster para mapa
  x <- brick[[variable]]
  x.values <- na.omit(raster::values(x))
  pal.raster <- colorNumeric(
    palette = viridis::viridis(100),
    domain = x.values,
    na.color = "transparent"
  )

  #preparando paleta para grupos
  if(is.null(group)){
    group = rep(1, length(x))
  }
  pal.groups <- colorFactor(
    palette = viridis::plasma(length(unique(group))),
    domain = unique(group),
    na.color = "transparent"
  )

  #mapa
  leaflet() %>%
    setView(
      lng = mean(lon),
      lat = mean(lat),
      zoom = 04
    ) %>%
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addRasterImage(
      x,
      colors = pal.raster,
      opacity = 0.4
      ) %>%
    addLegend(
      pal = pal.raster,
      values = x.values,
      title = variable,
      opacity = 1
      ) %>%
    addCircles(
      lng = lon,
      lat = lat,
      weight = 10,
      radius = 10,
      color = pal.groups(group),
      stroke = TRUE,
      opacity = 1,
      fillOpacity = 0.2
      ) %>%
    addLegend(
      pal = pal.groups,
      values = unique(group),
      title = "Taxon",
      opacity = 1
    )

  # addMarkers(
  #   lng = lon,
  #   lat = lat,
  #   group = group)

}




#Generate virtual species
##########################################################
##########################################################
#new.name: generateVirtualSpecies
generaEspecieVirtual <- function(variables.brick, nicho, prevalencia){

  #carga librerías
  require(raster)
  require(virtualspecies)
  require(viridis)
  require(ggplot2)
  theme_set(theme_cowplot())
  require(cowplot)

  #variables del nicho
  nicho.variables <- names(nicho)

  #variables brick a tabla
  variables.df <- na.omit(as.data.frame(variables.brick[[nicho.variables]]))

  #error si los nombres de las variables en nicho están mal
  if(sum(names(nicho) %in% names(variables.brick)) != length(nicho.variables)){
    stop("Algún nombre del objecto nicho está mal definido")
  }

  #a long format
  variables.df.long <- tidyr::gather(
    data = variables.df[, nicho.variables],
    key = variable,
    value = valor)

  #crea los nichos para cada variable para visualizarlos
  n <- nrow(variables.df)
  variables.nicho <- list()
  for(i in nicho.variables){
    variables.nicho[[i]] <- rnorm(
      n,
      mean = nicho[[i]][1],
      sd = nicho[[i]][2]
    )
  }
  variables.nicho <- data.frame(do.call("cbind", variables.nicho))


  #a formato largo
  variables.nicho <- tidyr::gather(
    data = variables.nicho,
    key = variable,
    value = valor
  )

  #plotea con ggplot
  plot.densidad <- ggplot(
    data = variables.df.long,
    aes(
      x = valor,
      group = variable
    )
  ) +
    geom_density(
      fill = viridis::viridis(1, begin = 0.99),
      alpha = 1,
      size = 0
    ) +
    facet_wrap("variable", scales = "free") +
    geom_density(
      data = variables.nicho,
      aes(
        x = valor,
        group = variable
      ),
      fill = viridis::viridis(1, begin = 0.3),
      alpha = 0.5,
      size = 0
    )

  #wrapper for virtualspecies::formatFunctions
  args <- list()
  for(i in 1:length(nicho)){
    args[[i]] <- c(fun = "dnorm", mean = nicho[[i]][1], sd = nicho[[i]][2])
  }
  names(args) <- names(nicho)

  #generating the details file for virtualspecies::generateSpFromFun
  details <- list()
  for (i in names(args)){
    details[[i]]$fun <- args[[i]]["fun"]
    args[[i]] <- args[[i]][-(names(args[[i]]) %in% "fun")]
    details[[i]]$args <- as.list(args[[i]])
    details[[i]]$args <- sapply(details[[i]]$args, as.numeric)
  }

  #generating nicho map from details
  virtual.species <- virtualspecies::generateSpFromFun(
    raster.stack = variables.brick[[nicho.variables]],
    parameters = details,
    rescale = TRUE
  )

  #variables brick a tabla
  nicho.map.df <- na.omit(
    as.data.frame(virtual.species$suitab.raster, xy = TRUE)
    )

  #plot mapa
  plot.mapa <- ggplot(data = nicho.map.df,
                      aes(x = x,
                          y = y,
                          fill = layer)
                      ) +
    geom_tile() +
    viridis::scale_fill_viridis(direction = -1) +
    labs(fill = "Suitability") +
    xlab("Longitude") +
    ylab("Latitude")

  #multipanel plot
  plot.nicho <- cowplot::plot_grid(
    plot.densidad,
    plot.mapa,
    ncol = 1,
    axis = "l",
    align = "hv")
  print(plot.nicho)

  #convertir a presencia ausencia
  virtual.species.rango <- virtualspecies::convertToPA(
    x = virtual.species,
    species.prevalence = prevalencia,
    plot = FALSE
  )

  virtual.species.rango$nicho.plot <- plot.nicho

  return(virtual.species.rango)

}

#############################################################################
#thinning
#This function reduces the spatial clustering of a set of presence records. It is intended to reduce spatial autocorrelation, and reduce sampling bias, specially at larger geographical scales.

#It requires two different arguments:
#data.table: a table with two fields representing latitude (named 'y') and longitude (named 'x')

#a min.dist value, provided in the same units as the coordinates, that will define the search radius when looking for pairs of coordinates within search distance to get rid of. Hint: the minimum distance can be extracted from the resolution of a raster containint the environmental factors, like "min.dist<-xres(v.brick.20km)"
thinning = function(xy, brick, separacion){

  #cambia nombres de columnas
  names(xy) <- c("x", "y")

  #calcula distancia mínima entre puntos
  min.dist = raster::xres(brick) * separacion

  #extrae valores de las variables para los puntos de presencia
  xy <- na.omit(
    data.frame(
      xy,
      raster::extract(
        x = brick,
        y = xy,
        df = TRUE
      )
    )
  )

  #remove column
  xy$ID <- NULL

  #genera el dataframe de extremos
  indices.extremos <- vector()
  for(variable in names(xy)){
    indices.extremos <- c(indices.extremos, which.min(xy[, variable]), which.max(xy[, variable]))
  }

  #quita repetidos
  indices.extremos <- unique(indices.extremos)

  #genera el dataframe xy.extremos
  xy.extremos <- xy[indices.extremos, ]

  #los elimina de xy
  xy <- xy[-indices.extremos, ]

  #count rows
  row<-1

  #repite la operación hasta que se cumple la condición de salida
  repeat{

    #contenido de la fila (para no tirar de toda la tabla en todas las operaciones)
    f<-xy[row, ]

    #genera los límites de la cuadrícula de búsqueda
    ymax<-f$y + min.dist
    ymin<-f$y - min.dist
    xmax<-f$x + min.dist
    xmin<-f$x - min.dist

    #selecciona de la tabla los datos con coordenadas dentro del rectángulo que no tienen las mismas coordenadas que la fila con la que estamos trabajando, y las elimina de la tabla
    xy <- xy[!((xy$y <= ymax) & (xy$y >= ymin) & (xy$x <= xmax) & (xy$x >= xmin) & (xy$y != f$y | xy$x != f$x)), ]

    #estima de filas por procesar
    print(paste("Processed rows: ", row, " out of ", nrow(xy), sep=""))

    #suma 1 al contador de la fila
    row<-row+1

    #condición de salida cuando llega a la última fila
    if(row>=nrow(xy))break
  }

  #añadiendo extremos
  xy <- rbind(xy.extremos, xy)[, c("x", "y")]

  return(xy)

}


#############################################################
#CALCULA BISERIAL CORRELATION PARA UNA TABLA DE PRESENCIAS
#presencias: dataframe con presencias y ausencias para un conjunto de variables
#presencia: nombre de la columna de presencia
#variables: nombres de las variables a analizar
biserialCorrelation <- function(presencias, presencia, variables){

  require(tidyr)
  require(ggplot2)
  require(viridis)

  #a formato long
  presencias.long <- tidyr::gather(
    data = presencias,
    key = variable,
    value = valor,
    variables
  )

  #presencia a factor
  presencias.long[, presencia] <- factor(presencias.long[, presencia])

  #plotea primero
  biserial.plot <- ggplot(
    data = presencias.long,
    aes_string(
      x = get(presencia),
      y = "valor",
      group = "variable",
      color = get(presencia)
    )
  ) +
    geom_point(
      alpha = 0.05,
      size = 3
    ) +
    facet_wrap("variable", scales = "free") +
    viridis::scale_color_viridis(
      discrete = TRUE,
      direction = -1
    ) +
    geom_smooth(
      method = "lm",
      size = 2,
      color = viridis::viridis(1, begin = 0.5)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    ylab("Variable") +
    xlab("Presencia")

  #plot a pantalla
  print(biserial.plot)

  #calcula biserial correlation para cada variable
  biserial.correlation <- data.frame(
    variable = variables,
    R2 = NA,
    p = NA,
    stringsAsFactors = FALSE
  )

  #itera por todas las variables
  for(variable in biserial.correlation$variable){

    #calcula correlacion
    temp.cor <- cor.test(presencias[, presencia], presencias[, variable])

    #guarda resultado
    biserial.correlation[
      biserial.correlation$variable == variable ,
      c("R2", "p")
      ] <- c(abs(temp.cor$estimate), round(temp.cor$p.value, 3))

  }

  #ordena por R2
  biserial.correlation <- biserial.correlation[
    order(
      biserial.correlation$R2,
      decreasing = TRUE
    ), ]

  #nuevos nombres de fila
  row.names(biserial.correlation) <- 1:nrow(biserial.correlation)

  #a pantalla
  print(biserial.correlation)

  #lista de resultados
  output.list <- list()
  output.list$plot <- biserial.plot
  output.list$df <- biserial.correlation

  return(output.list)

}


###################################################################
#PLOTEA DENSIDADES DE PRESENCIAS Y AUSENCIAS
#presencias: dataframe con presencias, ausencias, y sus valores de las variables
#presencia: nombre de la columna de presencia
#variables: nombres de las columnas de variables
plotUsoDisponibilidad <- function(presencias, presencia, variables){

  require(tidyr)
  require(ggplot2)
  require(viridis)

  #a formato long
  temp.long <- tidyr::gather(
    data = presencias,
    key = variable,
    value = valor,
    variables
  )

  #presencia a factor
  temp.long[, presencia] <- factor(temp.long[, presencia])

  #plotea con ggplot
  curvas.respuesta <- ggplot(
    data = temp.long,
    aes_string(
      x = "valor",
      group = get(presencia),
      fill = get(presencia)
    )
  ) +
    geom_density(
      alpha = 0.5,
      size = 0.2
    ) +
    facet_wrap("variable", scales = "free") +
    viridis::scale_fill_viridis(
      discrete = TRUE,
      direction = -1
    )
  x11(width = 20, height = 15, pointsize = 20)
  print(curvas.respuesta)

  return(curvas.respuesta)

}


######################################################################
#DENDROGRAMA DE CORRELACIÓN
#variables.df: dataframe de las variables sin valores nulos originado de un brick
#biserial.correlation: dataframe resultante de la función biserialCorrelation
#cor.threshold: umbral de correlación para separar grupos
dendrogramaCorrelacion <- function(variables.df, biserial.correlation, seleccion.automatica = FALSE, correlacion.maxima = 0.5){

  require(dplyr)
  require(magrittr)
  require(ggplot2)
  require(ggdendro)

  #obtiene variables con biserial.correlation significativa
  variables.seleccionadas <- biserial.correlation[biserial.correlation$p < 0.05, "variable"]

  #1: dendrograma de correlación de las variables seleccionadas
  #############################################################
  #calcula la matriz de correlación con cor
  temp.cor <- cor(variables.df[, variables.seleccionadas])

  #lo converti en matriz de distancias (menor distancia = mayor correlación)
  temp.dist <- as.dist(abs(temp.cor))

  #cluster (1 - correlación) para convertir correlación en distancia
  temp.cluster <- hclust(1 - temp.dist)


  #2: seleccion automatica
  ##############################################################
  if(seleccion.automatica == TRUE){

    #tabla de grupos
    temp.cluster.groups <- data.frame(group = cutree(temp.cluster, h = 1 - correlacion.maxima))
    temp.cluster.groups$variable <- row.names(temp.cluster.groups)
    temp.cluster.groups <- temp.cluster.groups[
      order(
        temp.cluster.groups$group,
        decreasing = FALSE
      ), ]
    row.names(temp.cluster.groups) <- 1:nrow(temp.cluster.groups)

    #le une biserial correlation
    #añade los valores de biserial correlation a las etiquetas del cluster
    temp.cluster.groups$R2 <- biserial.correlation$R2[
      match(
        temp.cluster.groups$variable,     #etiquetas cluster
        biserial.correlation$variable #variables biserial.correlation
      )
      ]

    #se queda con el máximo de cada grupo
    variables.seleccionadas <- temp.cluster.groups %>%
      dplyr::group_by(group) %>%
      dplyr::slice(which.max(R2)) %>%
      .$variable

  } #fin de selección automática


  #3: plotea cluster
  ##############################################################
  #datos requeridos para plotear el cluster
  temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

  #añade los valores de biserial correlation a las etiquetas del cluster
  temp.cluster.data$labels$R2 <- biserial.correlation$R2[
    match(
      temp.cluster.data$labels$label,     #etiquetas cluster
      biserial.correlation$variable #variables biserial.correlation
    )
    ]

  #extrae etiquetas
  labs <- ggdendro::label(temp.cluster.data)

  #añade señal a la etiqueta si la variable está seleccionada
  if(seleccion.automatica == TRUE){
    labs$label <- as.character(labs$label)
    for(i in 1:nrow(labs)){
      if(labs[i, "label"] %in% variables.seleccionadas){
        labs[i, "label"] <- paste("→ ", labs[i, "label"], sep = "")
      }
    }
  }
  labs$label <- factor(labs$label)


  #plotea dendrograma
  cluster.plot <- ggplot(ggdendro::segment(temp.cluster.data)) +
    geom_segment(
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend)
    ) +
    geom_text(
      data = ggdendro::label(temp.cluster.data),
      aes(
        label = labs$label,
        x = x,
        y = 0,
        colour = labs$R2,
        hjust = 1
      ),
      size = 10
    ) +
    coord_flip(ylim = c(-0.2, 1)) +
    viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
    theme(
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(2,2,2,2), "lines"),
      axis.text.x = element_text(size = 20),
      legend.position = "left"
    ) +
    labs(colour = "R2") +
    geom_hline(
      yintercept = 1 - correlacion.maxima,
      col = "red4",
      linetype = "dashed",
      size = 2,
      alpha = 0.5
    ) +
    scale_y_continuous(breaks = c(1 - correlacion.maxima, 0, 0.25, 0.5, 0.75, 1)) +
    ylab("1 - correlación")

  x11(width = 15, height = 10, pointsize = 20)
  print(cluster.plot)

  #lista de salida
  if(seleccion.automatica == TRUE){
    output.list <- list()
    output.list$variables.dendrograma <- cluster.plot
    output.list$variables.seleccionas <- variables.seleccionadas
    return(output.list)
  } else {
    return(cluster.plot)
  }

}
