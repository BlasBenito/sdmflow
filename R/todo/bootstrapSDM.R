
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
