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

