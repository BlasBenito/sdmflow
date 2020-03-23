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
