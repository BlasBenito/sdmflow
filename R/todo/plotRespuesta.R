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
