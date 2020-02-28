#' Biserial correlation analysis of presence and background data for variable selection
#'
#' @description Computes the biserial correlation between presence and background data for a set of predictors. A high biserial correlation for a given predictor indicates that the distributions of the presence and background records are separated enough in the space of predictor values to suggest that the predictor is a good candidate for a species distribution model.
#'
#' @usage s_biserial_cor(
#'   training.df,
#'   response.col = "presence",
#'   select.cols = NULL,
#'   omit.cols = c("x", "y"),
#'   axis.text.size = 6,
#'   legend.text.size = 12,
#'   strip.text.size = 10,
#'   point.size = 1,
#'   line.size = 1,
#'   plot = TRUE
#')
#'
#' @param training.df A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param response.col Character, name of the presence column.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{response.col} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis.
#' @param axis.text.size Numeric, size of the axis labels.
#' @param legend.text.size Numeric, size of the legend labels.
#' @param strip.text.size Numeric, size of the panel names.
#' @param point.size Size of points in the biserial correlation plot.
#' @param line.size Line width in the biserial correlation plot.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#'
#' @return A named list with two slots named \code{plot} and \code{df}. The former contains a ggplot object with the biserial correlation analysis. The latter is a data frame with the following columns:
#' \itemize{
#'   \emph{variable}: Name of the predictive variable.
#'   \emph{R2}: R-squared of the biserial correlation.
#'   \emph{p}: p-value of the correlation analysis.
#' }
#' The output data frame is ordered, starting with the higher R2 values.
#'
#' @examples
#' data(virtualSpeciesPB)
#' cPB <- s_biserial_cor(
#'   training.df = virtualSpeciesPB,
#'   response.col = "presence",
#'   select.cols = c("bio1", "bio5", "bio6")
#' )
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
s_biserial_cor <- function(
  training.df,
  response.col = "presence",
  select.cols = NULL,
  omit.cols = c("x", "y"),
  axis.text.size = 6,
  legend.text.size = 12,
  strip.text.size = 10,
  point.size = 1,
  line.size = 1,
  plot = TRUE
  ){

  #keeping numeric columns only and removing NA
  training.df <-
    training.df[, unlist(lapply(training.df, is.numeric))] %>%
    na.omit()

  #getting variables
  if(is.null(select.cols) == TRUE){
    select.cols <- colnames(training.df)[colnames(training.df) != response.col]
  }
  if(is.null(omit.cols) == FALSE){
    select.cols <- select.cols[!(select.cols %in% omit.cols)]
  }

  #subsetting x
  training.df <- training.df[, c(response.col, select.cols)]

  #to long format
  x.long <-
    training.df %>%
    tidyr::pivot_longer(
    cols = tidyselect::all_of(select.cols),
    names_to = "variable",
    values_to = "value"
  ) %>%
    data.frame() %>%
    dplyr::rename(presence = 1)

  #presence to factor for easier plotting
  x.long[, response.col] <- factor(x.long[, response.col])

  #plotea primero
  biserial.plot <- ggplot2::ggplot(
    data = x.long,
    aes(
      x = presence,
      y = value,
      group = variable,
      color = presence
    )
  ) +
    ggplot2::geom_point(
      alpha = 0.05,
      size = point.size
    ) +
    ggplot2::facet_wrap("variable", scales = "free") +
    viridis::scale_color_viridis(
      discrete = TRUE,
      direction = -1
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      size = line.size,
      color = viridis::viridis(1, begin = 0.5)) +
    ggplot2::guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::ylab("Variable") +
    ggplot2::xlab("Presence") +
    ggplot2::theme(legend.position = "bottom") +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = axis.text.size),
      legend.text = element_text(size = legend.text.size),
      strip.text = element_text(size = strip.text.size)
    )

  #prints plot to screen
  if(plot == TRUE){
    print(biserial.plot)
  }

  #dataframe to store results
  biserial.correlation <- data.frame(
    variable = select.cols,
    R2 = NA,
    p = NA,
    stringsAsFactors = FALSE
  )

  #iterates through variables
  for(variable in select.cols){

    #computes correlation
    temp.cor <- cor.test(
      training.df[, response.col],
      training.df[, variable]
      )

    #stores outcome
    biserial.correlation[
      biserial.correlation$variable == variable ,
      c("R2", "p")
      ] <- c(abs(temp.cor$estimate), round(temp.cor$p.value, 4))

  }

  #orders by R2
  biserial.correlation <-
    biserial.correlation %>%
    dplyr::arrange(dplyr::desc(R2))

  #resets rownames
  row.names(biserial.correlation) <- 1:nrow(biserial.correlation)

  #lista de resultados
  output.list <- list()
  output.list$plot <- biserial.plot
  output.list$df <- biserial.correlation

  return(output.list)

}
