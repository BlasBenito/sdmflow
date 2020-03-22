#' Biserial correlation analysis of presence and background data for variable selection
#'
#' @description Computes the biserial correlation between presence and background data for a set of predictors. For each numeric variable in \code{training.df} seleccted by the user, the weighted linear model  \code{lm(response.col ~ variable, data = training.df, weights = w)} is fitted, where \code{w} is computed with the function \code{\link{m_weights}}. The adjusted R-squared (a.k.a "biserial correlation") and p-values are extracted for each variable, and returned as a data frame. A high biserial correlation for a given predictor indicates that the distributions of the presence and background records are separated enough in the space of the predictor values to suggest that the predictor might be a good candidate variable to fit a species distribution model.
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
#' @param response.col Character, name of the response variable. Usually a presence-background column with ones and zeroes.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{response.col} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis.
#' @param axis.text.size Numeric, size of the axis labels.
#' @param legend.text.size Numeric, size of the legend labels.
#' @param strip.text.size Numeric, size of the panel names.
#' @param point.size Size of points in the biserial correlation plot.
#' @param line.size Line width in the biserial correlation plot.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}. Take in mind that plotting thousands of points per variable in \code{training.df} might take some time.
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
#' data(virtual.species.training)
#' biserial.cor <- s_biserial_cor(
#'   training.df = virtual.species.training,
#'   response.col = "presence",
#'   select.cols = c("bio1", "bio5", "bio6"),
#'   plot = FALSE
#' )
#' biserial.cor
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


  #dropping omit.cols
  if(sum(omit.cols %in% colnames(training.df)) == length(omit.cols)){
    training.df <-
      training.df %>%
      dplyr::select(-tidyselect::all_of(omit.cols))
  }

  #selecting select.cols
  if(is.null(select.cols) == FALSE){
    if(sum(select.cols %in% colnames(training.df)) == length(select.cols)){
      training.df <-
        training.df %>%
        dplyr::select(tidyselect::all_of(select.cols))
    }
  }

  #getting numeric columns only and removing cases with NA
  training.df <-
    training.df[, unlist(lapply(training.df, is.numeric))] %>%
    na.omit()

  #getting select cols
  select.cols <- colnames(training.df)[!(colnames(training.df) %in% response.col)]

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

  #computes weights
  w <- m_weights(training.df[, response.col])

  #iterates through variables
  for(variable in select.cols){

    #computes correlation
    # temp.cor <- cor.test(
    #   training.df[, response.col],
    #   training.df[, variable]
    #   )

    temp.cor <- lm(
      formula = as.formula(paste(response.col, "~", variable)),
      data = training.df,
      weights = w
      ) %>%
      summary()

    #getting R2
    R2 <- temp.cor$adj.r.squared
    R2 <- round(R2, 4)

    #getting p-value
    f <- temp.cor$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    p <- round(p, 4)

    #stores outcome
    biserial.correlation[
      biserial.correlation$variable == variable ,
      c("R2", "p")
      ] <- c(R2, p)

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
  class(output.list) <- c("list", "s_biserial_cor")

  return(output.list)

}
