#' Plots density functions of use and availability from a training data frame
#'
#' @description Plots the relative densities of ones (presence, a.k.a, "use") versus zeroes (background, a.k.a, "availability") for each environmental predictor in a training data frame. This plot helps to understand the relationship between use and availability in order to make informed decisions during variable selection. When for a given variable the density of use peaks over low availability it indicates that the species selects those values of a variable at a higher rate than what is expected by chance. On the other hand, variables with a very high overlap between use and availability will likely turn out to have a low predictive value during SDM fitting.
#'
#' @usage s_plot_density(
#'   training.df,
#'   response.col = "presence",
#'   select.cols = NULL,
#'   omit.cols = c("x", "y"),
#'   axis.text.size = 6,
#'   legend.text.size = 12,
#'   strip.text.size = 10
#' )
#'
#' @param training.df A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param response.col Character, name of the response variable. Usually a presence-background column with ones and zeroes.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{response.col} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis.
#' @param axis.text.size Numeric, size of the axis labels.
#' @param legend.text.size Numeric, size of the legend labels.
#' @param strip.text.size Numeric, size of the panel names.
#'
#' @return A ggplot object.
#'
#' @examples
#'data("virtual.species.training")
#'x <- s_plot_density(
#'  x = virtual.species.training,
#'  response.col = "presence",
#'  select.cols = NULL,
#'  omit.cols = c("x", "y")
#')
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
s_plot_density <- function(
  training.df,
  response.col = "presence",
  select.cols = NULL,
  omit.cols = c("x", "y"),
  axis.text.size = 6,
  legend.text.size = 12,
  strip.text.size = 10
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
  training.df.long <-
    training.df %>%
    tidyr::pivot_longer(
      cols = select.cols,
      names_to = "variable",
      values_to = "value"
    ) %>%
    data.frame() %>%
    dplyr::rename(presence = 1)

  #presence to factor for easier plotting
  training.df.long[training.df.long[, response.col] == 1, response.col] <- "use"
  training.df.long[training.df.long[, response.col] == 0, response.col] <- "availability"
  training.df.long[, response.col] <- factor(
    x = training.df.long[, response.col],
    levels = c("use", "availability")
    )

  #plotea con ggplot
  plot.use.availability <- ggplot2::ggplot(
    data = training.df.long,
    aes(
      x = value,
      group = presence,
      fill = presence
    )
  ) +
    ggplot2::geom_density(
      alpha = 0.5,
      size = 0.2,
      aes(y = ..scaled..)
    ) +
    ggplot2::facet_wrap("variable", scales = "free") +
    viridis::scale_fill_viridis(
      discrete = TRUE,
      direction = 1
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text = element_text(size = axis.text.size),
      legend.text = element_text(size = legend.text.size),
      strip.text = element_text(size = strip.text.size)
      ) +
    labs(fill="")

  #building response curves EXPERIMENTAL

  #getting data
  # density.data <- ggplot_build(plot.use.availability)
  # density.data <- density.data$data[[1]]

  print(plot.use.availability)

  return(plot.use.availability)
}
