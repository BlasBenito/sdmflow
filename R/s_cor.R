#' Correlation dendrogram to help reduce multicollinearity in a training dataset.
#'
#' @description Computes the correlation between all pairs of variables in a training dataset. If a \code{\link{s_biscor}} output is provided, it further selects variables automatically based on the R-squared value obtained by each variable in the biserial correlation analysis.
#'
#' @usage s_cor(
#'   training.df,
#'   select.cols = NULL,
#'   omit.cols = c("x", "y", "presence"),
#'   maximum.cor = 0.5,
#'   biserial.cor = NULL,
#'   plot = TRUE,
#'   text.size = 6
#'   )
#'
#'
#' @param training.df A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param maximum.cor Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables.
#' @param biserial.cor List, output of the function \code{\link{s_biserial_cor}}. Its R-squared scores are used to select variables.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#' @param text.size Numeric, size of the dendrogram labels.
#'
#' @return If \code{biserial.cor} is not NULL, a list with two slots named "dendrogram" (a ggplot2 object) and "selected.variables" with the dendrogram and the character vector with the selected variables. Otherwise it only returns the dendrogram, and the users have to select the variables by themselves.
#'
#' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'
#'biserial.cor <- s_biscor(
#'  training.df = virtualSpeciesPB,
#'  omit.cols = c("x", "y")
#')
#'
#'selected.vars <- s_cor(
#'  training.df = virtualSpeciesPB,
#'  select.cols = NULL,
#'  omit.cols = c("x", "y", "presence"),
#'  max.cor = 0.5,
#'  biserial.cor = biserial.cor
#')$selected.variables
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
s_cor <- function(
  training.df,
  select.cols = NULL,
  omit.cols = c("x", "y", "presence"),
  max.cor = 0.5,
  biserial.cor = NULL,
  plot = TRUE,
  text.size = 6
  ){

  #preparing output list
  output.list <- list()

  #keeping numeric columns only and removing NA
  training.df <-
    training.df[, unlist(lapply(training.df, is.numeric))] %>%
    na.omit()

  #getting variables
  if(is.null(select.cols) == TRUE){
    select.cols <- colnames(training.df)
  }
  if(is.null(omit.cols) == FALSE){
    if(sum(omit.cols %in% select.cols) == length(omit.cols)){
      select.cols <- select.cols[!(select.cols %in% omit.cols)]
    }
  }

  #subsetting training.df
  if(sum(select.cols %in% colnames(training.df)) == length(select.cols)){
    training.df <- training.df[, select.cols]
  } else {
    stop("variables must be column names of training.df.")
  }

  #computes correlation matrix
  cor.matrix <-
    training.df %>%
    cor() %>%
    abs() %>%
    as.dist()

  #cluster (converts correlation to distance)
  temp.cluster <- hclust(1 - cor.matrix)

  #if biserial.cor == NULL
  #-------------------------------------
  if(is.null(biserial.cor) == TRUE){

    #generates cluster data
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

    #plots cluster
      cluster.plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = ggdendro::segment(temp.cluster.data),
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend)
        ) +
        ggplot2::geom_text(
          data = ggdendro::label(temp.cluster.data),
          aes(
            label = label,
            x = x,
            y = 0,
            hjust = 1
          ),
          size = text.size
        ) +
        ggplot2::coord_flip(ylim = c(-0.4, 1)) +
        viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
        ggplot2::theme(
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(2,2,2,2), "lines"),
          axis.text.x = element_text(size = text.size * 2),
          legend.position = "bottom",
          legend.key.width = unit(2, "lines")
        ) +
        ggplot2::labs(colour = "R2") +
        ggplot2::geom_hline(
          yintercept = 1 - max.cor,
          col = "red4",
          linetype = "dashed",
          size = 1,
          alpha = 0.5
        ) +
        ggplot2::scale_y_continuous(breaks = c(1 - max.cor, 0, 0.25, 0.5, 0.75, 1)) +
        ggplot2::ylab("1 - correlation")

      if(plot == TRUE){
        ggplot2::theme_set(cowplot::theme_cowplot())
        print(cluster.plot)
        }

      #prepare output
      selected.variables <- colnames(training.df)

  } else {

    #gets only significnant variables from biserial correlation ouotput
    selected.variables <- biserial.cor$df[biserial.cor$df$p < 0.05, "variable"]

    #table of groups
    temp.cluster.groups <- data.frame(group = cutree(temp.cluster, h = 1 - max.cor))
    temp.cluster.groups$variable <- row.names(temp.cluster.groups)
    temp.cluster.groups <- temp.cluster.groups[
      order(
        temp.cluster.groups$group,
        decreasing = FALSE
      ), ]
    row.names(temp.cluster.groups) <- 1:nrow(temp.cluster.groups)

    #adds biserial correlation to cluster labels
    temp.cluster.groups$R2 <- biserial.cor$df[
      match(
        temp.cluster.groups$variable,     #cluster labels
        biserial.cor$df$variable #variables in biserial correlation output
      ), "R2"
      ]

    #gets the maximum of each group
    selected.variables <-
      temp.cluster.groups %>%
      dplyr::group_by(group) %>%
      dplyr::slice(which.max(R2)) %>%
      .$variable

    #prepares cluster plotting
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

    #gets R2
    temp.cluster.data$labels$R2 <- biserial.cor$df[
      match(
        temp.cluster.data$labels$label,     #etiquetas cluster
        biserial.cor$df$variable #variables biserialCorrelation.output
      ), "R2"
      ]

    #gets labels
    labs <- ggdendro::label(temp.cluster.data)

    #adds arrow to label if the variable is selected
    labs$label <- as.character(labs$label)
    for(i in 1:nrow(labs)){
      if(labs[i, "label"] %in% selected.variables){
        labs[i, "label"] <- paste("\u{2192} ", labs[i, "label"], sep = "")
      }
    }
    labs$label <- factor(labs$label)


    #plots dendrogram
      cluster.plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = ggdendro::segment(temp.cluster.data),
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend)
        ) +
        ggplot2::geom_text(
          data = ggdendro::label(temp.cluster.data),
          aes(
            label = labs$label,
            x = x,
            y = 0,
            colour = labs$R2,
            hjust = 1
          ),
          size = text.size
        ) +
        ggplot2::coord_flip(ylim = c(-0.4, 1)) +
        viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
        ggplot2::theme(
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(2,2,2,2), "lines"),
          axis.text.x = element_text(size = text.size * 2),
          legend.position = "bottom",
          legend.key.width = unit(2, "lines")
        ) +
        ggplot2::labs(colour = "Biserial correlation") +
        ggplot2::geom_hline(
          yintercept = 1 - max.cor,
          col = "red4",
          linetype = "dashed",
          size = 1,
          alpha = 0.5
        ) +
        ggplot2::scale_y_continuous(breaks = c(1 - max.cor, 0, 0.25, 0.5, 0.75, 1)) +
        ggplot2::ylab("1 - correlation")

      if(plot == TRUE){
        ggplot2::theme_set(cowplot::theme_cowplot())
        print(cluster.plot)
        }

  }

  #preparing output
  output.list$plot <- cluster.plot
  output.list$vars <- selected.variables
  return(output.list)

}
