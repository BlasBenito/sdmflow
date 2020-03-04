#' Automatic selection of predictive variables for species distribution modeling
#'
#' @description Applies \code{\link{s_biserial_cor}}, \code{\link{s_vif_auto}}, and \code{\link{s_cor_auto}} to automatically select a set of non-correlated variables with a biserial correlation as high as possible.
#'
#' @usage s_auto(
#'   training.df,
#'   response.col = "presence",
#'   select.cols = NULL,
#'   omit.cols = c("x", "y"),
#'   max.cor = 0.5,
#'   plot = TRUE,
#'   text.size = 6
#')
#'
#'
#' @param training.df A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param response.col Character, name of the presence column.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{response.col} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis.
#' @param max.cor Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables. Defaults to 0.5.
#' @param plot Boolean, if \code{TRUE}, prints last correlation dendrogram to test the final output.
#' @param text.size Numeric, size of the dendrogram labels.
#'
#' @return An object of class \code{s_auto} with three named slots. The slot \code{plot} contains a correlation dendrogram produced by \code{\link{s_cor}}. The slot \code{df} contains the VIF data frame of the selected variables produced by \code{\link{s_vif_auto}}. The slot \code{vars} is a character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#' data(virtual.species.training)
#' selected.vars <- s_auto(
#'   training.df = virtual.species.training,
#'   response.col = "presence",
#'   omit.cols = c("x", "y")
#' )
#' selected.vars
#'
#' HH::vif(virtual.species.training[, selected.vars])
#' cor(virtual.species.training[, selected.vars])
#'
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
s_auto <- function(
  training.df,
  response.col = "presence",
  select.cols = NULL,
  omit.cols = c("x", "y"),
  max.cor = 0.5,
  plot = TRUE,
  text.size = 6
  ){

  #computes biserial correlation
  biserial.cor <- s_biserial_cor(
    training.df = training.df,
    response.col = response.col,
    select.cols = select.cols,
    omit.cols = omit.cols,
    plot = FALSE
    )

  #completes exclude variables
  omit.cols <- c(omit.cols, response.col)

  #selectes variables by their bivariate correlation
  cor.auto <- s_cor_auto(
    training.df = training.df,
    select.cols = select.cols,
    omit.cols = omit.cols,
    max.cor = 0.5,
    biserial.cor = biserial.cor,
    plot = FALSE
  )

  #subsets biserial.cor
  biserial.cor$df <- biserial.cor$df[biserial.cor$df$variable %in% cor.auto$vars, ]

  #applies s_vif_auto
  vif.auto <- s_vif_auto(
    training.df = training.df,
    select.cols = select.cols,
    omit.cols = omit.cols,
    preference.order = NULL,
    biserial.cor = biserial.cor,
    verbose = FALSE
  )

  #subsets biserial.cor
  biserial.cor$df <- biserial.cor$df[biserial.cor$df$variable %in% vif.auto$vars, ]

  #final plot
  if(plot == TRUE){
    s.cor.plot <- s_cor(
      training.df = training.df[, vif.auto$vars],
      plot = TRUE,
      text.size = text.size
    )
  }

  #output
  output.list <- list()
  output.list$plot <- s.cor.plot$plot
  output.list$df <- vif.auto$df
  output.list$vars <- vif.auto$vars
  class(output.list) <- c("list", "s_auto")

  #return output
  return(output.list)

}
