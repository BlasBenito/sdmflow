#' Automatic selection of predictive variables for species distribution modeling
#'
#' @description Applies \code{\link{s_biserial_cor}}, \code{\link{s_cor}} (with correlation threshold set to 0.5), and \code{\link{s_auto_vif}} to automatically select a set of non-correlated variables with the higher biserial correlation as possible.
#'
#' @usage s_auto(
#'   training.df,
#'   response.col = "presence",
#'   select.cols = NULL,
#'   omit.cols = NULL,
#'   plot = TRUE,
#'   text.size = 6
#')
#'
#'
#' @param training.df A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param response.col Character, name of the presence column.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{response.col} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis.
#' @param plot Boolean, if \code{TRUE}, prints last correlation dendrogram to test the final output.
#' @param text.size Numeric, size of the dendrogram labels.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#' data(virtualSpeciesPB)
#' selected.vars <- s_auto(
#'   training.df = virtualSpeciesPB,
#'   response.col = "presence",
#'   omit.cols = c("x", "y")
#' )
#' selected.vars
#' HH::vif(virtualSpeciesPB[, selected.vars])
#' cor(virtualSpeciesPB[, selected.vars])
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
  plot = TRUE,
  text.size = 6
  ){

  #computes biserial correlation
  bis.cor <- s_biserial_cor(
    training.df = training.df,
    response.col = response.col,
    select.cols = select.cols,
    omit.cols = omit.cols,
    plot = FALSE
    )

  #completes exclude variables
  omit.cols <- c(omit.cols, response.col)

  #gets selected variables
  old.selected.variables <- bis.cor$df[bis.cor$df$p < 0.05, "variable"]

  #selectes variables by their bivariate correlation
  new.selected.variables <- s_cor_auto(
    training.df = training.df,
    select.cols = old.selected.variables,
    omit.cols = omit.cols,
    max.cor = 0.50,
    bis.cor = bis.cor,
    plot = FALSE
  )

  #generates try.to.keep vector
  try.to.keep <- bis.cor$df[bis.cor$df$variable %in% new.selected.variables, ]$variable
  if(length(try.to.keep) == 0){
    try.to.keep <- bis.cor$df$variable
  }

  #autovif
  selected.variables <- s_vif_auto(
    training.df = training.df[, new.selected.variables],
    select.cols = try.to.keep,
    verbose = FALSE
  )

  #final plot
  if(plot == TRUE){
    s_cor(
      training.df = training.df[, selected.variables],
      bis.cor = bis.cor,
      plot = TRUE,
      text.size = text.size
    )
  }

  #return output
  return(selected.variables)

}
