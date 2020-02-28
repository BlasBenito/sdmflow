#' Reduces multicollinearity automatically through repeated application of \code{\link{s_cor}}.
#'
#' @description Computes the correlation between all pairs of variables in a training dataset and removes variables until there are no more variables correlated above a given threshold (0.5 Pearson correlation by default). This function requires a \code{\link{s_biserial_cor}} output, and it won't run if the argument remains \code{NULL}.
#'
#' @usage s_cor_auto(
#' training.df,
#' select.cols = NULL,
#' omit.cols = c("x", "y", "presence"),
#' max.cor = 0.5,
#' biserial.cor = NULL,
#' plot = TRUE,
#' text.size = 6
#' )
#'
#'
#' @param training.df A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param max.cor Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables. Defaults to 0.5.
#' @param biserial.cor List, output of the function \code{\link{s_biserial_cor}}. Its R-squared scores are used to select variables.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#' @param text.size Numeric, size of the dendrogram labels.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'
#'biserial.cor <- s_biserial_cor(
#'  training.df = virtualSpeciesPB,
#'  omit.cols = c("x", "y")
#')
#'
#'selected.vars <- s_cor_auto(
#'  training.df = virtualSpeciesPB,
#'  omit.cols = c("x", "y", "presence"),
#'  max.cor = 0.5,
#'  biserial.cor = biserial.cor
#')$selected.variables
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
s_cor_auto <- function(
    training.df,
    select.cols = NULL,
    omit.cols = c("x", "y", "presence"),
    max.cor = 0.5,
    biserial.cor = NULL,
    plot = TRUE,
    text.size = 6
  ){

  #checks that there is a biserial correlation output
  if(is.null(biserial.cor) == TRUE){
    stop("The argument biserial.cor is empty.")
  }

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

  #subsetting x
  if(sum(select.cols %in% colnames(training.df)) == length(select.cols)){
    training.df <- training.df[, select.cols]
  } else {
    stop("variables must be column names of training.df.")
  }

  #gets selected variables
  old.selected.variables <- biserial.cor$df[biserial.cor$df$p < 0.05, "variable"]

  #selects variables
  repeat{

    #computes bivariate correlation
    new.selected.variables <- s_cor(
      training.df = training.df,
      select.cols = old.selected.variables,
      omit.cols = omit.cols,
      max.cor = 0.50,
      biserial.cor = biserial.cor,
      plot = FALSE
    )$vars

    if(length(old.selected.variables) == length(new.selected.variables)){
      break
    } else {
      old.selected.variables <- new.selected.variables
    }

  } #end of repeat

  #final plot
  if(plot == TRUE){
    output.list <- s_cor(
      training.df = training.df[, new.selected.variables],
      max.cor = max.cor,
      plot = TRUE,
      text.size = text.size
    )
  } else {
    output.list <- s_cor(
      training.df = training.df[, new.selected.variables],
      max.cor = max.cor,
      plot = FALSE,
      text.size = text.size
    )
  }

  return(output.list)
}
