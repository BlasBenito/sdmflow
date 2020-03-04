#' Computes weights for presence and background records
#'
#' @description Presence and background samples are highly unbalanced, generally by more than one order of magnitude. Many functions such as \code{\link[mgcv]{gam}} or \code{\link[ranger]{ranger}} have an argument named \code{weights} or similar that helps to work around the sample unbalance. Such an argument requires a numeric vector as input, with one position per record in the data used to fit the model. Each value in the weights vector represents the relative weight of a single case, and all the values in the weights vector get to sum 1. For example, if a presence column is \code{1, 0, 0}, its relative weights would be \code{0.5, 0.25, 0.25}. The function takes as input an ordered numeric vector of ones and zeroes representing the presence and background data of an input dataset to fit SDMs.
#'
#' @usage m_weights(x)
#'
#' @param x Ordered numeric vector with ones and zeroes respectively representing presence and background records. This will generally be the "presence" column of a data frame to be used to fit a species distribution model.
#'
#' @return A numeric vector with the weights of the ones and zeroes available in the input vector.
#'
#' @examples
#' data(virtual.species.training)
#' weights <- m_weights(x = virtual.species.training$presence)
#' table(weights)
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
m_weights <- function(x){

  #check that it is not a factor
  if(is.factor(x) == TRUE){
    x <- as.numeric(levels(x))[x]
  }

  #coerce to numeric
  if(is.numeric(x) == FALSE){
    x <- as.integer(x)
  }

  #check that there are only 0s and 1s
  if((sum(x == 1) + sum(x == 0)) != length(x)){
    stop("Argument x must have values 1 and 0 only.")
  }

  #checks if it is ordered (ones must go first)
  if(sum(rev(sort(x)) == x) != length(x)){
    stop("Argument x does not seem to be sorted. Ones must go before zeroes, as in 1, 1, 1, 0, 0, 0, ...")
  }

  #weight presences
  n.presences <- sum(x)
  weight.presences <- 1/n.presences

  #weight background
  n.background <- length(x) - n.presences
  weight.background <- 1/n.background

  #generates weights
  weights <- c(rep(weight.presences, n.presences), rep(weight.background, n.background))

  return(weights)

}
