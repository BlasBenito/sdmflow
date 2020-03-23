#############################################################################
#http://modtools.wordpress.com/2013/08/14/dsquared/
# Linear models come with an R-squared value that measures the proportion of variation that the model accounts for. The R-squared is provided with summary(model) in R. For generalized linear models (GLMs), the equivalent is the amount of deviance accounted for (D-squared; Guisan & Zimmermann 2000), but this value is not normally provided with the model summary. The Dsquared function, now included in the modEvA package (Barbosa et al. 2014), calculates it. There is also an option to calculate the adjusted D-squared, which takes into account the number of observations and the number of predictors, thus allowing direct comparison among different models (Weisberg 1980, Guisan & Zimmermann 2000).
Dsquared <- function(model, adjust = TRUE) {
  # version 1.1 (13 Aug 2013)
  # calculates the explained deviance of a GLM
  # model: a model object of class "glm"
  # adjust: logical, whether or not to use the adjusted deviance taking into acount the nr of observations and parameters (Weisberg 1980; Guisan & Zimmermann 2000)
  d2 <- (model$null.deviance - model$deviance) / model$null.deviance
  if (adjust) {
    n <- length(model$fitted.values)
    p <- length(model$coefficients)
    d2 <- 1 - ((n - 1) / (n - p)) * (1 - d2)
  }
  return(d2)
}  # end Dsquared function
