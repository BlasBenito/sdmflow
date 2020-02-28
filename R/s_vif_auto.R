#' Automatic variable selection with variance inflation factor (VIF) analysis
#'
#' @description Selects variables within a dataframe that are not correlated with each other, or with linear combinations of other variables, by using the variance inflation factor (VIF) criteria implemented in the \code{\link[HH]{vif}} function (Heilberger and Holland 2004).
#'
#' @usage s_vif_auto(
#'   training.df,
#'   select.cols = NULL,
#'   omit.cols = c("x", "y", "presence"),
#'   verbose = TRUE
#' )
#'
#' @param training.df A training data frame. Non-numeric columns are excluded from the analysis.
#' @param select.cols Character vector, names of the columns which VIF wants to be assessed. If \code{NULL}, all numeric variables but \code{presence.column} are considered. It is recommended to use the variable order of the \code{variable} column from the data frame output of \code{\link{s_biserial_cor}}.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param preference.order Character vector, column names of \code{training.df} in an order of selection priority desired by the user. For example, if \code{preference.order = c("bio1", "bio2", "bio3")}, the algorithm will first compute vif for all variables in \code{training.df} not included in \code{preference.order}, and remove on each step the variable with a higher vif. Then, vif is computed iteratively on the variables in \code{preference.order}, but removing always the variable with the lowest priority (instead of the variable with the higher vif). Finally, all variables resulting from both vif analyses are grouped together, and a new vif analysis is performed, removing first the variables not in \code{preference.order}. In summary, this option will try to preserve a set of variables as long as their vif values allow it. This option is incompatible with the argument \code{biserial.cor} (see below).
#' @param biserial.cor List, output of the function \code{\link{s_biserial_cor}}. Its R-squared scores are used to select variables. In fact, the column "variable" of the data frame within \code{biserial.cor} is used as input for the argument \code{preference.order} explained above. This is just a convenient way to set the priority in variable selection according to the output of \code{s_biserial_cor}.
#' @param verbose Boolean, defaults to TRUE. Triggers messages describing what variables are being removed.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'
#'selected.vars <- s_vif_auto(
#'  training.df = virtualSpeciesPB,
#'  select.cols = c("bio5", "bio6", "bio1", "bio12"),
#'  verbose = TRUE
#')
#'selected.vars
#'
#'#s_vif_auto() can also take the output of SDMworkshop::s_biserial_cor
#'#as biserial.cor argument, as follows:
#' data(virtualSpeciesPB)
#'
#' biserial.cor.output <- SDMworkshop::s_biserial_cor(
#' training.df = virtualSpeciesPB,
#' response.col = "presence",
#' select.cols = c("bio1", "bio5", "bio6")
#' )
#'
#' #using the output of s_biserial_cor() to prioritize variable selection
#' selected.vars <- SDMworkshop::s_vif_auto(
#'  training.df = df,
#'  biserial.cor = biserial.cor.output,
#'  verbose = TRUE
#')
#'selected.vars
#'
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>. The function \code{\link[HH]{vif}} is authored by Richard M. Heiberger <rmh@temple.edu>.
#' @references Heiberger, Richard M. and Holland, Burt (2004). Statistical Analysis and Data Display: An Intermediate Course with Examples in S-Plus, R, and SAS. Springer Texts in Statistics. Springer. ISBN 0-387-40270-5.
#' @export
s_vif_auto <- function(
  training.df,
  select.cols = NULL,
  omit.cols = c("x", "y", "presence"),
  preference.order = NULL,
  biserial.cor = NULL,
  verbose = TRUE
  ){

  #getting variables
  if(is.null(preference.order) == TRUE){
    preference.order <- colnames(training.df) #<- this is wrong, we are imposing an unwanted order!
  }
  if(is.null(omit.cols) == FALSE){
    if(sum(omit.cols %in% preference.order) == length(omit.cols)){
      preference.order <- preference.order[!(preference.order %in% omit.cols)]
    }
  }

  #keeping numeric columns only and removing NA
  training.df <-
    training.df[, unlist(lapply(training.df, is.numeric))] %>%
    na.omit() %>%
    dplyr::select(tidyselect::all_of(preference.order))

  #initializing selected vars
  selected.vars <- colnames(training.df)

  #removing the select.cols vars if available
  # selected.vars <- selected.vars[!(selected.vars %in% select.cols)]

  #message
  if(verbose == TRUE){cat("Removed variables: ")}

  #computes vif on selected.vars if there's more than one variable
  if(length(selected.vars) > 1){

    #computes vif
    repeat {

      #selects variables with vif lower than 5
      var.to.remove <-
        .vif_to_df(x = training.df[, selected.vars]) %>%
        dplyr::filter(vif > 5) %>%
        dplyr::filter(vif == max(vif)) %>%
        dplyr::slice(1) %>%
        dplyr::select(variable) %>%
        as.character()

      #if the first row contains a vif higher than 5
      if(var.to.remove != "character(0)"){

        #updates select.cols
        if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
        selected.vars <- selected.vars[selected.vars != var.to.remove]

        #stops if there are less than 3 vars left
        if(length(selected.vars) == 1){
          break
        }

      } else {
        break
      } #end of "if(var.to.remove != "character(0)")"

    } #end of repeat

  } #end of "if(length(selected.vars) > 1)..."

  #stops if there is only one selected var
  # if(is.null(selected.vars) == TRUE){
  if(length(selected.vars) == 1){
    if(verbose == TRUE){cat("I'm done! \n")}
    return(selected.vars)
  }

  #tries to keep variables in select.cols
  #--------------------------------------

  #checks if select.cols is in names(training.df)
  if(sum(preference.order %in% colnames(training.df)) == length(preference.order)){

    #generates preference df
    preference <- data.frame(
      variable = c(preference.order, colnames(training.df)[!(colnames(training.df) %in% preference.order)]),
      preference = c(1:length(preference.order), rep(length(preference.order)+1, length(colnames(training.df)) - length(preference.order))),
      stringsAsFactors = FALSE
    )

    #computes vif on variables in select.cols
    #----------------------------------------
    repeat {

      #selects variables with vif lower than 5
      vif.df <-
        .vif_to_df(x = training.df[, preference.order]) %>%
        dplyr::inner_join(y = preference, by = "variable")

      #if the first row contains a vif higher than 5
      if(max(vif.df$vif) > 5){

        #selects variable to remove
        var.to.remove <-
          vif.df %>%
          dplyr::filter(preference == max(preference)) %>%
          dplyr::slice(1) %>%
          dplyr::select(variable) %>%
          as.character()

        #if the first row contains a vif higher than 5
        if(var.to.remove != "character(0)"){

          #updates select.cols
          if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
          selected.vars <- selected.vars[selected.vars != var.to.remove]

          #stops if there are less than 3 vars left
          if(length(selected.vars) == 1){
            break
          }

        } #end of "if(var.to.remove != "character(0)")"

      } else {
        break
      }

    } #end of repeat

    #end of "if(sum(select.cols %in% colnames(x)) == length(select.cols))"
  } else {

    #identifies badly defined variables
    missing.vars <- select.cols[(select.cols %in% colnames(training.df)) == FALSE]

    #message for user
    if(length(missing.vars) == 1){
      paste(
        "The variable ",
        missing.vars,
        "in the argument select.cols are not column names of x."
        ) %>%
        message()
      stop()
    } else {
      paste(
        "The variables",
        paste(
          missing.vars,
          collapse = ", "
          ),
        "in the argument select.cols are not column names of x."
        ) %>%
        message()
      stop()
    }
  } #end of "identifies badly defined variables"


  #vif on selected.vars and select.cols
  #--------------------------------------

  #gets all available variables
  selected.vars <- c(select.cols, selected.vars)

  #stops if there is only one selected var
  if(length(selected.vars) == 1){
    if(verbose == TRUE){cat("I'm done!")}
    return(selected.vars)
    stop()
  }

  #computes vif
  repeat {

    #selects variables with vif lower than 5
    vif.df <-
      .vif_to_df(x = x[, selected.vars]) %>%
      dplyr::inner_join(y = preference, by = "variable")

    #if the first row contains a vif higher than 5
    if(max(vif.df$vif) > 5){

      #selects variable to remove
      var.to.remove <-
        vif.df %>%
        dplyr::filter(!(variable %in% select.cols)) %>%
        dplyr::filter(vif == max(vif)) %>%
        dplyr::slice(1) %>%
        dplyr::select(variable) %>%
        as.character()

      #updates selected.vars
      if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
      selected.vars <- selected.vars[selected.vars != var.to.remove]

      #stops if there are less than 3 vars left
      if(length(selected.vars) == 1){
        break
      }

    } else {
      selected.vars <- vif.df$variable
      break
    } #end of "if(max(vif.df$vif) > 5)..."

  } #end of repeat

  if(verbose == TRUE){cat("I'm done! \n")}
  return(selected.vars)

} #end of function


#' @export
.vif_to_df <- function(x){

  #turns vif output into tidy df
  df <-
    data.frame(
      HH::vif(xx = x),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::arrange(dplyr::desc(vif))

  return(df)
}
