
##############################################################################
mapLocalImportance=function(predictors.brick, response.raster, scale.factor){

  #loading libraries
  require(data.table)
  require(raster)

  if(scale.factor < 5){
    scale.factor <- 5
  }

  #starting to create the final stack
  final.stack <- stack()
  # final.stack <- stack(final.stack, predictors.brick)

  #name of the response variable and predictors
  name.response.variable <- names(response.raster)
  names.variables <- names(predictors.brick)

  #creating vectors to store variable names with "_coef" and "_R2". This vectors will be used to select columns in the data.table
  names.variables.coef <- vector()

  #creating sample raster
  id.raster <- raster(response.raster)

  #Loop through scale factors
  for (sf in scale.factor){

    #define bigger cells
    id.raster <- aggregate(id.raster, fact = sf, expand = TRUE)

    #add ID values
    id.raster <- setValues(id.raster, seq(1, ncell(id.raster)))

    #go back to the previous resolution
    id.raster <- disaggregate(id.raster, fact = sf)

    #need to use crop here, more cells than expected!
    id.raster <- crop(id.raster, extent(response.raster))
    names(id.raster) <- "id"

    #stacking all the data
    names.variables.R2 <- vector()
    names.variables.pvalue <- vector()

    #populating vectors
    for (variable in names.variables){
      names.variables.coef[length(names.variables.coef) + 1] <- paste(variable, "_coef",sep = "")
      names.variables.R2[length(names.variables.R2) + 1] <- paste(variable, "_R2", sep = "")
      names.variables.pvalue[length(names.variables.pvalue) + 1] <- paste(variable, "_pvalue", sep = "")
    }

    #list of formulas to fit the linear models
    formulas <- list()
    data.stack <- stack(predictors.brick, response.raster, id.raster)

    #as data frame
    data.df <- as.data.frame(data.stack)

    #id as factor
    data.df$id <- as.factor(data.df$id)

    #ordered row names
    data.df$key <- seq(1, nrow(data.df))

    #create columns to store coef, R2 and pvalues
    data.df[ , c(names.variables.coef, names.variables.R2, names.variables.pvalue)] <- as.numeric(NA)

    #fill formulas
    formulas <- lapply(names.variables, function(x) as.formula(paste(name.response.variable, " ~ ", x, sep = "")))

    #names for the formulas list
    names(formulas) <- names.variables

    #counts the number of non-NA values on each id
    valid.ids <- aggregate(x = !is.na(data.df[ , names.variables[1]]), by = list(data.df$id), FUN = sum)

    #right column names (setnames is better than names, doesn't copy the whole table again)
    setnames(valid.ids, old = names(valid.ids), new = c("id", "n"))

    #selects ids with 25 or more cases
    valid.ids <- valid.ids[valid.ids$n >= 25, "id"]

    #convert data.frame to data.table and set key (subsets are faster this way)
    data.df <- data.table(data.df)
    setkey(data.df, id)

    #ITERATE THROUGH IDs
    for (valid.id in valid.ids){

      #fits a model for each variable
      lm.temp <- lapply(names.variables, function(x) lm(formulas[[x]], data = data.df[paste(valid.id)], na.action = na.exclude))
      names(lm.temp) <- names.variables

      #storing coefficients
      data.df[paste(valid.id), (names.variables.coef) := lapply(lm.temp, function(x) summary(x)$coefficients[2])]

      #storing R2
      data.df[paste(valid.id), (names.variables.R2) := lapply(lm.temp, function(x) summary(x)$r.squared)]

      #storing pvalue
      data.df[paste(valid.id), (names.variables.pvalue) := lapply(lm.temp, function(x) anova(x)$'Pr(>F)'[1])]

      #remove results list to start from scratch in the next loop
      rm(lm.temp)

    } #end of loop through ids

    #Not using data.table anymore, converting to data.frame
    data.df <- data.frame(data.df)

    #ordering the table
    data.df <- data.df[with(data.df, order(data.df$key)), ]

    #TURNING THE RESULTS INTO A MAP
    #copy variables stack
    stack.coef <- predictors.brick
    stack.R2 <- predictors.brick
    stack.pvalue <- predictors.brick

    ##loop through variables to set values and generate values for the resulting stack
    names.stack.coef <- vector()
    names.stack.R2 <- vector()
    names.stack.pvalue <- vector()

    for (variable.name in names.variables){

      #populate vectors with names
      names.stack.coef[length(names.stack.coef)+1] <- paste(variable.name, "_coef_", sf, sep = "")
      names.stack.R2[length(names.stack.R2)+1] <- paste(variable.name, "_R2_", sf, sep = "")
      names.stack.pvalue[length(names.stack.pvalue)+1] <- paste(variable.name, "_pvalue_", sf, sep = "")

      #set coef values
      stack.coef[[variable.name]] <- setValues(x = stack.coef[[variable.name]], values = data.df[ , paste(variable.name, "_coef", sep = "")])

      #Set R2 values
      stack.R2[[variable.name]] <- setValues(x = stack.R2[[variable.name]], values = data.df[ , paste(variable.name, "_R2", sep = "")])

      #Set pvalues
      stack.pvalue[[variable.name]] <- setValues(x = stack.pvalue[[variable.name]], values = data.df[ , paste(variable.name, "_pvalue", sep = "")])

    }

    #set stacks names
    names(stack.coef) <- names.stack.coef
    names(stack.R2) <- names.stack.R2
    names(stack.pvalue) <- names.stack.pvalue

    #mask values outside the coast
    stack.coef <- raster::mask(x = stack.coef, mask = response.raster)
    stack.R2 <- raster::mask(x = stack.R2, mask = response.raster)
    stack.pvalue <- raster::mask(x = stack.pvalue, mask = response.raster)

    #computes the layer with the maximum R2 value for each cell
    most.important.layer<-which.max(stack.R2)
    names(most.important.layer) <- paste("most_important_variable_", sf, sep = "")

    #stack stacks with the final stack (not kidding)
    final.stack <- stack(final.stack, stack.coef, stack.R2, stack.pvalue, most.important.layer)

  }

  #returning results
  return(final.stack)

} #end of function
