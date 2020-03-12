# TESTING NEW FUNCTIONS
library(sdmflow)
library(ggplot2)
data("virtual.species.training")
data("europe2000")

#v_match_rasters
#---------------------------------
raster.template <- raster::raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc")
raster.template.crs <- "+init=epsg:4326"

#if raster template is a path
x <- v_match_rasters(
  raster.template = "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc"
  )
inherits(x, "RasterLayer")
raster::crs(x)

#if raster template is a raster without crs
raster.template <- raster::raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc")
x <- v_match_rasters(
  raster.template = raster.template
)
inherits(x, "RasterLayer")
raster::crs(x)

#if raster template is a raster with crs
raster.template <- raster::raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc")
raster::crs(raster.template) <- "+init=epsg:4326"
x <- v_match_rasters(
  raster.template = raster.template
)
inherits(x, "RasterLayer")
raster::crs(x)

#if raster.template.crs is badly defined
raster.template <- raster::raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc")
x <- v_match_rasters(
  raster.template = raster.template,
  raster.template.crs = "hola"
)
inherits(x, "RasterLayer")
raster::crs(x)

#if raster template is any other object
x <- v_match_rasters(
  raster.template = virtual.species.training
)

#checking argument input.folder
x <- v_match_rasters(
  raster.template = "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc",
  raster.template.crs = "+init=epsg:4326", #default option
  input.folder = "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters",
  output.folder = "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/even_rasters",
  n.cores = 2
)

#reading speed: readRDS is way faster!
system.time(x <- readRDS(file = "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/even_rasters/ndvi.rds"))

system.time(x <- raster::raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc"))

raster::raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/even_rasters/ndvi.rds")





#o_make_training NOT DONE YET!!
#----------------------------------

#presence-only data
presence.only <- o_make_training(
  xy = virtual.species$observed.presence,
  variables = europe2000,
  presence.only = TRUE,
  plot = TRUE
)

#background
background <- o_make_training(
  xy = virtual.species$observed.presence,
  variables = europe2000,
  n,
  background = TRUE,
  plot = TRUE
)

#restricted background
restricted.background <- o_make_training(
  xy = virtual.species$observed.presence,
  variables = europe2000,
  n,
  restricted.background = TRUE,
  restricted.background.buffer = 100,
  plot = TRUE
)

#applying thinning
restricted.background <- o_make_training(
  xy = virtual.species$observed.presence,
  variables = europe2000,
  n = 1000,
  restricted.background = TRUE,
  restricted.background.buffer = 100,
  plot = TRUE,
  thinning = TRUE,
  minimum.distance = raster::xres(europe2000)
)

#s_plot_density
#---------------
#TO DO: add response curves
density.plot <- s_plot_density(
  training.df = virtual.species.training,
  response.col = "presence",
  omit.cols = c("x", "y")
)


#s_biserial_cor
#---------------
biserial.cor <- s_biserial_cor(
    training.df = virtual.species.training,
    response.col = "presence",
    omit.cols = c("x", "y"),
    plot = FALSE
  )
biserial.cor$df
class(biserial.cor)

#s_cor
#---------------
s.cor.out <- s_lower_cor(
  training.df = virtual.species.training,
  omit.cols = c("x", "y", "presence")
)

#s_cor with biserial.cor argument
s.cor.out <- s_cor(
  training.df = virtual.species.training,
  omit.cols = c("x", "y", "presence"),
  biserial.cor = biserial.cor,
  max.cor = 0.75
)
s.cor.out$plot
s.cor.out$vars

cor.matrix <-
  virtual.species.training[, s.cor.out$vars] %>%
  cor()

cor.matrix[lower.tri(cor.matrix)] %>%
  round(2) %>%
  as.vector() %>%
  hist()

#s_vif_auto
#---------------
#notes about the function
#scenarios
#1. only training df and omit.cols is provided
#the analysis is done for all numeric variables not in omit.cols, without taking any order of preference into account
vif.auto.out <- s_lower_vif(
  training.df = virtual.species.training
)


#2, biserial.cor is provided
#variables are processed according to their priority.
biserial.cor <- s_biserial_cor(
  training.df = virtual.species.training,
  response.col = "presence",
  omit.cols = c("x", "y"),
  plot = FALSE
)

vif.auto.out <- s_lower_vif(
  training.df = virtual.species.training,
  biserial.cor = biserial.cor
)

#3, preference.order is provided
#variables in preference.order are processed separately from variables not in preference.order
#the former are selected according to priority, the latter are selected by removing those with maximum vif on each step.
vif.auto.out <- s_lower_vif(
  training.df = virtual.species.training,
  preference.order = c("bio1", "bio5", "bio6", "bio12")
)


#s_auto
#---------------
s.auto.out <- s_auto(
  training.df = virtual.species.training,
  response.col = "presence",
  omit.cols = c("x", "y"),
  max.cor = 0.75,
  plot = TRUE,
  text.size = 6
)

cor(training.df[, s.auto.out$vars]) %>%
  as.dist() %>%
  round(2) %>%
  abs()






























# 1 testing importASC ---------------------------
library(raster)
x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = "+init=epsg:4326",
  to.memory = FALSE
  )

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = NULL,
  to.memory = FALSE
)

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = "unknown",
  to.memory = FALSE
)

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = NA,
  to.memory = FALSE
)

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = "+init=epsg:4326",
  to.memory = TRUE
)

rm(x)


# 2 testing plotVariable ----------------------------------------------------
library(SDMworkshop)
data("europe2000")
plotVariable(
  variable = europe2000[["bio1"]],
  option = "B",
  opacity = 0.7
  )

#plotting points
data(virtualSpecies)
plotVariable(
  x = europe2000[["bio1"]],
  option = "B",
  opacity = 0.7,
  points.x = virtualSpecies$observed.presence$x,
  points.y = virtualSpecies$observed.presence$y,
  points.size = 5
)


# 3 testing makeVirtualSpecies --------------------------------------------
library(SDMworkshop)
data("europe2000")

niche.parameters <- list(
  bio12 = c(500, 250),
  bio5 = c(240, 50),
  bio6 = c(10, 30),
  human_footprint = c(0, 30),
  topo_slope = c(0, 2),
  landcover_veg_herb = c(100, 35)
)

vs <- makeVirtualSpecies(
  variables = europe2000,
  niche.parameters = NULL,
  max.n = 200,
  species.type = "multiplicative",
  seed = NULL
  )


# 4 testing autoVIF ----------------------------------------------------
library(SDMworkshop)
library(raster)
data("europe2000")
df <- as.data.frame(europe2000)
vif.output <- autoVIF(x = df)
vif.output <- autoVIF(x = df, try.to.keep = c("bio5", "bio6", "bio12", "ndvi_minimum", "topo_diversity", "biolog"))
vif.output <- autoVIF(x = df, try.to.keep = c("bio5", "bio6", "bio12", "ndvi_minimum", "topo_diversity", "biolog", "artro"))
vif.output <- autoVIF(x = df, try.to.keep = c("bio5", "bio6", "bio12", "bio1", "ndvi_minimum", "topo_diversity"), verbose = TRUE)

#example
data("europe2000")
df <- raster::as.data.frame(europe2000[[c("bio1", "bio5", "bio6", "bio11")]])
selected.vars <- autoVIF(
  x = df,
  try.to.keep = NULL,
  verbose = TRUE
  )
selected.vars

selected.vars <- autoVIF(
  x = df,
  try.to.keep = c("bio5", "bio6"),
  verbose = TRUE
)
selected.vars

# 5 testing corPB -------------------------------------------------------
library(SDMworkshop)
data(virtualSpeciesPB)

#autoVIF can also take the output of corPB
#as try.to.keep argument, as follows:
data(virtualSpeciesPB)

cPB <- s_biserial_cor(
training.df = virtualSpeciesPB,
response.col = "presence",
select.cols = c("bio1", "bio5", "bio6", "bio11", "bio12")
)

#note that cPB$df$variable is ordered from
#higher to lower biserial correlation
#higher biserial correlation is linked
#to higher predictive importance
selected.vars <- s_vif_auto(
 training.df = virtualSpeciesPB,
 select.cols = cPB$df$variable,
 omit.cols = c("x", "y", "presence"),
 verbose = TRUE
)
selected.vars


# 5 testing weightPB -------------------------------------------------------
data("virtualSpeciesPB")
weights <- weightPB(x = virtualSpeciesPB$presence)
table(weights)

weights <- weightPB(x = c(0, 1, 0, 1))

# 5 testing autocor -------------------------------------------------------
data("virtualSpecies")
data(europe2000)
sp.cor <- testSpatialCorrelation(
  xy = virtualSpecies$observed.presence,
  variables = europe2000
  )
sp.cor

data("virtualSpecies")
data(europe2000)
sp.cor <- reduceSpatialCorrelation(
  xy = virtualSpecies$observed.presence,
  variables = europe2000,
  minimum.distance = 3,
  random.start = TRUE,
  seed = NULL,
  verbose = TRUE
)
sp.cor
nrow(virtualSpecies$observed.presence)
nrow(sp.cor)

plotVariable(
  variable = europe2000[["bio1"]],
  points.x = virtualSpecies$observed.presence$x,
  points.y = virtualSpecies$observed.presence$y
)

plotVariable(
  variable = europe2000[["bio1"]],
  points.x = sp.cor$x,
  points.y = sp.cor$y
)


# 6 prepareTrainingData -------------------------------------------------------
data("virtualSpecies")
data(europe2000)
presence.only <- prepareTrainingData(
  xy = virtualSpecies$observed.presence,
  variables = europe2000,
  presence.only = TRUE
)

#background
background <- prepareTrainingData(
  xy = virtualSpecies$observed.presence,
  variables = europe2000,
  n = 1000,
  background = TRUE,
  plot = TRUE
)

#restricted background
restricted.background <- prepareTrainingData(
  xy = virtualSpecies$observed.presence,
  variables = europe2000,
  n = 1000,
  restricted.background = TRUE,
  restricted.background.buffer = 100,
  plot = TRUE
)

#with thinning
restricted.background <- prepareTrainingData(
  xy = virtualSpecies$observed.presence,
  variables = europe2000,
  n = 1000,
  restricted.background = TRUE,
  restricted.background.buffer = 100,
  plot = TRUE,
  thinning = TRUE,
  minimum.distance = raster::xres(europe2000)
)

# 6 correlationDendrogram -----------------------------------------------------ç


#auto version
selected.vars <- s_cor_auto(
  training.df = virtualSpeciesPB,
  e.omit = c("x", "y", "presence"),
  max.cor = 0.25,
  bis.cor = bis.cor,
  text.size = 6
)

# 7 autoSelectVariables -------------------------------------------------------
data("virtualSpeciesPB")
selected.vars <- autoSelectVariables(
  x = virtualSpeciesPB,
  presence.column = "presence",
  exclude.variables = c("x", "y")
  )
selected.vars
HH::vif(virtualSpeciesPB[, selected.vars])
cor(virtualSpeciesPB[, selected.vars])

# 8 plotUseAvailability -------------------------------------------------------
data("virtualSpeciesPB")
x <- plotUseAvailability(
  x = virtualSpeciesPB,
  presence.column = "presence",
  variables = NULL,
  exclude.variables = c("x", "y"),
  plot = TRUE
)


# 9 import4D  and dfFrom4D-----------------------------

folder <- "/home/blas/Dropbox/GITHUB/R_packages/SDMworkshop/example_data/by_file"
dynamic.vars <- c("chl", "cufes", "sst", "tsm")
static.vars <- c("bat", "dist_guadalete", "dist_guadalquivir", "dist_guadiana", "slope")
vars.crs <- "+init=epsg:4326"
times <- c("2006", "2007", "2009", "2010", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
to.memory = FALSE
to.data.frame = FALSE


# modify raster.template to check that the homogeneization options actually work
raster.template <- raster::raster("/home/blas/Dropbox/GITHUB/R_packages/SDMworkshop/example_data/by_file/bat.asc")
raster::crs(raster.template) <- "+init=epsg:4326"
raster.template <- raster::crop(raster.template, y = c(-10, 10, 30, 50))
raster.template <- raster::aggregate(raster.template, fact = 2)
raster.template <- raster::projectRaster(from = raster.template, res = 10000, crs = "+init=epsg:23030")

x <- import4D(
  raster.template = raster.template,
  folder = folder,
  dynamic.vars = dynamic.vars,
  static.vars = static.vars,
  vars.crs = vars.crs,
  times = times,
  to.memory = to.memory,
  to.data.frame = to.data.frame
  )

plot(x[[1]])


x <- import4D(
  raster.template = "/home/blas/Dropbox/GITHUB/R_packages/SDMworkshop/example_data/by_file/bat.asc",
  folder = folder,
  dynamic.vars = dynamic.vars,
  static.vars = static.vars,
  vars.crs = vars.crs,
  times = times,
  to.memory = to.memory,
  to.data.frame = to.data.frame
)

plot(x[[1]])

x <- import4D(
  raster.template = "/home/blas/Dropbox/GITHUB/R_packages/SDMworkshop/example_data/by_file/bat.asc",
  raster.template.crs = vars.crs,
  folder = folder,
  dynamic.vars = dynamic.vars,
  static.vars = static.vars,
  vars.crs = vars.crs,
  times = times,
  to.memory = to.memory,
  to.data.frame = to.data.frame
)

plot(x[[1]])


x <- import4D(
  folder = folder,
  dynamic.vars = dynamic.vars,
  static.vars = static.vars,
  times = times
)

plot(x[[1]])

x <- import4D(
  folder = folder,
  dynamic.vars = dynamic.vars,
  times = times,
  to.data.frame = TRUE
)

plot(x[[1]])

x.df <- dfFrom4D(x = x)


# 10 extract4D ----------------------------------------
library(readr)
presence <- read_delim("example_data/presence.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)
