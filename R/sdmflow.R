#' @details Details
"_PACKAGE"

#' Predictive variables for Europe representing current times.
#'
#' Raster brick with predictive variables to fit species distribution models for the European territory at ~20km resolution and reference system WGS84 (units are degrees of latitude and longitude, EPSG is 4326).
#' The variables are:
#' \itemize{
#'   \item \emph{bio1}: annual mean temperature (ºC x 10).
#'   \item \emph{bio2}: mean diurnal temperature range, computed as \eqn{mean of monthly (max temp - min temp)}.
#'   \item \emph{bio3}: isothermality, computed as \eqn{(bio2/bio7)*100}.
#'   \item \emph{bio4}: temperature seasonality (standard deviation of monthly averages * 100).
#'   \item \emph{bio5}: maximum temperature of the warmest month.
#'   \item \emph{bio6}: minimum temperature of the coldest month.
#'   \item \emph{bio7}: temperature annual range, computed as \eqn{(bio5-bio6}.
#'   \item \emph{bio8}: mean temperature of the wettest quarter.
#'   \item \emph{bio9}: mean temperature of the driest quarter.
#'   \item \emph{bio10}: mean temperature of the warmest quarter.
#'   \item \emph{bio11}: mean temperature of the coldest quarter.
#'   \item \emph{bio12}: annual precipitation (mm.).
#'   \item \emph{bio13}: precipitation of wettest month.
#'   \item \emph{bio14}: precipitation of driest month.
#'   \item \emph{bio15}: precipitation seasonality (standard deviation of monthly averages * 100).
#'   \item \emph{bio16}: precipitation of wettest quarter.
#'   \item \emph{bio17}: precipitation of driest quarter.
#'   \item \emph{bio18}: precipitation of warmest quarter.
#'   \item \emph{bio19}: precipitation of coldest quarter.
#'   \item \emph{ndvi_average}: average normalized vegetation index.
#'   \item \emph{ndvi_minimum}: minimum normalized vegetation index.
#'   \item \emph{ndvi_maximum}: maximum normalized vegetation index.
#'   \item \emph{topo_slope}: topographic slope (º).
#' }
#'
#' @details
#'
#' Bioclim variables (\emph{bioXX}) were downloaded from the \emph{Worldclim v1.4} dataset (Hijmans et al. 2005, \url{https://www.worldclim.org/version1}).
#'
#' The variable \emph{topo_slope} was generated from the same digital elevation model with the \emph{r.slope.aspect} function of the GRASS GIS software (Hofierkaet al. 2009).
#'
#'
#' @docType data
#' @keywords datasets
#' @name europe2000
#' @format Raster brick with 22 layers.
#' @references Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#' @references Sanderson, E.W., Jaiteh, M., Levy, M.A., Redford, K.H., Wannebo, A.V., Woolmer, G. (2002) The Human Footprint and the Last of the Wild: The human footprint is a global map of human influence on the land surface, which suggests that human beings are stewards of nature, whether we like it or not, BioScience, Volume 52, Issue 10, October 2002, Pages 891–904, https://doi.org/10.1641/0006-3568(2002)052[0891:THFATL]2.0.CO;2
#' @references Hofierka, J., Suri, M. (2002): The solar radiation model for Open source GIS: implementation and applications. International GRASS users conference in Trento, Italy, September 2002.
#' @references Jarvis, A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM 90m Database (\url{http://srtm.csi.cgiar.org}).
#' @references Hofierka, J., Mitasova, H., Neteler, M., 2009. Geomorphometry in GRASS GIS. In: Hengl, T. and Reuter, H.I. (Eds), Geomorphometry: Concepts, Software, Applications. Developments in Soil Science, vol. 33, Elsevier, 387-410 pp, \url{http://www.geomorphometry.org}.
"europe2000"


#' Predictive variables for Europe representing climatic conditions during the Last Glacial Maximum (21 ka BP).
#'
#' Raster brick with predictive variables to fit species distribution models for the European territory at ~20km resolution and reference system WGS84 (units are degrees of latitude and longitude, EPSG is 4326).
#' The variables are:
#' \itemize{
#'   \item \emph{bio1}: annual mean temperature (ºC x 10).
#'   \item \emph{bio2}: mean diurnal temperature range, computed as \eqn{mean of monthly (max temp - min temp)}.
#'   \item \emph{bio3}: isothermality, computed as \eqn{(bio2/bio7)*100}.
#'   \item \emph{bio4}: temperature seasonality (standard deviation of monthly averages * 100).
#'   \item \emph{bio5}: maximum temperature of the warmest month.
#'   \item \emph{bio6}: minimum temperature of the coldest month.
#'   \item \emph{bio7}: temperature annual range, computed as \eqn{(bio5-bio6}.
#'   \item \emph{bio8}: mean temperature of the wettest quarter.
#'   \item \emph{bio9}: mean temperature of the driest quarter.
#'   \item \emph{bio10}: mean temperature of the warmest quarter.
#'   \item \emph{bio11}: mean temperature of the coldest quarter.
#'   \item \emph{bio12}: annual precipitation (mm.).
#'   \item \emph{bio13}: precipitation of wettest month.
#'   \item \emph{bio14}: precipitation of driest month.
#'   \item \emph{bio15}: precipitation seasonality (standard deviation of monthly averages * 100).
#'   \item \emph{bio16}: precipitation of wettest quarter.
#'   \item \emph{bio17}: precipitation of driest quarter.
#'   \item \emph{bio18}: precipitation of warmest quarter.
#'   \item \emph{bio19}: precipitation of coldest quarter.
#' }
#'
#' @details
#'
#' Bioclim variables (\emph{bioXX}) were downloaded from the CCSM4 model available in the \emph{Last Glacial Maximum section} of the \emph{Worldclim v1.4} dataset (Hijmans et al. 2005, \url{https://www.worldclim.org/version1}).
#'
#' @docType data
#' @keywords datasets
#' @name europe21kBP
#' @format Raster brick with 19 layers.
#' @references Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
"europe21kBP"


#' Dataframe with presence data of 8 Quercus species in Europe and 2000 true absences for all species.
#'
#' Dataframe with 3 columns and 6946 rows containing presence data of the species \emph{Quercus robur}, \emph{Quercus petraea}, \emph{Quercus ilex}, \emph{Quercus cerris}, \emph{Quercus pyrenaica}, \emph{Quercus faginea}, \emph{Quercus suber}, \emph{Quercus pubescens}, and \emph{Quercus robur}, and 2000 absences. The presence data has been extracted from the EU-Forest dataset (Mauri et al. 2017, \url{https://figshare.com/collections/A_high-resolution_pan-European_tree_occurrence_dataset/3288407}). Absences were defined at EU-Forest plots without any \emph{Quercus} species, and are therefore true absences.
#' The dataset columns are:
#' \itemize{
#'   \item \emph{x}: longitude in degrees, datum WGS84 (EPSG 4326).
#'   \item \emph{y}: latitude in degrees, datum WGS84 (EPSG 4326).
#'   \item \emph{species}: character column with species names and the string \emph{absence}.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name quercus
#' @format Data frame with 3 columns and 6946 rows.
#' @references Mauri, A., Strona, G. & San-Miguel-Ayanz, J. (2017) EU-Forest, a high-resolution tree occurrence dataset for Europe. Scientific Data 4, 160123 doi:10.1038/sdata.2016.123
"quercus"


#' Named list describing a virtual species.
#'
#' Named list with 5 named slots containing a description of a virtual species generated with the function \code{\link{makeVirtualSpecies}}, which depends on the function \code{\link[virtualspecies]{generateSpFromFun}} (Leroy et al. 2015).
#' The list slots are:
#' \itemize{
#'   \item \emph{niche.dimensions}: vector with the names of the variables in \code{\link{europe2000}} used to define the ecological niche of the virtual species.
#'   \item \emph{niche.parameters}: data frame with the mean (column \emph{mean}) and the standard deviation (column \emph{sd}) of the normal functions used to define the ecological niche of the virtual species.
#'   \item \emph{niche.plot}: plot describing the niche and distribution of the species.
#'   \item \emph{suitability}: raster map, suitability raster for the virtual species as defined by \code{\link[virtualspecies]{generateSpFromFun}}.
#'   \item \emph{observed.presence}: data frame with the coordinates \emph{x} and \emph{y} (reference system WGS84, EPSG 4326) of the simulated presences of the virtual species.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name virtualSpecies
#' @format Named list.
#' @references Leroy B, Meynard CN, Bellard C, Courchamp F (2015). “virtualspecies, an R package to generate virtual species distributions.” Ecography. doi: 10.1111/ecog.01388.
"virtual.species"


#' Presence and background data for a virtual species
#'
#' Dataframe with 8344 observations and 34 variables containing presence records of a virtual species and background data. Each record contains the latitude, longitude, and values of 31 predictive variables, along with a presence column with 1 indicating presence, and 0 indicating background. This data is ready for model fitting.
#'
#' @docType data
#' @keywords datasets
#' @name virtualSpeciesPB
#' @format Data frame.
"virtual.species.training"

#' @import tibble dplyr ggplot2 leaflet virtualspecies
NULL

#' @import utils
utils::globalVariables(c("vif", "variable", "presence", "value", "layer", "R2", "x", "y", "group", ".", "xend", "yend", "biserial.cor", "label", "..scaled.."))

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
