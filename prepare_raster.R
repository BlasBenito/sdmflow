library(raster)
library(rgdal)

continentality <- raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/continentality.img")

crs(continentality)

continentality <- raster::aggregate(x =continentality, fact = 10)

writeRaster(continentality, "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/continentality.asc")

hfp <- raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/hfp.")

hfp <- raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/hfp.asc")
ndvi <- raster("/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/ndvi.asc")

crs(elev)
crs(hfp) <- "+init=epsg:4326"
crs(ndvi) <- "+init=epsg:4326"

#elev to EPSG 25830 (ETRS89 UTM ZONE 30)
elev_utm <- raster::projectRaster(from = elev, res = 1000, crs = "+init=epsg:25830")
crs(elev_utm)
plot(elev_utm)

writeRaster(elev_utm, "/home/blas/Dropbox/GITHUB/R_packages/sdmflow_shared/example_data/uneven_rasters/elev_utm.asc")
