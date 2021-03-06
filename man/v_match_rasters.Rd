% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/v_match_rasters.R
\name{v_match_rasters}
\alias{v_match_rasters}
\title{Matching the reference coordinate system, extension, resolution, and mask of rasters from different sources.}
\usage{
v_match_rasters(
 raster.template = NULL,
 raster.template.crs = "+init=epsg:4326",
 input.folder,
 output.folder,
 default.crs = "+init=epsg:4326",
 n.cores = NULL
 )
}
\arguments{
\item{raster.template}{Complete path to a raster file or raster object in the R environment to be used as a template. All rasters in \code{input.folder} will be adjusted to the coordinate reference system, exent, and resolution of this raster layer.}

\item{raster.template.crs}{Character string in proj4string format defining a coordinate reference system of \code{raster.template}. Only required when the raster format of the template does not contain this information (as it happens with .asc files). The default crs is \emph{"+init=epsg:4326"}, valid latitude-longitude data according the global datum WGS84.}

\item{input.folder}{Character string, path (without final slash) to the folder containing the raster files to be matched, or folder containing subfolders, each one containing raster files representing data from different times.}

\item{output.folder}{Character string, path (without final slash) of the folder where the matched rasters will be written. Defaults to "/matched_rasters" in the working directory if nothing else is provided}

\item{default.crs}{Default coordinate system for those files in \code{input.folder} that don't have one. The default value is \code{"+init=epsg:4326"}, as in \code{raster.template.crs}. Use this argument with care!}

\item{n.cores}{Integer, number of cores to be used during the matching operations. If the target rasters are very large, and the RAM memory available is low, \code{n.cores} should be low as well.}
}
\value{
The function writes the matched rasters to \code{output.folder} in R format, with the extensions .grd and .gri. These rasters can be imported directly with \code{raster::raster()} and outside of R with Quantum GIS.

The function also returns an object of the class "environmental.data" with a slot named "meta" containing a data frame with information (metadata) that helps to trace the transformations applied to each raster file. This output can be used down the line by \code{v_import_rasters} to import the matched rasters into the R session. The object has two variants:

\itemize{
\item **4D** variant: If \code{input.folder} contains raster files, the output object has the attribute "4D", and the "meta" slot contains a data frame with metadata.
\item **5D** variant: If \code{input.folder} contains sub-folders with raster files representing different times, the output object has the attribute "5D", and the "meta" slot has sub-slots named after the different sub-folders, and each one contains the data frame with the metadata of the operations applied for each particular folder.
}
}
\description{
Raster files from different sources may have different reference coordinate systems, extensions, resolutions, and valid cells. However, species distribution models require all rasters to have the same features, and this function facilitates the tedious task of processing raster layers so they can be used in species distribution modelling exercises.
}
\details{
This function requires a \strong{\code{raster.template}}, which is a raster file stored in the hard disk or a raster object in the R environment having the desired properties.

If the template file does not contain information about the coordinate reference system (.asc files don't), the user must define the argument \strong{\code{raster.template.crs}}, which takes a character string following the \href{https://proj.org/usage/quickstart.html}{proj4string}. The easiest way to do this is by using the \href{https://spatialreference.org/}{EPSG} code of the given coordinate reference system. For example, for for latitude-longitude data based on the datum WGS84, the usual proj4string format would be \code{"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"}. The EPSG alternative is \code{"+init=epsg:4326"}, way shorter and easier to remember. Note that this argument is not required if the file format of \code{raster.template} already contains information about the coordinate reference system. This is the case with geotif files (.tif extension) and many others.

The \strong{\code{input.folder}} argument is either:
\itemize{
\item The path to the folder containing the raster files to be matched. For example, the folder ".../my_rasters" containing the rasters "temperature.tif" and "rainfall.tif"). This is named \strong{4D} data (x, y, variable values, and variable names) in this package.

\item The path to the folder containing the subfolders with the rasters representing different times. For example, the folder ".../my_rasters" containing the subfolders "2019" and "2020", each one containing the rasters "temperature.tif" and "rainfall.tif" of the respective years. This is named \strong{5D} data (x, y, variable values, variable names, and times). In this case, the sub-folders with the matched rasters are written to \code{output.path}.
}

When the data is separated in subfolders representing different times, the mask (spatial distribution of no-data values) for each subfolder is computed separately. This allows different times to have different masks. This is especially useful when different times may have very different masks, as it is the case between the present time and the Last Glacial Maximum.

This function relies on the following functions of the \code{raster} package:
\itemize{
\item \code{\link[raster]{projectRaster}}: to reproject any given raster to the same coordinate reference system, extension and resolution as \code{raster.template} when they do not have the same crs.
\item \code{\link[raster]{resample}}: to adjust a raster to the extension and resolution of \code{raster.template} when they have the same crs.
\item \code{\link[raster]{crop}}: to adjust a raster to the extension of \code{raster.template} when they have the same crs and resolution.
\item \code{\link[raster]{mask}}: to apply a common mask to all rasters.
\item \code{\link[raster]{trim}}: to remove NA values from the border of the rasters.
\item \code{\link[raster]{raster}}: to import raster files from disk into R.
\item \code{\link[raster]{crs}}: to set the coordinate reference system of layers that do not have any.
}
}
\author{
Blas Benito <blasbenito@gmail.com>. The functions \code{\link[raster]{raster}}, \code{\link[raster]{crs}}, \code{\link[raster]{projectRaster}}, \code{\link[raster]{resample}}, \code{\link[raster]{crop}}, \code{\link[raster]{mask}}, and \code{\link[raster]{trim}} are authored by Robert J. Hijmans.
}
