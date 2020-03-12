#TO DO: better order and more clear structure

library(devtools)
library(roxygen2)
library(pkgnet)
library(usethis)
library(rhub)
library(pkgdown)

#CHECK RESULTS PAGE
# https://cran.r-project.org/web/checks/check_results_distantia.html

#about this
# http://r-pkgs.had.co.nz/check.html
# usethis::use_travis()
#generate readme file
# usethis::use_readme_rmd()


#PREPARE PACKAGE SDMworkshop
#update help files
roxygen2::roxygenise()
write("importFrom(stats, cor.test, na.omit, quantile, rnorm, pf, lm, as.formula, cor, hclust, cutree, as.dist)", file="NAMESPACE", append=TRUE)
write("importFrom(ape, Moran.I)", file="NAMESPACE", append=TRUE)
write("importFrom(tools, file_path_sans_ext)", file="NAMESPACE", append=TRUE)
write("importFrom(HH, vif)", file="NAMESPACE", append=TRUE)
write("importFrom(data.table, rbindlist)", file="NAMESPACE", append=TRUE)
write("importFrom(geosphere, distm, distGeo)", file="NAMESPACE", append=TRUE)
write("importFrom(utils, globalVariables)", file="NAMESPACE", append=TRUE)
write("importFrom(raster, raster, stack, brick, as.data.frame, crs)", file="NAMESPACE", append=TRUE)
write("importFrom(dplyr, filter, rename, arrange, slice, select, inner_join)", file="NAMESPACE", append=TRUE)
write("importFrom(tibble, rownames_to_column)", file="NAMESPACE", append=TRUE)
write("importFrom(tidyr, pivot_longer)", file="NAMESPACE", append=TRUE)
write("importFrom(virtualspecies, generateSpFromFun, convertToPA, sampleOccurrences)", file="NAMESPACE", append=TRUE)

#adding pipe support
usethis::use_pipe()

#adding data
usethis::use_data(europe21kBP, quercus, virtual.species, virtual.species.training, europe2000, overwrite = TRUE)

#local check
devtools::check(cran=TRUE, incoming=TRUE, run_dont_test = TRUE, document=FALSE, build_args="--no-build-vignettes")

#build vignettes https://kbroman.org/pkg_primer/pages/vignettes.html
devtools::build_vignettes()
# system("rm vignettes/using_virtualPollen.log vignettes/using_virtualPollen.nb.html vignettes/header.tex")

#build webpage
pkgdown::build_site()

#local check
devtools::check(cran=TRUE, incoming=TRUE, run_dont_test = TRUE, document=FALSE, build_args="--no-build-vignettes")

 #check in rhub
checks <- check_rhub(platforms=c("windows-x86_64-release", "windows-x86_64-oldrel", "windows-x86_64-devel", "ubuntu-gcc-release", "ubuntu-gcc-devel", "macos-elcapitan-release"), interactive = FALSE)

#check in windows
check_win_devel() #failed
check_win_release() #note on name
check_win_oldrelease() #note on name

#prepare for release
devtools::release(check = FALSE)

#build package
system("R CMD build . --compact-vignettes")

#check package in local
system("R CMD check memoria_1.0.0.tar.gz --as-cran")

#generate manual.pdf (not needed for submission)
system("R CMD Rd2pdf . --title=memoria memoria --output=./memoria-manual.pdf --force --internals")
system("mv memoria-manual.pdf vignettes")


#checking package
check(manual = TRUE, cran = TRUE)

#release
devtools::release()

#report about the package
result <- pkgnet::CreatePackageReport('virtualPollen')
result

#extra checking bioconductor


