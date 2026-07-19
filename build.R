library(roxygen2)
#setwd("~/git/")
#devtools::create("tides")
setwd("~/git/tides")

devtools::document()

devtools::check()

# once you have the package updated, you can use it to build the vignettes, check the whole thing, and reinstall again
devtools::build_vignettes()

devtools::install(build_vignettes = TRUE)

# # or from github, after push
# devtools::install_github("ianhussey/tides")

library(tides)

?tides
vignette("tides")


detach("package:tides", unload = TRUE)


# cran checks
# win-builder
library(devtools)
check_win_devel() # emails results to the maintainer address in DESCRIPTION

# R-hub
library(rhub)
rhub_check() # v2: runs on GitHub Actions in your repo
