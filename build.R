
library(roxygen2)
#setwd("~/git/")
#devtools::create("tides")
setwd("~/git/tides")

devtools::document()

devtools::check(vignettes = FALSE)

#devtools::install()
# or from github, after push
devtools::install_github("ianhussey/tides")

library(tides)

?tides

detach("package:tides", unload=TRUE)

# once you have the package updated, you can use it to build the vignettes, check the whole thing, and reinstall again
devtools::build_vignettes()
devtools::check()

