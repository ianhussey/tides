
library(roxygen2)
#setwd("~/git/")
#devtools::create("tides")
setwd("~/git/tides")

devtools::document()

#devtools::build_vignettes()
#devtools::check()

devtools::check(vignettes = FALSE)

#devtools::install()
# or from github, after push
devtools::install_github("ianhussey/tides")

library(tides)

?tides

detach("package:tides", unload=TRUE)
