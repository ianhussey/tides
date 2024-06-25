
library(roxygen2)
#setwd("~/git/")
#devtools::create("TIDES")
setwd("~/git/TIDES")

devtools::document()

#devtools::build_vignettes()
#devtools::check()

devtools::check(vignettes = FALSE)

devtools::install()

# or from github, after push
#devtools::install_github("ianhussey/TIDES")

library(TIDES)

?TIDES

detach("package:TIDES", unload=TRUE)
