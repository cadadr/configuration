# packages.R --- install R packages

# Run with `Rscript --no-init-file'

libdir <- Sys.getenv("R_LIBS_USER")

if(!dir.exists(libdir)) {
  dir.create(libdir, recursive = T, showWarnings = T)
}

options(repos=c(CRAN="https://cloud.r-project.org/"))

install.packages("rio", libdir)
install.packages("tidyverse", libdir)
install.packages("lintr", libdir)
install.packages("gridExtra", libdir)
install.packages("emuR", libdir)
install.packages("lingtypology", libdir)

## FIXME(2022-10-18): does this work fine with ESS?
# - https://github.com/emacs-ess/ESS/issues/554
# - https://github.com/emacs-ess/ESS/issues/1214
# - https://stackoverflow.com/q/72735320
# install.packages("languageserver", libdir)
