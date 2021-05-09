# rpackages.R --- install R packages

# Run with `Rscript --no-init-file'

libdir <- Sys.getenv("R_LIBS_USER")

if(!dir.exists(libdir)) {
  dir.create(libdir, recursive = T, showWarnings = T)
}

install.packages("rio", libdir)
install.packages("tidyverse", libdir)
install.packages("lintr", libdir)
install.packages("gridExtra", libdir)
install.packages("emuR", libdir)
install.packages("lingtypology", libdir)
