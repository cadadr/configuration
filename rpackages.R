# rpackages.R --- install R packages

# Run with `Rscript --no-init-file'

libdir <- Sys.getenv("R_LIBS_USER")

if(!dir.exists(libdir)) {
  dir.create(libdir, recursive = T, showWarnings = T)
}

install.packages("rio", libdir)
install.packages("tidyverse", libdir)

