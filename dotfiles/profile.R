# profile.R --- R environment.

### Set options:

# Disable scientific notation.
options(scipen=999)
# options(scipen=0) #enable scientific notation

# Don't ask to save image when exiting
# (from: <https://stackoverflow.com/a/38206498/6999086>).
formals(quit)$save <- formals(q)$save <- "no"

# Make errors easier to see (from:
# <https://stackoverflow.com/a/1349232/6999086>)
options(showWarnCalls=T, showErrorCalls=T)



### Load some useful functions by default:

# The idea with ‘my.’ prefix here is that using those, I won’t be
# fooled by my default environment and forget to import packages when
# publishing code.

my.describe <- psych::describe
my.import <- rio::import
my.export <- rio::export
my.factorify <- function(x, ...) {
    for(col in list(...)) {
        x[col] <- as.factor(x[col])
    }
    return(x)
}



### Aliases:

# Adapted from: https://stackoverflow.com/a/1798428/6999086
cd <- setwd
pwd <- getwd
l <- dir



### History:

# Adapted from: https://stackoverflow.com/a/1357432/6999086

if (interactive()) {
    my.histfile.default <- file.path(Sys.getenv('HOME'), '.Rhistory')
    my.histfile <- Sys.getenv('R_HISTFILE', my.histfile.default)

    if ( ! file.exists(my.histfile) ) {
        file.create(my.histfile)
    }

    utils::loadhistory(my.histfile)

    Sys.setenv('R_HISTSIZE' = '10000')

    .Last <- function() {
        if (!any(commandArgs()=='--no-readline') && interactive()){
            require(utils)
            try(savehistory(my.histfile))
        }
    }
}
