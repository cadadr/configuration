# profile.R --- R environment.

### Set options:

# Disable scientific notation.
options(scipen=999)
# options(scipen=0) #enable scientific notation



### Load some useful functions by default:

# The idea with ‘G.’ prefix here is that using those, I won’t be
# fooled by my default environment and forget to import packages when
# publishing code.

G.describe <- psych::describe
G.import <- rio::import
G.export <- rio::export
