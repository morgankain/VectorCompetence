## Required packages to run this code
needed_packages <- c(
    "tidyverse"
  , "reshape2"
  , "ggplot2"
  , "magrittr"
  , "lme4"
  , "RColorBrewer"
  , "emmeans"
  , "brglm2"
)

if (install_needed_packages) {
if (length(setdiff(needed_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(needed_packages, rownames(installed.packages())))  
}
}

## load packages. Install all packages that return "FALSE"
print(paste(
  needed_packages
, "loaded? -- "
, unlist(lapply(needed_packages, require, character.only = TRUE))
  ))

## Also load a cleaner ggplot2 theme
source("ggplot_theme.R")
