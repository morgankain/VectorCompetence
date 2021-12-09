#################################################################################
## Code associated with the publication: 
## Not all mosquitoes are created equal: incriminating mosquitoes as vectors of arboviruses
## Data cleaning, statistical analyses, and publication figures
#################################################################################

## Required packages and functions
install_needed_packages <- TRUE     ## Install packages you are missing 
source("packages_and_functions.R")

## Data cleaning and extraction of the maximum from each mosquito-virus pair
source("data_clean_max_extract.R")

## Organization/cleaning of the output for stats and plotting
source("output_clean.R")

## All stats reported in the manuscript (e.g., Figure 2) apart from the simple reporting of the maxima (e.g., Figures 3 and 4)
 ## WARNING: the bootMer for the GLMM models for Supplemental Figure S7 take about 2h each (there are three). Set this value
  ## to false to just run the other stats
run.bootmer <- FALSE
source("stats.R")

## All of the manuscript figures (+/- Figure S7 depending on choice for run.bootmer)
source("figures.R")

