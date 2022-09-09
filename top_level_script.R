#################################################################################
## Code associated with the publication: 
## Not all mosquitoes are created equal: a synthesis of vector competence experiments
##   reinforces virus associations of Australian mosquitoes
##
## Data cleaning, statistical analyses, and publication figures
#################################################################################

## Required packages and functions
install_needed_packages <- TRUE     ## Install packages you are missing?
source("packages_and_functions.R")

## Decide whether to use the maximum detected proportion or all of the data for analyses 
 ## NOTE: The first submitted manuscript version used only the maximum while all revisions (and thus the published paper) 
  ## used all of the data. Leaving the code for the max here for transparency and for interested readers who might want to
   ## see what the results look like using the maximum detected proportions
    ## Set this to FALSE to recover the analysis done for the published manuscript
use.max <- FALSE

if (!use.max) {

## Extract the maxima for a few supplemental figures. These maxima are not used for analyses if
 ## use.max == FALSE, just a few supplemental figures.
  ## The modifications to the raw data here are just used to store some data frames; the raw data is 
   ## reloaded and cleaned for the analyses in the script below
source("data_clean_max_extract.R")

## Clean raw data for analyses
source("data_clean.R")

## Run all stats except for the Aedes aegypti GLMMs (see script below)
source("stats_all.R")
  
## Fit glmms for Aedes aegypti
 ## WARNING: the bootMer for CI from the fitted GLMM models for Aedes aegypti took about 2h each (one model each for infection,
  ## dissemination, and transmission) on my laptop (mid 2010s Macbook Pro Intel with 16gb ram). 
   ## Set run.bootmer to false to just skip obtaining CI for these models
run.bootmer <- FALSE
source("aedes_glmm.R")
  
## All of the manuscript figures that rely on "stats_all.r". Running from source() here is not advised. 
 ## Open the script and work through the figures
# source("figures.R")
  
## 
  
} else {
  
## Data cleaning and extraction of the maximum from each mosquito-virus pair
source("data_clean_max_extract.R")
  
## Organization/cleaning of the output for stats and plotting
source("output_clean.R")
  
## All stats reported in the manuscript (again, first submission) apart from the simple reporting of the maxima (which does not require statistics)
   ## See comment above about run.bootmer
run.bootmer <- FALSE
source("stats.R")

## Figures script updated for the final manuscript and no longer reliable for these older analyses

}
