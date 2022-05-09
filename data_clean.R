  ## Load the data
All <- read.csv("All.csv") %>% 
  ## Rename some columns for more consistent naming conventions
  rename(
  num.Infection       = num.infected
, total.Infection     = total.infect
, num.Dissemination   = num.disseminated
, total.Dissemination = total.dissem
, num.Transmission    = num.transmitted
, total.Transmission  = total.transmitted
) %>% 
  ## Calculate percent
  mutate(
  perc.Infection     = num.Infection / total.Infection
, perc.Dissemination = num.Dissemination / total.Dissemination
, perc.Transmission  = num.Transmission / total.Transmission
) %>% 
  ## Remove Ae. polynesiensis which doesn't occur in Australia
  filter(species != "ae_polynesiensis") %>% 
  ## convert refs to numeric than factor to shorten them
  mutate(ref = as.factor(ref)) %>% 
  mutate(ref = as.numeric(ref)) %>% 
  ## and add a column to identify different replicated experiments that had the same dose and day within a given citation
  group_by(species, virus, infectious.dose.mid, day.PE.mid, ref) %>% 
  mutate(experiment = seq(n())) %>%
  ungroup()

All %<>% dplyr::select(
  species, virus, infectious.dose.mid, day.PE.mid, ref, experiment    ## covaraites
  , num.Infection    , total.Infection                    ## Infection response
  , num.Dissemination, total.Dissemination                ## Dissemination response
  , num.Transmission , total.Transmission                 ## Transmission response
  ) %>% pivot_longer(
    -c(species, virus, infectious.dose.mid, day.PE.mid, ref, experiment)
  , names_to  = "response"
  , values_to = "value")

All %<>% mutate(
  ## convert names to get unique responses
  type = apply(as.matrix(All$response), 1, FUN = function(x) strsplit(x, "[.]")[[1]][1])
, component = apply(as.matrix(All$response), 1, FUN = function(x) strsplit(x, "[.]")[[1]][2])
) %>% dplyr::select(-response) %>% 
  ## put proportion exposed and proportion positive next to each other
  pivot_wider(values_from = "value", names_from = "type")

## A bit of renaming for analyses
All %<>% rename(
  Mosquito  = species
, Virus     = virus
, dose      = infectious.dose.mid
, day       = day.PE.mid
, Component = component) %>% 
  ## proportion for analysis
  mutate(prop = num / total)

## Dropping Rift Valley Fever Virus
All %<>% filter(Virus != "RVFV")

## Finicky/nitpicky tidying mostly for manuscript figures 
mosq.names <- strsplit(as.character(All$Mosquito), "_") %>% 
  sapply(., FUN = function(x) c(paste(
    paste(x[1], ".", sep = ""), x[2]
    , collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1))
    , substring(x, 2), sep = "", collapse = " "))
All %<>% mutate(Mosquito = mosq.names)

mosq.genus <- strsplit(as.character(All$Mosquito), " ") %>% 
  sapply(., FUN = function(x) x[[1]])

## Complete Genus Names
All %<>% mutate(Genus = mosq.genus) %>% mutate(Genus = plyr::mapvalues(Genus
 , to = c(
    "Aedes"
  , "Anopheles"
  , "Coquillettidia"
  , "Culex"
  , "Mansonia"
  , "Verrallina"
 )
 , from = c("Ae.", "An.", "Cq.", "Cx.", "Ma.", "Ve.")
))

mosq.species <- strsplit(as.character(All$Mosquito), " ") %>% 
  sapply(., FUN = function(x) x[[2]])
All %<>% mutate(Species = mosq.species)

## Virus Family identification
All %<>% mutate(Virus_Family = Virus) %>% 
  mutate(Virus_Family = plyr::mapvalues(Virus_Family
    , from = c(
  "DENV_4", "DENV_3", "DENV_2", "DENV_1", "ZIKV"  , "YFV"   
, "MVEV"  , "KUNV"  , "WNV"   , "JEV"
, "BFV"   , "RRV"   , "CHIKV"
    )
    , to   = c(
      rep("AAFV", 6)
    , rep("CAFV", 4)
    , rep("ARAL", 3)
    )
    ))

## Add research effort (total sample size [tss]) at the level of interest (groupings between mosquito genera and virus families)
All %<>% left_join(.
  , All %>% group_by(Genus, Virus_Family, Component) %>% 
  summarize(tss = sum(total, na.rm = T))) %>%
  ## scale for analysis (as these values get huge)
  mutate(tss  = scale(tss)[, 1])

## Center dose and day within pairing to make intercepts more interpretable
All %<>% mutate(
  dose = scale(dose)[, 1]
, day  = scale(day)[, 1]
)

## Rearrange columns for aesthetics mostly
All %<>% relocate(c(Genus, Species), .after = Mosquito) %>%
  relocate(Virus_Family, .after = Virus)
