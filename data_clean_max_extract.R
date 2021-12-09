
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
  filter(species != "ae_polynesiensis"
)

## Initiating lists of mosquitoes and viruses
virus_list      <- unique(All$virus) %>% sort()
mosquito_list   <- unique(All$species) %>% sort()
components_list <- c("Infection", "Dissemination", "Transmission")

## Set up containers to hold the results
maxs              <- array(dim = c(length(virus_list), length(mosquito_list), length(components_list)))
dimnames(maxs)    <- list(virus_list, mosquito_list, components_list)
num_exp           <- array(dim = c(length(virus_list), length(mosquito_list), 2, length(components_list)))
dimnames(num_exp) <- list(virus_list, mosquito_list, c("all", "max"), components_list)
num_ind           <- array(dim = c(length(virus_list), length(mosquito_list), 2, length(components_list)))
dimnames(num_ind) <- list(virus_list, mosquito_list, c("all", "max"), components_list)

## And an empty data frame with the correct column names to which rows will be added
all.res       <- data.frame(Mosquito = "", Virus = "", Component = ""
  , dose = 0, day = 0
  , num = 0, prop = 0
  , max_val = 0); all.res <- all.res[-1, ]  

## Find the proportions for all metrics of competence (over k), for all viruses (over v) and species (over m)
for (k in seq_along(components_list)) {
  
  for (v in seq_along(virus_list)) {
 
  tempAll_virus <- All %>% filter(virus == virus_list[[v]])
  
  ## select only those experiments that measured aspects of transmission k
  tempAll <- tempAll_virus %>% filter(
    !is.na(get(paste("total", components_list[[k]], sep = ".")))
  , !is.na(get(paste("num", components_list[[k]], sep = ".")))
    ) %>% 
   ##  Drop those experiments that didn't use at least 5 mosquitoes
  filter(get(paste("total", components_list[[k]], sep = ".")) >= 5)

  ## total count of all mosquitoes infected with this virus and exposed to the virus
  if (nrow(tempAll) > 0) {
    
  all.yes <- tempAll %>% group_by(species) %>%
    filter(get(max(paste("perc", components_list[[k]], sep = "."))) ==
        max(get(paste("perc", components_list[[k]], sep = ".")))) %>% 
    ungroup() %>% dplyr::select(paste("num", components_list[[k]], sep = ".")) %>% sum()
  all.all <- tempAll %>% group_by(species) %>%
    filter(get(max(paste("perc", components_list[[k]], sep = "."))) ==
        max(get(paste("perc", components_list[[k]], sep = ".")))) %>% 
    ungroup() %>% dplyr::select(paste("total", components_list[[k]], sep = ".")) %>% sum()
  
  } else {
  
  all.yes <- 0
  all.all <- 0
   
  }
  
  for (m in seq_along(mosquito_list)) {
    
    species_m <- tempAll %>% filter(species == mosquito_list[[m]])
    
    if (nrow(species_m) > 0) {
    
    all.res.temp <- data.frame(
      Mosquito  = mosquito_list[m]
    , Virus     = virus_list[v]
    , Component = components_list[[k]]
    , dose      = species_m$infectious.dose.mid
    , day       = species_m$day.PE.mid
    , num       = species_m  %>% select(paste("total", components_list[[k]], sep = ".")) %>% unname()
    , prop      = with(species_m, get(paste("num", components_list[[k]], sep = ".")) / get(paste("total", components_list[[k]], sep = ".")))
    ) %>% 
      ## which entry is the max?
      mutate(max_val   = ifelse(prop == max(prop), 1, 0)) 

    all.res <- rbind(all.res, all.res.temp)
    
    }
    
    species_m <- species_m %>% 
      filter(get(max(paste("perc", components_list[[k]], sep = "."))) == 
        max(get(paste("perc", components_list[[k]], sep = "."))))
  
    if (nrow(species_m) > 0) {
    
    ## total count of mosquito species m infected with this virus and exposed to the virus
    species.yes <- species_m  %>% select(paste("num", components_list[[k]], sep = ".")) %>% sum()
    species.all <- species_m  %>% select(paste("total", components_list[[k]], sep = ".")) %>% sum()
    
    ## fill in total sample size
    num_ind[v, m, 1, k] <- sum(all.res.temp$num)
    num_exp[v, m, 1, k] <- nrow(all.res.temp)
    
    ## and max
    num_ind[v, m, 2, k] <- sum((all.res.temp %>% filter(max_val == 1))$num)
    num_exp[v, m, 2, k] <- nrow((all.res.temp %>% filter(max_val == 1)))   
    
    maxs[v, m, k] <- all.res.temp %>% filter(Mosquito == mosquito_list[[m]], Virus == virus_list[[v]]
         , Component == components_list[[k]]) %>% 
         filter(max_val == 1) %>% dplyr::select(prop) %>% unname() %>% unlist() %>% max()
  
    } else {
      
    species.yes  <- 0
    species.all  <- 0
  
    all.res <- rbind(all.res, data.frame(
      Mosquito  = mosquito_list[m]
    , Virus     = virus_list[v]
    , Component = components_list[[k]]
    , dose      = NA
    , day       = NA
    , num       = NA
    , prop      = NA
    , max_val   = NA
    )) 
    
    }
    
    } ## m
  }   ## v
}     ## k


## And a bit of cleaning for stats and plotting
maxs.gg           <- reshape2::melt(maxs)
names(maxs.gg)    <- c("Virus", "Mosquito", "Component", "Ratio")
num_ind.gg        <- reshape2::melt(num_ind)
names(num_ind.gg) <- c("Virus", "Mosquito", "Subset", "Component", "Individuals_Tested")
num_ind.gg        <- num_ind.gg %>% pivot_wider(names_from = Subset, values_from = Individuals_Tested)
num_exp.gg        <- reshape2::melt(num_exp)
names(num_exp.gg) <- c("Virus", "Mosquito", "Subset", "Component", "Experiments")
num_exp.gg        <- num_exp.gg %>% pivot_wider(names_from = Subset, values_from = Experiments)

names(num_ind.gg)[c(4, 5)] <- c("all.mosq", "max.mosq")
names(num_exp.gg)[c(4, 5)] <- c("all.exp", "max.exp")

## Combine everything
maxs.gg           <- left_join(maxs.gg, num_ind.gg) %>% left_join(., num_exp.gg)


