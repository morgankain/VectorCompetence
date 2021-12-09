
### -- max.gg cleanup and figure prep work -- ###

## Finicky/nitpicky tidying mostly for manuscript figures 
mosq.names <- strsplit(as.character(maxs.gg$Mosquito), "_") %>% 
  sapply(., FUN = function(x) c(paste(
    paste(x[1], ".", sep = ""), x[2]
    , collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1))
    , substring(x, 2), sep = "", collapse = " "))
maxs.gg %<>% mutate(Mosquito = mosq.names)

mosq.genus <- strsplit(as.character(maxs.gg$Mosquito), " ") %>% 
  sapply(., FUN = function(x) x[[1]])
maxs.gg %<>% mutate(Genus = mosq.genus)

mosq.species <- strsplit(as.character(maxs.gg$Mosquito), " ") %>% 
  sapply(., FUN = function(x) x[[2]])
maxs.gg %<>% mutate(Species = mosq.species)

## Organize viruses to print in a sensible order
maxs.gg %<>% mutate(
  Virus = factor(Virus, levels = c(
  "RVFV"  , "DENV_4", "DENV_3", "DENV_2", "DENV_1"
, "ZIKV"  , "YFV"   
, "MVEV"  , "KUNV"  , "WNV"   , "JEV"
, "BFV"   , "RRV"   , "CHIKV"
  ))
)

## Sample sizes for Figures 3 and 4
maxs.gg <- maxs.gg %>% mutate(
  mos_max = paste("[", as.character(max.mosq), "]", sep = "")
, mos_sub = paste("{", as.character(all.mosq), "}", sep = "")
  )

maxs.gg <- maxs.gg %>% mutate(
  mos_max = plyr::mapvalues(mos_max, to = NA, from = "[NA]")
, mos_sub = plyr::mapvalues(mos_sub, to = NA, from = "{NA}")
  )

## Color palette for Figures 3 and 4
my_palette <- colorRampPalette(c("lightskyblue", "lightgoldenrod1", "red"))(n = 300)
col_breaks = c(
  seq(0, 0.33    , length = 100)   
, seq(0.34, 0.66 , length = 100)   
, seq(0.67, 1.00, length = 100)
  )  


### -- Research effort and experiment characteristics (all.res) cleanup and figure prep work -- ###

mosq.names <- strsplit(as.character(all.res$Mosquito), "_") %>% 
  sapply(., FUN = function(x) c(paste(
    paste(x[1], ".", sep = ""), x[2]
    , collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1))
    , substring(x, 2), sep = "", collapse = " "))
all.res    <- all.res %>% mutate(Mosquito = mosq.names)

mosq.genus <- strsplit(as.character(all.res$Mosquito), " ") %>% 
  sapply(., FUN = function(x) x[[1]])
all.res <- all.res %>% mutate(Genus = mosq.genus)

all.res <- all.res %>% mutate(Genus = plyr::mapvalues(Genus
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

all.res <- all.res %>% mutate(
  Virus = factor(Virus, levels = c(
  "RVFV"  , "DENV_4", "DENV_3", "DENV_2", "DENV_1"
, "ZIKV"  , "YFV"   
, "MVEV"  , "KUNV"  , "WNV"   , "JEV"
, "BFV"   , "RRV"   , "CHIKV"
  ))
)

## Take averages of experiment characteristics for mosquito-virus combos with multiple experiments with the same
 ## maximum value
maxs.gg.effort <- maxs.gg %>% left_join(.
 , (all.res %>% dplyr::select(Mosquito, Virus, Component, dose, day, num, max_val) %>% 
     filter(max_val == 1) %>%
    group_by(Mosquito, Virus, Component) %>%
    summarize(max.mosq = sum(num, na.rm = T), avg_dose = mean(dose, na.rm = T))
 )
  )

maxs.gg.effort %<>% mutate(Component = factor(Component, levels = c("Infection", "Dissemination", "Transmission")))


### -- Corrleations cleanup and figure prep work -- ###

max_cor <- maxs.gg %>% 
  dplyr::select(Virus, Genus, Species, Mosquito, Component, Ratio, max.mosq) %>% 
  pivot_wider(names_from = Component, values_from = Ratio) %>% 
  group_by(Virus, Mosquito, Genus, Species) %>%
  summarize(
    mean_inf = mean(Infection, na.rm = T)
  , mean_dis = mean(Dissemination, na.rm = T)
  , mean_tra = mean(Transmission, na.rm = T)
  , num.mosq = sum(max.mosq, na.rm = T)
  )

maxs.gg.rank <- maxs.gg %>% filter(!is.na(Ratio)) %>% 
  group_by(Virus, Component) %>% arrange(desc(Ratio)) %>% mutate(Rank = seq(1, n())) %>%
  mutate(stage = as.numeric(Component))

spec.per.genus    <- maxs.gg.rank %>% group_by(Genus) %>% summarize(num_spec = length(unique(Species)))

## More finicky/nitpicky tidying for manuscript figure legends and beautification
spec.per.genus <- maxs.gg.rank %>% group_by(Genus) %>% summarize(num_spec = length(unique(Species)))
color.repeat   <- unlist(apply(matrix(spec.per.genus$num_spec), 1, FUN = function(x) scales::hue_pal()(15)[seq(1, x)]))
override.shape <- rep(as.numeric(as.factor(spec.per.genus$Genus)), spec.per.genus$num_spec)
override.shape <- data.frame(override.shape = override.shape)
override.shape <- override.shape %>% mutate(override.shape
  , mosq_shapes = plyr::mapvalues(override.shape, from = c(1,2,3,4,5,6), to = c(16,0,13,17,1,18))
)
