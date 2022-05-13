
### --- Main Test Figures --- ###

## -- Figure 2 -- ##

facet_names <- list(
  expression(paste(bolditalic('Aedes-'), bold("associated flaviviruses"), sep = ""))
, expression(paste(bolditalic('Culex-'), bold("associated flaviviruses"), sep = ""))
, expression(bold("Arthritogenic alphaviruses"))
)
facet_labeller <- function(variable,value){
  return(facet_names[value])
}

emm.pred %>% mutate(Virus = factor(Virus
  , levels = c(
    "Haemorrhagic flaviviruses"
  , "Encephalitogenic flaviviruses"
  , "Arthritogenic alphaviruses"))) %>% 
  mutate(Virus = plyr::mapvalues(Virus
    , from = c("Haemorrhagic flaviviruses"
      , "Encephalitogenic flaviviruses")
    , to   = c("Aedes-associated flaviviruses"
      , "Culex-associated flaviviruses"))) %>% {
  ggplot(., aes(prob.I, prob.T)) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, linetype = "dashed") +
    geom_errorbar(aes(ymin = lwr.T, ymax = upr.T, colour = Genus), lwd = 0.4, width = 0.03) + 
    geom_errorbarh(aes(xmin = lwr.I, xmax = upr.I, colour = Genus), lwd = 0.4, height = 0.03) +
    geom_point(aes(colour = Genus), size = 3) +
    scale_y_continuous(limits = c(-0.035, 1)
      , breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)
      , labels = c(0, 0.25, 0.50, 0.75, 1)) +
    scale_x_continuous(limits = c(-0.035, 1)
      , breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)
      , labels = c(0, 0.25, 0.50, 0.75, 1)) + 
    scale_colour_brewer(
      palette = "Dark2"
    , labels  = c(
      expression(italic("Aedes"))
    , expression(italic("Anopheles"))
    , expression(italic("Coquillettidia"))
    , expression(italic("Culex"))
    , expression(italic("Mansonia"))
    , expression(italic("Verrallina"))
      )) +
    xlab("Proportion infected") +
    ylab("Propotion transmitting") +
    facet_wrap(~Virus, nrow = 1
    , labeller = facet_labeller
      ) +
    theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 14)
  , legend.text.align = 0
  , legend.position = "left"
  , axis.title.y.right = element_text(vjust = 2))
      }

## Figure 3,4 A (note: panels A and B combined in Keynote)

maxs.gg.s <- maxs.gg %>% 
  group_by(Component, Virus) %>% 
  summarise(
    mean_comp = weighted.mean(Ratio, weights = max.mosq, na.rm = T)
  , lwr       = quantile(Ratio, 0.2, na.rm = T)
  , upr       = quantile(Ratio, 0.8, na.rm = T)
  )

## for 3 and 4 minor adjustments to this is needed (e.g., Infection vs Transmission and axis labels)

## 306 x 775 ; 3.20 x 8.08
maxs.gg.s %>% filter(Component == "Transmission") %>% 
  mutate(Virus = plyr::mapvalues(Virus
    , from = c("DENV_1", "DENV_2", "DENV_3", "DENV_4")
    , to   = c("DENV-1", "DENV-2", "DENV-3", "DENV-4"))) %>% 
mutate(
  Virus = factor(Virus, levels = rev(c(
  "DENV-4", "DENV-3", "DENV-2", "DENV-1"
, "ZIKV"  , "YFV"   
, "MVEV"  , "KUNV"  , "WNV"   , "JEV"
, "BFV"   , "RRV"   , "CHIKV"
, "RVFV"  
  ))
  )
) %>% 
  filter(Virus != "RVFV") %>% {
ggplot(., aes(mean_comp, Virus)) +
    geom_point() +
    geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.3) +
  scale_x_continuous(position = "top", limits = c(0, 1)
    , breaks = c(0, 0.25, 0.50, 0.75, 1.00), labels = c(0, 0.25, 0.50, 0.75, 1)) +
  scale_y_discrete(
    labels = rev(c(
 "DENV-4", "DENV-3", "DENV-2", "DENV-1"
, "ZIKV"  , "YFV"   
, "MVEV"  
, expression("WNV"["KUNV"])  
, expression("WNV"["NY99"])    
, "JEV"
, "BFV"   , "RRV"   , "CHIKV"
    )
  )) +
  ylab("Virus") +
  xlab("Proportion") +
  theme(
    legend.key.size = unit(1.0, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 16)
  , axis.text.x = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  , panel.border = element_blank()
  , axis.line = element_line(colour = "black")
  , panel.background = element_blank()) +
  geom_hline(yintercept = 7.5, linetype = "dotted") +
  geom_hline(yintercept = 3.5, linetype = "dotted") 
  }


## Figure 3,4 B (note: panels A and B combined in Keynote)

### 1350 x 900
maxs.gg %>% filter(Component == "Transmission") %>% 
  mutate(Virus = plyr::mapvalues(Virus
    , from = c("DENV_1", "DENV_2", "DENV_3", "DENV_4")
    , to   = c("DENV-1", "DENV-2", "DENV-3", "DENV-4"))) %>% 
mutate(
  Virus = factor(Virus, levels = rev(c(
  "DENV-4", "DENV-3", "DENV-2", "DENV-1"
, "ZIKV"  , "YFV"   
, "MVEV"  , "KUNV"  , "WNV"   , "JEV"
, "BFV"   , "RRV"   , "CHIKV"
, "RVFV"  
  ))
  )
) %>% 
  filter(Virus != "RVFV") %>% {
ggplot(., aes(Mosquito, Virus, Z = Ratio)) + 
  geom_raster(aes(
    fill  = Ratio
    ), alpha = 0.75) +
    
#  geom_tile(aes(fill = Ratio, size = Individuals_Tested), colour = "black") +
#  geom_text(aes(label = round(Ratio, 2)), nudge_y = 0.3, fontface = "bold") +
#  geom_text(aes(label = mos_max), nudge_y = 0, size = 3) +
#  geom_text(aes(label = mos_sub), nudge_y = -0.3, size = 3) +

  geom_text(aes(label = round(Ratio, 2)), nudge_y = 0.2, fontface = "bold") +
  geom_text(aes(label = mos_max), nudge_y = -0.2, size = 3.5) +
# geom_text(aes(label = separ), nudge_y = -0.13, size = 4) +
# geom_text(aes(label = mos_sub), nudge_y = -0.3, size = 3.5) +
    
# guides(fill  = guide_colorbar(title = "Risk Ratio")) +
  guides(fill  = guide_colorbar(title = "Proportion")) +
  scale_y_discrete(
    labels = rev(c(
 "DENV-4", "DENV-3", "DENV-2", "DENV-1"
, "ZIKV"  , "YFV"   
, "MVEV"  
, expression("WNV"["KUNV"])  
, expression("WNV"["NY99"])    
, "JEV"
, "BFV"   , "RRV"   , "CHIKV"
    )
  )) +
  scale_fill_gradientn(
  values  = scales::rescale(col_breaks, c(0, 1))
# , breaks = c(0, 1, 2, 5, 10)
, breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)
, colours = my_palette
, na.value = "white"
    ) +
  geom_hline(yintercept = 3.5, linetype = "dotted") +
  geom_hline(yintercept = 7.5, linetype = "dotted") +
  geom_vline(xintercept = 15.5, linetype = "dotted") +
  geom_vline(xintercept = 16.5, linetype = "dotted") +
  geom_vline(xintercept = 17.5, linetype = "dotted") +
  geom_vline(xintercept = 22.5, linetype = "dotted") +
  geom_vline(xintercept = 24.5, linetype = "dotted") +
  xlab("Mosquito species") +
  theme(
    legend.key.size = unit(1.0, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 14, angle = 310, hjust = 0, face = "italic") 
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  , panel.border = element_blank()
  , panel.background = element_blank()
  )
  }


## Figure 5

spec.per.genus <- All %>% group_by(Genus) %>% summarize(num_spec = length(unique(Species)))

color.repeat <- unlist(apply(matrix(spec.per.genus$num_spec)
  , 1, FUN = function(x) scales::hue_pal()(15)[seq(1, x)])
  )

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

max_cor <- max_cor %>% mutate(Mosquito = factor(Mosquito
  , levels = unique(max_cor$Mosquito)))

color.labels <- unique(max_cor$Mosquito)

max_cor %>% mutate(Virus_Group = plyr::mapvalues(Virus
    , to   = c(
      "Arthritogenic alphaviruses"
    , "Arthritogenic alphaviruses"
    , "Arthritogenic alphaviruses"
    , "Encephalitogenic flaviviruses"
    , "Encephalitogenic flaviviruses"
    , "Encephalitogenic flaviviruses"
    , "Encephalitogenic flaviviruses"
    , "Haemorrhagic flaviviruses"
    , "Haemorrhagic flaviviruses"
    , "Haemorrhagic flaviviruses"
    , "Haemorrhagic flaviviruses"
    , "Haemorrhagic flaviviruses"
    , "Haemorrhagic flaviviruses"
    , "Phlebovirus"
      )
    , from = c(
      "CHIKV"
    , "RRV"
    , "BFV"
    , "JEV"
    , "WNV"
    , "KUNV"
    , "MVEV"
    , "YFV"
    , "ZIKV"
    , "DENV_1"
    , "DENV_2"
    , "DENV_3"
    , "DENV_4"
    , "RVFV"
    ))) %>% filter(Virus_Group != "Phlebovirus") %>% mutate(Genus = plyr::mapvalues(Genus
 , to = c(
    "Aedes"
  , "Anopheles"
  , "Coquillettidia"
  , "Culex"
  , "Mansonia"
  , "Verrallina"
 )
 , from = c("Ae", "An", "Cq", "Cx", "Ma", "Ve")
)) %>% mutate(Virus_Group = plyr::mapvalues(Virus_Group
    , from = c("Haemorrhagic flaviviruses", "Encephalitogenic flaviviruses")
    , to   = c("Aedes-borne flaviviruses", "Culex-borne flaviviruses"))) %>% 
  droplevels( ) %>% {

ggplot(., aes(mean_inf, mean_tra)) +
  geom_point(aes(
    size   = num.mosq * 5
  , colour = interaction(Species, Genus)
  , shape  = Genus)
    ) +

   scale_color_manual(
    values = color.repeat
  , labels = color.labels
  , name   = "Mosquito
species") +

    scale_shape_manual(
    values = unique(override.shape$mosq_shapes)
  , guide = FALSE) +

  scale_size_continuous(name = "Number of
mosquitoes"
    , breaks = c(10, 50, 100, 200, 400, 600)
    , range  = c(2, 12)) +

    scale_x_continuous(labels = c(0, 0.25, 0.50, 0.75, 1)
 , limits = c(-0.02, 1.02)
  ) +
scale_y_continuous(labels = c(0, 0.25, 0.50, 0.75, 1)
 , limits = c(-0.02, 1.02)
  ) +
  
        guides(
    colour = guide_legend(
        override.aes = list(
        shape = override.shape$mosq_shapes
      , size  = 4))) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, linetype = "dashed") +
  theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14, face = "bold")
  , legend.text = element_text(size = 14, face = "italic")
  , legend.position = "bottom"
  , axis.title.y.right = element_text(vjust = 2)) +
   xlab("Proportion infected") +
   ylab("Proportion transmitting") + 
        
    facet_wrap(~Virus_Group
      , nrow = 1
      , labeller = facet_labeller
      ) 
  }

### --- Supplemental Figures --- ###

## Figure S1
## 13 x 9.5
ttest <- all.res %>% 
  group_by(Mosquito, Component, Virus) %>% 
  filter(Component != "Dissemination") %>%
  dplyr::summarize(
    num = sum(num, na.rm = T)
  ) %>% 
  ungroup(Virus) %>%
  filter(!is.na(num)) %>%
  pivot_wider(names_from = "Component", values_from = c("num"))

ggplot(ttest, aes(Infection, Transmission)) +
  geom_point(lwd = 2) + 
  facet_wrap(~Mosquito) +
  scale_y_continuous(trans = "pseudo_log", breaks = c(0, 10, 100, 1000)) +
  scale_x_continuous(trans = "pseudo_log", breaks = c(0, 10, 100, 1000)) +
  xlab("Experimental mosquitoes for infection") +
  ylab("Experimental mosquitoes for transmission") +
  theme(
    legend.key.size = unit(.75, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 14)
  , strip.text.x = element_text(face = "italic")
    ) 

## Figure S2

maxs.gg.effort <- maxs.gg %>% left_join(.
 , (all.res %>% dplyr::select(Mosquito, Virus, Component, dose, day, num, max_val) %>% 
     filter(max_val == 1) %>%
    group_by(Mosquito, Virus, Component) %>%
    summarize(max.mosq = sum(num, na.rm = T), avg_dose = mean(dose, na.rm = T))
 )
  )

maxs.gg.effort$Component <- factor(maxs.gg.effort$Component, levels = c("Infection", "Dissemination", "Transmission"))

maxs.gg.effort %>% dplyr::select(Virus, Mosquito, Component, all.mosq, Ratio, avg_dose) %>% {
  ggplot(., aes(all.mosq, Ratio)) +
    geom_point(aes(colour = avg_dose), lwd = 3) +
    xlab("Total number of experimental mosquitoes") +
    ylab("Maximum detected proportion") +
    scale_x_log10() +
    scale_colour_gradient2(
      name = "Average Dose"
    , low  = "seagreen1"
    , mid  = "sienna2"
    , high = "magenta4"
    , midpoint = 8
    ) +
    theme(
    legend.key.size = unit(.75, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 14)
      ) +
    facet_wrap(~Component)
}

## Figures S3-S6 (like Figures 3 and 4, minor adjustments needed here to get each of S3-S6)

facet_names <- list(
  "DENV-4"
, "DENV-3"
, "DENV-2"
, "DENV-1"
, "ZIKV"
, "YFV"
, "MVEV"
, expression(bold("WNV"["KUNV"]))
, expression(bold("WNV"["NY99"]))
, "JEV"
, "BFV"
, "RRV"
, "CHIKV"
)
facet_labeller <- function(variable,value){
  return(facet_names[value])
}

which_component <- "Transmission"

## 11.7 x 11.7
all.res %>% filter(!is.na(max_val)
  , Component == which_component) %>%
  mutate(Mosquito = factor(Mosquito, levels = rev(sort(unique(all.res$Mosquito))))) %>% 
  filter(Virus != "RVFV") %>% droplevels() %>%
  {
ggplot(., aes(dose, Mosquito)) +
   geom_point(aes(lwd = num, colour = as.factor(max_val))
     , alpha = 0.75) +
    scale_color_manual(
      values = c("dodgerblue3", "firebrick3")
, name = "Maximum
number
transmitting?"
      , labels = c("No", "Yes")
      ) +
    scale_size_continuous(name = "Number of
mosquitoes
tested"
     # , breaks = c(10, 50, 100, 200, 400)
      , breaks = c(10, 25, 50, 75)
      ) +
    scale_shape_manual(values = c(16, 2), name = "Maximum
number
transmitting?"
      , labels = c("No", "Yes")) +
    ylab("Mosquito species") +
#    xlab("Days post exposure") +
 #   scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
    xlab(expression(Infectious~dose~(log[10]~IU/mL))) +
 #  geom_hline(data = (all.res.mean.all %>% filter(Component == which_component)), aes(yintercept = mean_day), linetype = "dotted") +
 #  geom_hline(data = (all.res.mean.max %>% filter(Component == which_component)), aes(yintercept = mean_dose), linetype = "dotted") +
    guides(colour = guide_legend(
        override.aes = list(size = 4))
      , shape = guide_legend(
        override.aes = list(size = 4))) +
  geom_hline(yintercept = 12.5, linetype = "dotted") +
  geom_hline(yintercept = 11.5, linetype = "dotted") +
  geom_hline(yintercept = 10.5, linetype = "dotted") +
  geom_hline(yintercept = 5.5, linetype = "dotted") +
  geom_hline(yintercept = 3.5, linetype = "dotted") +
    facet_wrap(~Virus, ncol = 5
    , labeller = facet_labeller
      ) +
      theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12)#, angle = 310, hjust = 0) 
  , axis.text.y = element_text(size = 10, face = "italic") 
  , axis.title.x = element_text(size = 12) 
  , axis.title.y = element_text(size = 12)
  )
  }


### Figure S7 (Note, wont work until the bootMer is run, which takes about 2h per model)

virus_plotting <- c("CHIKV", "DENV-3", "ZIKV")

## Infection
out.gg <- readRDS("inf.boot.Rds")

All.s <- All %>% filter(!is.na(infectious.dose.mid), !is.na(day.PE.mid), !is.na(total.infect))

All.lme4.data <- (All.s %>% filter(species == "ae_aegypti"))
virus_range   <- unique(All.lme4.data$virus)
day_range     <- seq(
  min(unique(All.lme4.data$day.PE.mid))
, max(unique(All.lme4.data$day.PE.mid))
, 1
)
dose_range    <- seq(
  min(unique(All.lme4.data$infectious.dose.mid))
, max(unique(All.lme4.data$infectious.dose.mid))
, .3
)

n_samps     <- 1000
dose_range  <- seq(1, 10, by = 0.5)
median_day  <- All.s %>% filter(species == "ae_aegypti", !is.na(total.infect), !is.na(num.infected)) %>% 
  group_by(virus) %>%
  summarize(median_day = median(day.PE.mid))

out.vals <- expand.grid(
      day.PE.mid          = day_range
    , virus               = virus_range
    , infectious.dose.mid = dose_range
      )

out.vals <- cbind(out.vals, t(out.gg$t))
out.vals <- reshape2::melt(out.vals
  , c("day.PE.mid", "virus", "infectious.dose.mid"))
out.vals <- out.vals %>% rename(iter = variable)
out.vals <- out.vals %>% group_by(
  day.PE.mid, virus, infectious.dose.mid
) %>% summarize(
  lwr   = quantile(value, c(0.025))
, lwr_n = quantile(value, c(0.1))
, mid   = quantile(value, c(0.5))
, upr_n = quantile(value, c(0.9))
, upr   = quantile(value, c(0.975))
)
out.vals <- out.vals %>% rename(
  Day   = day.PE.mid
, Dose  = infectious.dose.mid
) %>% mutate(
  virus = plyr::mapvalues(virus, from = c("DENV_3"), to = c("DENV-3"))
)

(inf.plot <- out.vals %>% filter(virus %in% virus_plotting) %>%
    filter(Day == 12) %>% droplevels() %>% {
ggplot(
  .
, aes(Dose, mid)) + 
  geom_line(lwd = 0.5) +
  geom_ribbon(aes(x = Dose, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_point(
    data = (All.s %>% filter(species == "ae_aegypti") %>% 
        mutate(virus = plyr::mapvalues(virus, from = c("DENV_3"), to = c("DENV-3"))) %>% 
        filter(virus %in% virus_plotting)) %>% droplevels()
    , aes(infectious.dose.mid, num.infected / total.infect
    , size   = total.infect
    , colour = day.PE.mid
  ))  + 
  scale_colour_gradient(low = "deepskyblue1", high = "firebrick3", name = "Days
post
exposure       ") +
  scale_size_continuous(name = "Mosquitoes
tested") +
  facet_wrap(~virus) +
  xlab(expression(Infectious~dose~(log[10]~IU/mL))) +
  ylab("Proportion infected") +
  theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 16)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  , panel.spacing = unit(1, "lines")
  )
})

## Dissemination
out.gg <- readRDS("dis.boot.Rds")

All.s <- All %>% filter(!is.na(infectious.dose.mid), !is.na(day.PE.mid), !is.na(total.dissem))
n_samps      <- 1000
day_range    <- seq(1, 25, by = 0.5)
virus_range  <- unique((All.s %>% filter(species == "ae_aegypti"))$virus)
median_dose  <- All.s %>% filter(species == "ae_aegypti", !is.na(total.dissem), !is.na(num.disseminated)) %>%
  group_by(virus) %>% 
  summarize(median_dose = median(infectious.dose.mid))

out.vals <- expand.grid(
      day.PE.mid          = day_range
    , virus               = virus_range
    , infectious.dose.mid = dose_range
      )

out.vals <- cbind(out.vals, t(out.gg$t))
out.vals <- reshape2::melt(out.vals
  , c("day.PE.mid", "virus", "infectious.dose.mid"))
out.vals <- out.vals %>% rename(iter = variable)
out.vals <- out.vals %>% group_by(
  day.PE.mid, virus, infectious.dose.mid
) %>% summarize(
  lwr   = quantile(value, c(0.025))
, lwr_n = quantile(value, c(0.1))
, mid   = quantile(value, c(0.5))
, upr_n = quantile(value, c(0.9))
, upr   = quantile(value, c(0.975))
)
out.vals <- out.vals %>% rename(
  Day   = day.PE.mid
, Dose  = infectious.dose.mid
) %>% mutate(
  virus = plyr::mapvalues(virus, from = c("DENV_3"), to = c("DENV-3"))
)

(dis.plot <- out.vals %>% filter(virus %in% virus_plotting) %>% 
    filter(Dose == 7.0) %>% {
ggplot(
  .
, aes(Day, mid)) + 
  geom_line(lwd = 0.5) +
  geom_ribbon(aes(x = Day, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_point(
    data = (All.s %>% filter(species == "ae_aegypti") %>% 
        mutate(
  virus = plyr::mapvalues(virus, from = c("DENV_3"), to = c("DENV-3"))
) %>%
        filter(virus %in% virus_plotting))
    , aes(day.PE.mid, num.disseminated / total.dissem
    , size   = total.infect
    , colour = infectious.dose.mid
  )) + 
  scale_colour_gradient(low = "olivedrab2", high = "mediumorchid4"
  , name = expression(
    atop(
      "Infectious dose"
    , (log[10]~IU/mL)
    )
    )
    ) +
  scale_size_continuous(name = "Mosquitoes
tested") +
  facet_wrap(~virus) +
    guides(size = FALSE) +
  xlab("Days post exposure") + 
  ylab("Proportion with disseminated infection") +
  theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 16)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  , panel.spacing = unit(1, "lines")
  )

})

## Transmission
out.gg <- readRDS("tra.boot.Rds")

All.s <- All %>% filter(!is.na(infectious.dose.mid), !is.na(day.PE.mid), !is.na(total.transmitted))

n_samps     <- 1000
day_range   <- seq(1, 25, by = 0.5)
virus_range <- unique((All.s %>% filter(species == "ae_aegypti"))$virus)
median_dose  <- All.s %>% filter(species == "ae_aegypti", !is.na(total.transmitted), !is.na(num.transmitted)) %>%
  group_by(virus) %>% 
  summarize(median_dose = median(infectious.dose.mid))

out.vals <- expand.grid(
      day.PE.mid          = day_range
    , virus               = virus_range
    , infectious.dose.mid = dose_range
      )

out.vals <- cbind(out.vals, t(out.gg$t))
out.vals <- reshape2::melt(out.vals
  , c("day.PE.mid", "virus", "infectious.dose.mid"))
out.vals <- out.vals %>% rename(iter = variable)
out.vals <- out.vals %>% group_by(
  day.PE.mid, virus, infectious.dose.mid
) %>% summarize(
  lwr   = quantile(value, c(0.025))
, lwr_n = quantile(value, c(0.1))
, mid   = quantile(value, c(0.5))
, upr_n = quantile(value, c(0.9))
, upr   = quantile(value, c(0.975))
)
out.vals <- out.vals %>% rename(
  Day   = day.PE.mid
, Dose  = infectious.dose.mid
) %>% mutate(
  virus = plyr::mapvalues(virus, from = c("DENV_3"), to = c("DENV-3"))
)

(tra.plot <- out.vals %>% filter(virus %in% virus_plotting) %>% 
    filter(Dose == 7.0) %>% {
ggplot(
  .
, aes(Day, mid)) + 
  geom_line(lwd = 0.5) +
  geom_ribbon(aes(x = Day, ymin = lwr, ymax = upr), alpha = 0.25) +
  scale_size_continuous(name = "Mosquitoes
tested") +
  geom_point(
    data = (All.s %>% filter(species == "ae_aegypti") %>% 
        mutate(
  virus = plyr::mapvalues(virus, from = c("DENV_3"), to = c("DENV-3"))
) %>%
        filter(virus %in% virus_plotting))
    , aes(day.PE.mid, num.transmitted / total.transmitted
    , size   = total.infect
    , colour = infectious.dose.mid
  )) + 
  scale_colour_gradient(low = "olivedrab2", high = "mediumorchid4"
  , name = expression(
    atop(
      "Infectious dose"
    , (log[10]~IU/mL)
    )
    )) +
  facet_wrap(~virus) +
      guides(size = FALSE) +
  xlab("Days post exposure") + 
  ylab("Proportion transmitting") +
  theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 16)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  , panel.spacing = unit(1, "lines")
  )

})

inf.plot <- inf.plot + scale_colour_gradient(low = "deepskyblue1", high = "firebrick3"
, name = "Days
post
exposure          ")


gridExtra::grid.arrange(
  inf.plot
, dis.plot
, tra.plot
, nrow = 3
)
