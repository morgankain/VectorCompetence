## Figure 2

facet_names <- list(
  expression(paste(bolditalic('Aedes-'), bold("associated flaviviruses"), sep = ""))
, expression(paste(bolditalic('Culex-'), bold("associated flaviviruses"), sep = ""))
, expression(bold("Arthritogenic alphaviruses"))
)
facet_labeller <- function(variable,value){
  return(facet_names[value])
}

emm.pred %>% mutate(Virus = plyr::mapvalues(Virus
    , from = c("AAFV", "ARAL", "CAFV")
    , to   = c(
        "Aedes-associated flaviviruses"
      , "Arthritogenic alphaviruses"
      , "Culex-associated flaviviruses"))) %>% 
  mutate(Virus = factor(Virus
  , levels = c(
    "Aedes-associated flaviviruses"
  , "Culex-associated flaviviruses"
  , "Arthritogenic alphaviruses"))) %>% {
  ggplot(., aes(prob.I, prob.T)) +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, linetype = "dashed") +
    geom_errorbar(aes(ymin = lwr.T, ymax = upr.T, colour = Genus), lwd = 0.4, width = 0.03) + 
    geom_errorbarh(aes(xmin = lwr.I, xmax = upr.I, colour = Genus), lwd = 0.4, height = 0.03) +
    geom_point(aes(
      colour = Genus
    , shape  = Genus
      ), size = 3) +
scale_shape_manual(
  values = c(16, 0, 5, 17, 18, 8)
    , labels  = c(
      expression(italic("Aedes"))
    , expression(italic("Anopheles"))
    , expression(italic("Coquillettidia"))
    , expression(italic("Culex"))
    , expression(italic("Mansonia"))
    , expression(italic("Verrallina"))
      )
) +
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

## Figure 5

for_cor.test <- left_join(
(
  vc.I.data %>% dplyr::select(Virus, Mosquito, prop, Genus, Species, Virus_Family, total, num) %>% 
    mutate(meas = "Infection") %>% group_by(
  Mosquito, Genus, Virus, Virus_Family
) %>% summarize(
  mean_inf = mean(prop, na.rm = T)
, sd_inf   = sd(prop, na.rm = T)
)
)
, 
(
  vc.T.data %>% dplyr::select(Virus, Mosquito, prop, Genus, Species, Virus_Family, total, num) %>% 
    mutate(meas = "Transmission") %>% group_by(
  Mosquito, Genus, Virus, Virus_Family
) %>% summarize(
  mean_tra = mean(prop, na.rm = T)
, sd_tra   = sd(prop, na.rm = T)
)
)
)

spec.per.genus <- All %>% group_by(Genus) %>% summarize(num_spec = length(unique(Species)))

color.repeat <- rep(brewer.pal(n = 6, name = "Dark2"), spec.per.genus$num_spec)

gen_abv <- apply(as.matrix(for_cor.test$Mosquito), 1, FUN = function(x) strsplit(x, "[.]")[[1]][1])
for_cor.test %<>% ungroup() %>% mutate(gen_abv = gen_abv)
# for_cor.test %<>% mutate(gen_abv = ifelse(Genus %in% c("Anopheles", "Coquillettidia", "Mansonia", "Verrallina"), gen_abv, NA))

## Add back species
cor.test.spec <- apply(
  for_cor.test$Mosquito %>% as.matrix()
, 1
, FUN = function(x) strsplit(x, "[. ]")[[1]][3]
  )

for_cor.test$Species <- cor.test.spec

## Annoyingly cant figure out how to do this with dplyr; definitely a way...
uni_gen <- unique(for_cor.test$Genus)
for (i in seq_along(uni_gen)) {
  for_cor.test.temp <- for_cor.test %>% filter(Genus == uni_gen[i]) %>% droplevels()
  for_cor.test.temp %<>% mutate(Species = as.factor(Species)) %>%
    mutate(Species = as.numeric(Species))
  if (i == 1) {
    for_cor.test.all <- for_cor.test.temp
  } else {
    for_cor.test.all <- rbind(for_cor.test.all, for_cor.test.temp)
  }
}

## for_cor.test.all %<>% mutate(gen_abv = ifelse(gen_abv %in% c("Ae", "Cx"), NA, gen_abv))

for_cor.test.all %<>% unite(plot_lab, gen_abv, Species, sep = "-")

#for_cor.test.all %<>% mutate(
#  left_brack = "["
#, right_brack = "]"
#, plot_lab_c = plot_lab
#) %>% unite(plot_lab2, left_brack, plot_lab, right_brack, sep = "") %>%
#  unite(Mosquito, plot_lab2, Mosquito, sep = " ")

facet1_names <- c(
  expression(paste(bolditalic('Aedes-'), bold("associated flaviviruses"), sep = ""))
, expression(paste(bolditalic('Culex-'), bold("associated flaviviruses"), sep = ""))
, expression(bold("Arthritogenic alphaviruses"))
)

facet2_names <- c(
  expression(italic("Aedes")), expression(italic("Anopheles")), expression(italic("Coquillettidia"))
, expression(italic("Culex")), expression(italic("Mansonia")), expression(italic("Verrallina"))
)

facet_labeller <- function(variable,value){
  if (variable=='Virus_Family') {
    return(facet1_names[value])
  } else if (variable=='Genus') {
    return(facet2_names[value])
  }
}

color_trial <- c(
    # 14 aedes
    "black", "grey40", "grey80"
  , "mediumpurple3", "deeppink3", "deeppink1"
  , "steelblue3", "steelblue1"
  , "mediumblue", "navy"
  , "turquoise3", "turquoise1"
  , "springgreen3", "springgreen1",
    # 1 anopheles
    "darkseagreen4",
    # 1 Cq
    "darkgreen",
    # 5 Cx
    "darkgoldenrod1", "darkgoldenrod3", "orange", "darkorange2", "darkorange4", 
    # 2 Ma
    "red1", "red3",
    # 3 Ve
    "brown1", "brown4", "black"
  )

## 16.28 x 7.33
for_cor.test.all %>% ungroup() %>% 
  mutate(Virus_Family = plyr::mapvalues(Virus_Family
    , from = c("AAFV", "ARAL", "CAFV")
    , to   = c(
        "Aedes-associated flaviviruses"
      , "Arthritogenic alphaviruses"
      , "Culex-associated flaviviruses"))) %>% 
  mutate(Virus_Family = factor(Virus_Family
  , levels = c(
    "Aedes-associated flaviviruses"
  , "Culex-associated flaviviruses"
  , "Arthritogenic alphaviruses"))) %>% 
  mutate(Virus = plyr::mapvalues(Virus
    , from = c("DENV_1", "DENV_2", "DENV_3", "DENV_4")
    , to   = c("DENV-1", "DENV-2", "DENV-3", "DENV-4"))) %>% 
  mutate(Virus = factor(Virus
    , levels = c(
  "DENV-4", "DENV-3", "DENV-2", "DENV-1", "ZIKV"  , "YFV"   
, "MVEV"  , "KUNV"  , "WNV"   , "JEV"
, "BFV"   , "RRV"   , "CHIKV", "RVFV"  
    )
    )) %>% group_by(Genus) %>% mutate(
      Mosquito = factor(Mosquito, levels = unique(Mosquito))
    ) %>% {
ggplot(., aes(mean_inf, mean_tra)) +
  geom_errorbar(aes(
    ymin = mean_tra - sd_tra
  , ymax = mean_tra + sd_tra
  , colour = Mosquito)
  , alpha = 0.4
    ) +
  geom_errorbarh(aes(
    xmin = mean_inf - sd_inf
  , xmax = mean_inf + sd_inf
  , colour = Mosquito)
  , alpha = 0.4
    ) +
        
#  geom_text(
#    aes(
#      label  = plot_lab_c
#    , colour = Mosquito
#    )
#  , fontface = "bold"
# ,  show.legend = FALSE
#  ) +
  
  geom_point(aes(
    colour = Mosquito
  , shape  = Virus)
  , size = 4
    ) +
  
# geom_text(aes(label = gen_abv), hjust = -0.2, vjust = 1.2) +
        
scale_colour_manual(
 values = c(
   colorRampPalette(brewer.pal(name="Paired", n = 12))(14) %>% sample()
 , brewer.pal(name="Dark2", n = 3)[1]
 , brewer.pal(name="Dark2", n = 3)[1]
 , brewer.pal(name="Dark2", n = 5)
 , brewer.pal(name="Dark2", n = 3)[c(1, 2)]
 , brewer.pal(name="Dark2", n = 3)
 )
) +

scale_x_continuous(labels = c(0, 0.25, 0.50, 0.75, 1)
 , limits = c(-0.02, 1.02)
  ) +
scale_y_continuous(labels = c(0, 0.25, 0.50, 0.75, 1)
 , limits = c(-0.02, 1.02)
  ) +
scale_shape_manual(
  values = c(
    0, 1, 2, 5, 15, 16
  , 15, 16, 17, 18
  , 15, 16, 17 
)
#, label  = c(
#  "[AAFV] DENV-4"
#, "[AAFV] DENV-3"
#, "[AAFV] DENV-2"
#, "[AAFV] DENV-1"
#, "[AAFV] ZIKV"
#, "[AAFV] YFV"
#, "[CAFV] MVEV"
#, "[CAFV] KUNV"
#, "[CAFV] WNV"
#, "[CAFV] JEV"
#, "[ArAl] BFV"
#, "[ArAl] RRV"
#, "[ArAl] CHIKV"
#)
  ) +
  geom_abline(slope = 1, intercept = 0, lwd = 0.5, linetype = "dashed") +
  theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14, face = "bold")
  , legend.text = element_text(size = 14, face = "italic")
 # , legend.position = "bottom"
  , legend.position = "right"
  , axis.title.y.right = element_text(vjust = 2)
  , strip.text.x = element_text(size = 11)
  , strip.text.y = element_text(size = 14)) +
   xlab("Proportion infected") +
   ylab("Proportion transmitting") + 
#    facet_wrap(~Virus_Family
#      , nrow = 1
#      , labeller = facet_labeller
#      ) +
    facet_grid(Genus~Virus_Family
      , labeller = labeller(
         .rows  = as_labeller(facet2_names)
      ,  .cols  = as_labeller(facet1_names)
      )) +
    guides(colour = guide_legend(
      nrow = 26
    , byrow = F
    , override.aes = list(size = 3)))
}

## Finding some specific examples of variation to highlight
for_cor.test %>% filter(Virus_Family == "ARAL", Genus == "Aedes") %>% arrange(mean_inf)
vc.I.data %>% filter(Mosquito == "Ae. procax", Virus == "RRV") %>% as.data.frame()
vc.I.data %>% filter(Mosquito == "Ae. aegypti", Virus == "BFV") %>% as.data.frame()

for_cor.test %>% filter(Virus_Family == "CAFV", Genus == "Culex") %>% arrange(mean_tra)
vc.T.data %>% filter(Mosquito == "Cx. sitiens", Virus == "WNV") %>% as.data.frame()
vc.T.data %>% filter(Mosquito == "Cx. annulirostris", Virus == "WNV") %>% as.data.frame()

## checking number of data points with an infection or transmission measure but not both
for_cor.test %>% mutate(
  inf_meas = ifelse(is.na(mean_inf), 1, 0)
, tra_meas = ifelse(is.na(mean_tra), 1, 0)) %>%
  mutate(
  one_meas = inf_meas + tra_meas
  ) %>% filter(one_meas == 1)

## Supplemental Figure for the individual deviates

library(stevemisc)

test_gg.I <- stevemisc::show_ranef(vc.I.model, "Species")
test_gg.T <- stevemisc::show_ranef(vc.T.model, "Species")
ranefs.I  <- test_gg.I$data 
ranefs.T  <- test_gg.T$data 

ranefs.I %<>% left_join(.
  , 
vc.I.data %>% group_by(Species) %>% slice(1) %>% 
  dplyr::select(Species, Mosquito) %>% 
    rename(grp = Species)
)

ranefs.T %<>% left_join(.
  , 
vc.T.data %>% group_by(Species) %>% slice(1) %>% 
  dplyr::select(Species, Mosquito) %>% 
    rename(grp = Species)
)

ranefs.I %<>% dplyr::select(-grpvar, -term, -grp, -condsd) %>% 
  rename(mi_I = condval, ub_I = ub, lb_I = lb)

ranefs.T %<>% dplyr::select(-grpvar, -term, -grp, -condsd) %>% 
  rename(mi_T = condval, ub_T = ub, lb_T = lb)

ranefs <- left_join(ranefs.I, ranefs.T)

ranefs %<>% arrange(mi_I) %>% mutate(
  Mosquito = factor(Mosquito, levels = Mosquito)
)

ranefs %>% {
  ggplot(., aes(mi_I, Mosquito)) + 
    geom_point(position = position_nudge(x = 0, y = 0.2)) +
    geom_errorbarh(
      aes(xmin = lb_I, xmax = ub_I), height = 0.2
    , position = position_nudge(x = 0, y = 0.2)) +
    xlab("Conditional mode of the 'Species' level random effect") +
    geom_point(aes(mi_T, Mosquito)
      , colour = "firebrick3"
      , position = position_nudge(x = 0, y = -0.2)) +
    geom_errorbarh(
      aes(xmin = lb_T, xmax = ub_T), height = 0.2
    , position = position_nudge(x = 0, y = -0.2)
    , colour = "firebrick3") +
    ylab("Mosquito Species")
}
