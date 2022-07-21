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


## -- Figure 3 -- ##

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

### --- Supplemental Figures --- ###

## S1 and S2 residual vs fitted plots
data.frame(
  resids = residuals(vc.I.model)
, fitted = fitted(vc.I.model)
) %>% {
  ggplot(., aes(fitted, resids)) + 
    geom_point() + 
    geom_smooth() +
    geom_hline(yintercept = 0) + 
    ggtitle("Infection") +
    xlab("Fitted Values") +
    ylab("Residuals")
}

data.frame(
  resids = residuals(vc.T.model)
, fitted = fitted(vc.T.model)
) %>% {
  ggplot(., aes(fitted, resids)) + 
    geom_point() + 
    geom_smooth() +
    geom_hline(yintercept = 0) + 
    ggtitle("Transmission") +
    xlab("Fitted Values") +
    ylab("Residuals")
}

## Figures 3,4 from first submission, now supplemental A (note: panels A and B combined in Keynote)

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

## B (note: panels A and B combined in Keynote)

### 1350 x 900
maxs.gg %>% filter(Component == "Infection") %>% 
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
  geom_vline(xintercept = 14.5, linetype = "dotted") +
  geom_vline(xintercept = 15.5, linetype = "dotted") +
  geom_vline(xintercept = 16.5, linetype = "dotted") +
  geom_vline(xintercept = 21.5, linetype = "dotted") +
  geom_vline(xintercept = 23.5, linetype = "dotted") +
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

## Supplemental Figure for the individual deviates (S5)

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

## Figures S6-S9 (like Figures 3 and 4, minor adjustments needed here to get each of S3-S6)

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

### Figure S10 (Note, wont work until the bootMer is run, which takes about 2h per model)

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
    filter(Dose == 7.0, Day <= 18) %>% {
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
    filter(Dose == 7.0, Day <= 18) %>% {
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

## Figure S11

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


## Figure S13
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


