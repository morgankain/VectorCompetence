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

color.repeat <- unlist(apply(matrix(spec.per.genus$num_spec)
  , 1, FUN = function(x) scales::hue_pal()(15)[seq(1, x)])
  )

## 16.28 x 7.33
for_cor.test %>% ungroup() %>% 
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
  , "Arthritogenic alphaviruses"))) %>% {

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
  geom_point(aes(
    colour = Mosquito
  , shape  = Genus)
  , size = 3
    ) +
scale_x_continuous(labels = c(0, 0.25, 0.50, 0.75, 1)
 , limits = c(-0.02, 1.02)
  ) +
scale_y_continuous(labels = c(0, 0.25, 0.50, 0.75, 1)
 , limits = c(-0.02, 1.02)
  ) +
scale_shape_manual(
  values = c(16, 0, 5, 17, 18, 8)
) +
  geom_abline(slope = 1, intercept = 0, lwd = 0.5, linetype = "dashed") +
  theme(
    legend.key.size = unit(.55, "cm")
  , legend.title = element_text(size = 14, face = "bold")
  , legend.text = element_text(size = 14, face = "italic")
  , legend.position = "bottom"
  , axis.title.y.right = element_text(vjust = 2)) +
   xlab("Proportion infected") +
   ylab("Proportion transmitting") + 
    facet_wrap(~Virus_Family
      , nrow = 1
      , labeller = facet_labeller
      ) 
}

## Finding some specific examples of variation to highlight
for_cor.test %>% filter(Virus_Family == "ARAL", Genus == "Aedes") %>% arrange(mean_inf)
vc.I.data %>% filter(Mosquito == "Ae. procax", Virus == "RRV") %>% as.data.frame()
vc.I.data %>% filter(Mosquito == "Ae. aegypti", Virus == "BFV") %>% as.data.frame()

for_cor.test %>% filter(Virus_Family == "CAFV", Genus == "Culex") %>% arrange(mean_tra)
vc.T.data %>% filter(Mosquito == "Cx. sitiens", Virus == "WNV") %>% as.data.frame()
vc.T.data %>% filter(Mosquito == "Cx. annulirostris", Virus == "WNV") %>% as.data.frame()


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
    xlab("Conditional Mode") +
    geom_point(aes(mi_T, Mosquito)
      , colour = "firebrick3"
      , position = position_nudge(x = 0, y = -0.2)) +
    geom_errorbarh(
      aes(xmin = lb_T, xmax = ub_T), height = 0.2
    , position = position_nudge(x = 0, y = -0.2)
    , colour = "firebrick3")
}


