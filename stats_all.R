
## quick look at what data exists for infection
All %>% filter(Component == "Infection") %>% {
  ggplot(., aes(x = dose, y = prop)) + geom_point() +
    facet_grid(Genus~Virus_Family, scales = "free")
}

vc.I.data <- All %>% filter(Component == "Infection") %>% mutate(ref = as.factor(ref))

## One of the few [only?] methods of dealing with complete separation with random effects is to go Bayesian
 ## Rank deficient but that's fine, some Genus*Virus_Family simply wont get estimated
vc.I.model <- blme::bglmer(
   ## response in form of cbind(success, failure); equivalent to using the proportion as response and n as weights
   cbind(num, total - num) ~ 
   ## fixed effects
   Genus*Virus_Family + dose + day +
   ## random effects
   (1 | ref) + (1 | Species)
   ## SD of 3 on the fixed effects as suggest in Fox et al. 2015
, fixef.prior = normal(cov = diag(9,15))
, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
, data    = vc.I.data
, family  = "binomial"
  )

## quick look at what data exists for Transmission
All %>% filter(Component == "Transmission") %>% {
  ggplot(., aes(x = day, y = prop)) + geom_point() +
    facet_grid(Genus~Virus_Family, scales = "free")
}

vc.T.data <- All %>% filter(Component == "Transmission") %>% mutate(ref = as.factor(ref))

vc.T.model <- blme::bglmer(
   ## response in form of cbind(success, failure)
   cbind(num, total - num) ~ 
   ## fixed effects
   Genus*Virus_Family + dose + day +
   ## random effects
   (1 | ref) + (1 | Species)
, fixef.prior = normal(cov = diag(9,15))
, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
, data    = vc.T.data
, family  = "binomial"
  )

## Estimates from both models using emmeans
emm_s.I <- emmeans::emmeans(
    vc.I.model
  , ~ Genus | Virus_Family
  , at = list(
    dose = 0
  , day  = 0
    )
  , type = "response")

emm.pred.I <- data.frame(
  prob.I = summary(emm_s.I)$prob 
, lwr.I  = summary(emm_s.I)$asymp.LCL
, upr.I  = summary(emm_s.I)$asymp.UCL
, vir    = rep(sort(unique(vc.I.data$Virus_Family)), each = n_distinct(vc.I.data$Genus))
, mos    = rep(rep(unique(vc.I.data$Genus) %>% sort()), n_distinct(vc.I.data$Virus_Family)))

emm_s.T <- emmeans::emmeans(
    vc.T.model
  , ~ Genus | Virus_Family
  , at   = list(
    day  = 1
  , dose = 0
  )
  , type = "response")

emm.pred.T <- data.frame(
  prob.T = summary(emm_s.T)$prob 
, lwr.T  = summary(emm_s.T)$asymp.LCL
, upr.T  = summary(emm_s.T)$asymp.UCL
, vir    = rep(sort(unique(vc.T.data$Virus_Family)), each = n_distinct(vc.T.data$Genus))
, mos    = rep(rep(unique(vc.T.data$Genus) %>% sort()), n_distinct(vc.T.data$Virus_Family)))

emm.pred <- cbind(emm.pred.I, emm.pred.T[, 1:3])
names(emm.pred)[c(4, 5)] <- c("Virus", "Genus")

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

for_cor.test <- left_join(
(
  vc.I.data %>% dplyr::select(Virus, Mosquito, prop, Genus, Species, Virus_Family, total, num) %>% 
    rename(Infection = prop) %>% ungroup()
)
, 
(
  vc.T.data %>% dplyr::select(Virus, Mosquito, prop, Genus, Species, Virus_Family, total, num) %>% 
    rename(Transmission = prop) %>% ungroup()
)
)

for_cor.test %<>% group_by(
  Mosquito, Genus, Virus, Virus_Family
) %>% summarize(
  mean_inf = mean(Infection, na.rm = T)
, mean_tra = mean(Transmission, na.rm = T)
, sd_inf   = sd(Infection, na.rm = T)
, sd_tra   = sd(Transmission, na.rm = T)
, tot_s    = sum(total, na.rm = T)
, num_s    = sum(num, na.rm = T)
)

plot(for_cor.test$mean_inf, for_cor.test$mean_tra)

with(for_cor.test
, cor.test(mean_inf, mean_tra)
)

## Look at some of the most examined species' infection and transmission across all tested viruses
left_join(
  lm.data.ttt.I %>% group_by(Mosquito) %>% filter(!is.na(Ratio)) %>% 
    mutate(num_vir = length(unique(Virus))) %>% filter(num_vir > 5) %>% summarize(mean_inf = mean(Ratio, na.rm = T))
, lm.data.ttt.T %>% group_by(Mosquito) %>% filter(!is.na(Ratio)) %>% 
    mutate(num_vir = length(unique(Virus))) %>% filter(num_vir > 5) %>% summarize(mean_tra = mean(Ratio, na.rm = T))
) %>% mutate(dif = mean_inf - mean_tra) %>% arrange(desc(dif))

## Simple relationship between infection and transmission
glm.rt <- glm(Infection ~ Transmission
 , family  = "binomial"
 , weights = max_samps
 , data    = for_cor.test)

summary(glm.rt)
with(summary(glm.rt), 1 - deviance/null.deviance)

## Calculate the sample-size weighted proportional change in proportion between each step

maxs.gg %>% 
  group_by(Component) %>% 
  summarize(avg_ratio = weighted.mean(Ratio, max.mosq, na.rm = T)) %>%
  mutate(prop_ratio = avg_ratio / lag(avg_ratio, 1))
