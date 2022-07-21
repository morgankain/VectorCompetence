
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

## Fit glmms for aedes
source("aedes_glmm.R")
