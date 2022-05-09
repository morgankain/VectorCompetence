
## quick look at what data exists for infection
All %>% filter(Component == "Infection") %>% {
  ggplot(., aes(x = dose, y = prop)) + geom_point() +
    facet_grid(Genus~Virus_Family, scales = "free")
}

## Subset 
vc.I.data <- All %>% filter(Component == "Infection") %>% 
  mutate(ref = as.factor(ref)) %>% filter(Genus != "Anopheles")   

vc.I.model <- glmer(
   ## response in form of cbind(success, failure)
   cbind(num, total - num) ~ 
   ## fixed effects
   Genus*Virus_Family + dose +
   ## random effects
   (1 | ref) + (1 | Species)
, data    = vc.I.data
, family  = "binomial"
  )

vc.I.data2 <- All %>% filter(Component == "Infection") %>% mutate(ref = as.factor(ref))

vc.I.model2 <- blme::bglmer(
   ## response in form of cbind(success, failure)
   cbind(num, total - num) ~ 
   ## fixed effects
   Genus*Virus_Family + dose +
   ## random effects
   (1 | ref) + (1 | Species)
, fixef.prior = normal(cov = diag(9,14))
, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 20000))
, data    = vc.I.data2
, family  = "binomial"
  )

## quick look at what data exists for Transmission
All %>% filter(Component == "Transmission") %>% {
  ggplot(., aes(x = day, y = prop)) + geom_point() +
    facet_grid(Genus~Virus_Family, scales = "free")
}

find_all_zeros <- All %>% filter(Component == "Transmission") %>%
  group_by(Genus, Virus_Family) %>% 
  summarize(max_prop = max(prop, na.rm = T)) %>% filter(max_prop == 0) %>% 
  mutate(GF = interaction(Genus, Virus_Family))

'%notin%' <- Negate('%in%')

vc.T.data <- All %>% filter(Component == "Transmission") %>% 
  mutate(ref = as.factor(ref)) %>% 
  mutate(GF = interaction(Genus, Virus_Family)) %>% 
  filter(GF %notin% find_all_zeros$GF)

vc.T.model <- glmer(
   ## response in form of cbind(success, failure)
   cbind(num, total - num) ~ 
   ## fixed effects
   Genus*Virus_Family + dose + day +
   ## random effects
   (1 | ref) + (1 | Species)
, data    = vc.T.data
, family  = "binomial")

vc.T.data2 <- All %>% filter(Component == "Transmission") %>% mutate(ref = as.factor(ref))

vc.T.model2 <- blme::bglmer(
   ## response in form of cbind(success, failure)
   cbind(num, total - num) ~ 
   ## fixed effects
   Genus*Virus_Family + dose + day +
   ## random effects
   (1 | ref) + (1 | Species)
, fixef.prior = normal(cov = diag(9,15))
, control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 20000))
, data    = vc.T.data2
, family  = "binomial"
  )

## Estimates from both models
emm_s.I <- emmeans::emmeans(
    vc.I.model2
  , ~ Genus | Virus_Family
  , at = list(
    dose = 0
    )
  , type = "response")

emm.pred.I <- data.frame(
  prob.I = summary(emm_s.I)$prob 
, lwr.I  = summary(emm_s.I)$asymp.LCL
, upr.I  = summary(emm_s.I)$asymp.UCL
, vir    = rep(sort(unique(vc.I.data2$Virus_Family)), each = n_distinct(vc.I.data2$Genus))
, mos    = rep(rep(unique(vc.I.data2$Genus) %>% sort()), n_distinct(vc.I.data2$Virus_Family)))

emm_s.T <- emmeans::emmeans(
    vc.T.model2
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
, vir    = rep(sort(unique(vc.T.data2$Virus_Family)), each = n_distinct(vc.T.data2$Genus))
, mos    = rep(rep(unique(vc.T.data2$Genus) %>% sort()), n_distinct(vc.T.data2$Virus_Family)))

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
