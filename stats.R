
## Prep the data for the linear model for groupings
lm.data.A <- all.res %>% 
  group_by(Virus, Mosquito, Component) %>%
  summarize(
    all_samps = sum(num, na.rm = T)
  )

lm.data.B <-  all.res %>% 
  group_by(Virus, Mosquito, Component) %>%
  filter(max_val == 1) %>% 
  summarize(
    dose      = mean(dose, na.rm = T)
  , day       = mean(day, na.rm = T)
  , max_samps = round(mean(num, na.rm = T))
  )
  
lm.data <- left_join(lm.data.A, lm.data.B) %>% left_join(.
  , (maxs.gg %>% dplyr::select(Virus, Mosquito, Component, Ratio, Genus, Species))
  )

lm.data <- lm.data %>% 
  mutate(Virus_Group = plyr::mapvalues(Virus
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
    ))) %>% mutate(Genus = plyr::mapvalues(Genus
 , to = c(
    "Aedes"
  , "Anopheles"
  , "Coquillettidia"
  , "Culex"
  , "Mansonia"
  , "Verrallina"
 )
 , from = c("Ae.", "An.", "Cq.", "Cx.", "Ma.", "Ve.")
)) %>% filter(!is.na(Genus))

lm.data.ttt.I <- lm.data %>% filter(Component == "Infection") %>% 
  filter(Virus_Group != "Phlebovirus")

lm.data.ttt.T <- lm.data %>% filter(Component == "Transmission") %>% 
  filter(Virus_Group != "Phlebovirus")

## Scale total number of sample sizes and use this scaled variable as relative research effort
lm.data.ttt.I <- transform(lm.data.ttt.I, scaled_samps = scale(all_samps))
lm.data.ttt.T <- transform(lm.data.ttt.T, scaled_samps = scale(all_samps))

test.glm.I <- glm(
     Ratio ~ Virus_Group*Genus + scaled_samps + dose + day
   , family  = "binomial"
   , method  = brglmFit
   , weights = max_samps
   , data    = lm.data.ttt.I)

test.glm.T <- glm(
     Ratio ~ Virus_Group*Genus + scaled_samps + dose + day
   , family  = "binomial"
   , method  = brglmFit
   , weights = max_samps
   , data    = lm.data.ttt.T)
  
## Use emmeans to extract the estimates for each pairing
emm_s.T <- emmeans::emmeans(test.glm.T, ~ Genus | Virus_Group, type = "response")
emm_s.I <- emmeans::emmeans(test.glm.I, ~ Genus | Virus_Group, type = "response")

## and stick them in a data frame
emm.pred.I <- data.frame(
  prob.I = summary(emm_s.I)$prob 
, lwr.I  = summary(emm_s.I)$asymp.LCL
, upr.I  = summary(emm_s.I)$asymp.UCL
, vir    = rep(sort(unique(lm.data.ttt.I$Virus_Group)), each = 6)
, mos    = rep(rep(unique(lm.data.ttt.I$Genus)), 3)
)

emm.pred.T <- data.frame(
  prob.T = summary(emm_s.T)$prob
, lwr.T  = summary(emm_s.T)$asymp.LCL
, upr.T  = summary(emm_s.T)$asymp.UCL
, vir    = rep(sort(unique(lm.data.ttt.T$Virus_Group)), each = 6)
, mos    = rep(rep(unique(lm.data.ttt.T$Genus)), 3)
)

emm.pred <- cbind(emm.pred.I, emm.pred.T[, 1:3])

names(emm.pred)[c(4, 5)] <- c("Virus", "Genus")


### --- Association between infection and transmission (in various ways) --- ###

for_cor.test <- left_join(
(
  lm.data.ttt.I %>% dplyr::select(Virus, Mosquito, Ratio, Genus, Species, Virus_Group, max_samps) %>% 
    rename(Infection = Ratio)
)
, 
(
  lm.data.ttt.T %>% dplyr::select(Virus, Mosquito, Ratio, Genus, Species, Virus_Group) %>% 
    rename(Transmission = Ratio)
)
)

with(for_cor.test
, cor.test(Infection, Transmission)
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
 , data    = for_cor.test
)

summary(glm.rt)
with(summary(glm.rt), 1 - deviance/null.deviance)

## Calculate the sample-size weighted proportional change in proportion between each step

maxs.gg %>% 
  group_by(Component) %>% 
  summarize(avg_ratio = weighted.mean(Ratio, max.mosq, na.rm = T)) %>%
  mutate(prop_ratio = avg_ratio / lag(avg_ratio, 1))


### --- glmm for aedes aeqypti --- ###

## Start from scratch with the original data
All   <- read.csv("All.csv")

## For Aedes aegypti check the viruses with good coverage of each data type
test.inf <- All %>% filter(
  !is.na(infectious.dose.mid)
, !is.na(day.PE.mid)
, !is.na(total.infect)
, species == "ae_aegypti") %>% 
  group_by(virus) %>% 
  dplyr::summarize(
  infection = sum(length(unique(infectious.dose.mid)))
  )

test.dis <- All %>% filter(
  !is.na(infectious.dose.mid)
, !is.na(day.PE.mid)
, !is.na(total.dissem)
, species == "ae_aegypti") %>% 
  group_by(virus) %>% 
  dplyr::summarize(
  disseminated = sum(length(unique(day.PE.mid)))
  )

test.trans <- All %>% filter(
  !is.na(infectious.dose.mid)
, !is.na(day.PE.mid)
, !is.na(total.transmitted)
, species == "ae_aegypti") %>% 
  group_by(virus) %>% 
  dplyr::summarize(
  transmitted = sum(length(unique(day.PE.mid)))
  )

left_join(test.inf, test.dis) %>% left_join(., test.trans)
 ## CHIKV
 ## DENV_3
 ## ZIKV

virus_plotting <- c("CHIKV", "DENV_3", "ZIKV")


## Fit separately (could also consider multivariate model to get the correlations among the outcomes)

## Infection

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

All.lme4 <- glmer(
   cbind(num.infected, total.infect - num.infected) ~ virus*infectious.dose.mid + day.PE.mid +
  (1 | ref)
## cbind(success, failure)
, data    = All.lme4.data 
, family  = "binomial"
)

n_samps     <- 1000
dose_range  <- seq(1, 10, by = 0.5)
median_day  <- All.s %>% filter(species == "ae_aegypti", !is.na(total.infect), !is.na(num.infected)) %>% 
  group_by(virus) %>%
  summarize(median_day = median(day.PE.mid))

if (run.bootmer) {

## bootstrap
bootmer.time <- system.time({
All.lme4.bb.ci <- bootMer(
    All.lme4
  , FUN  = function(x) predict(x, re.form = NA
    , newdata = expand.grid(
      day.PE.mid          = day_range
    , virus               = virus_range
    , infectious.dose.mid = dose_range
      )
    , type = "response")
  , .progress = "txt"
  , nsim = 500)
})

## best to save and load for figures as this takes quite a long time
saveRDS(All.lme4.bb.ci, "inf.boot.Rds")

}

## Dissemination

All.s <- All %>% filter(!is.na(infectious.dose.mid), !is.na(day.PE.mid), !is.na(total.dissem))

All.lme4 <- glmer(
   cbind(num.disseminated, total.dissem - num.disseminated) ~ virus*day.PE.mid + infectious.dose.mid +
  (1 | ref)
## cbind(success, failure)
, data    = (All.s %>% filter(species == "ae_aegypti")) 
, family  = "binomial"
)

n_samps      <- 1000
day_range    <- seq(1, 25, by = 0.5)
virus_range  <- unique((All.s %>% filter(species == "ae_aegypti"))$virus)
median_dose  <- All.s %>% filter(species == "ae_aegypti", !is.na(total.dissem), !is.na(num.disseminated)) %>%
  group_by(virus) %>% 
  summarize(median_dose = median(infectious.dose.mid))

if (run.bootmer) {

bootmer.time <- system.time({
All.lme4.bb.ci <- bootMer(
    All.lme4
  , FUN  = function(x) predict(x, re.form = NA
    , newdata = expand.grid(
      day.PE.mid          = day_range
    , virus               = virus_range
    , infectious.dose.mid = dose_range
      )
    , type = "response")
  , .progress = "txt"
  , nsim = 500)
})

saveRDS(All.lme4.bb.ci, "dis.boot.Rds")

}

#### Transmission

All.s <- All %>% filter(!is.na(infectious.dose.mid), !is.na(day.PE.mid), !is.na(total.transmitted))

## num.transmitted
## total.transmitted
All.lme4 <- glmer(
  cbind(num.transmitted, total.transmitted - num.transmitted) ~ virus*day.PE.mid + infectious.dose.mid +
  (1 | ref)
## cbind(success, failure)
, data    = (All.s %>% filter(species == "ae_aegypti")) 
, family  = "binomial"
)

## Fixed effects
fix_eff_matrix <- MASS::mvrnorm(n_samps
  , mu    = getME(All.lme4, c("beta"))
  , Sigma = summary(All.lme4)[["vcov"]])

n_samps     <- 1000
day_range   <- seq(1, 25, by = 0.5)
virus_range <- unique((All.s %>% filter(species == "ae_aegypti"))$virus)
median_dose  <- All.s %>% filter(species == "ae_aegypti", !is.na(total.transmitted), !is.na(num.transmitted)) %>%
  group_by(virus) %>% 
  summarize(median_dose = median(infectious.dose.mid))

if (run.bootmer) {

bootmer.time <- system.time({
All.lme4.bb.ci <- bootMer(
    All.lme4
  , FUN  = function(x) predict(x, re.form = NA
    , newdata = expand.grid(
      day.PE.mid          = day_range
    , virus               = virus_range
    , infectious.dose.mid = dose_range
      )
    , type = "response")
  , .progress = "txt"
  , nsim = 500)
})

saveRDS(All.lme4.bb.ci, "tra.boot.Rds")

}

