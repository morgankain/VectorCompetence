
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

## Fit glmms for aedes
source("aedes_glmm.R")

