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
