library(tidyverse)
library(lme4)
library(lmerTest)

ERNdat = read.delim("./ERN example/Flanker_ERN+CRN_nobe_nobs_long.txt")

# compare random factor structure -------------------------------------------------------------

# hierarchical model (electrode nested within subject)
lmer(MeanAmp ~ 1 + (1|Subject) + (1|Subject:Electrode), data = ERNdat) %>% 
  summary()

# cross-classified model (electrode crossed with subject)
lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), data = ERNdat) %>% 
  summary()

# estimate model with dummy-coded Condition variable (correct = 0, incorrect = 1) -------------

ERNdat$Condition.d = ifelse(ERNdat$Condition == "Correct", 0, 1)

m1 = lmer(MeanAmp ~ Condition.d + (Condition.d|Subject) + (1|Electrode), data = ERNdat)
summary(m1)

# calculate CI for effect of Condition
confint(m1)

# estimate model with effect-coded Condition variable (correct = -1, incorrect = 1) -----------

ERNdat$Condition.e = ifelse(ERNdat$Condition == "Correct", -1, 1)

m2 = lmer(MeanAmp ~ Condition.e + (Condition.e|Subject) + (1|Electrode), data = ERNdat)
summary(m2)

# calculate CI for effect of Condition
confint(m2)

# estimate model with anxiety
m3 = lmer(MeanAmp ~ Condition.e * GAD_sum + (Condition.d|Subject) + (1|Electrode), data = ERNdat)
summary(m3)
confint(m3)
