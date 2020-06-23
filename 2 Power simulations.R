library(dplyr)
library(lme4)
library(lmerTest)
library(simr)

# run power analysis


# 1. Read in data -------------------------------------------------

sigavgdat = read.delim("ERPfix_catTask_eyesonly_signalAvg.txt")
sigavgdat$Race.d = ifelse(sigavgdat$Race == "Black", -1, 1)

trialdat = read.delim("ERPfix_catTask_eyesonly_singleTrial.txt")
trialdat$Race.d = ifelse(trialdat$Race == "Black", -1, 1)



# 2. Observed effect size, extend along sub -------------------------------

# model for signal averaged data
mod.sigavg = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = sigavgdat) 

mod.sigavg.extend = extend(mod.sigavg, along = "Subject", n = 65)
p1 = powerCurve(mod.sigavg.extend, along = "Subject", breaks=(1:13)*5)
plot(p1) # effect = -.5896

# model for trial level data
mod.trial = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = trialdat) 

mod.trial.extend = extend(mod.trial, along = "Subject", n = 65)
p2 = powerCurve(mod.trial.extend, along = "Subject", breaks=(1:13)*5)
plot(p2) # effect = -.5858

# 3. Smaller effect size, extend along sub --------------------------------

# model for signal averaged data
mod.sigavg2 = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = sigavgdat) 

fixef(mod.sigavg2)["Race.d"] = -.3

mod.sigavg.extend2 = extend(mod.sigavg2, along = "Subject", n = 100)
p3 = powerCurve(mod.sigavg.extend2, along = "Subject", breaks=(1:20)*5)
plot(p3)

# model for trial level data
mod.trial2 = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = trialdat) 

fixef(mod.sigavg2)["Race.d"] = -.3

mod.trial.extend2 = extend(mod.trial2, along = "Subject", n = 100)
p4 = powerCurve(mod.trial.extend2, along = "Subject", breaks=(1:20)*5)
plot(p4)


#   -----------------------------------------------------------------------




# smaller effect size, extend along sub
mod.sigavg2 = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = sigavgdat)

fixef(mod.sigavg2)["Race.d"] = -.5

mod.sigavg.extend2 = extend(mod.sigavg2, along = "Subject", n = 65)
p1.2 = powerCurve(mod.sigavg.extend2, along = "Subject", breaks=(1:13)*5)
plot(p1.2)

# n = 65, extend along effect size
effectDat = NULL
mod.sigavg = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = sigavgdat) 
for (effect in c(-.1, -.2, -.3, -.4, -.5, -.6, -.7, -.8, -.9, -1.0)) {
  fixef(mod.sigavg)["Race.d"] = effect
  
  sim = powerSim(mod.sigavg)
  
  effectDat = rbind(effectDat, data.frame(effect = effect, 
                                          power = summary(sim)$mean,
                                          lower = summary(sim)$lower,
                                          upper = summary(sim)$upper))
}
for (effect in c(-.15, -.25, -.35, -.45, -.55, -.65)) {
  fixef(mod.sigavg)["Race.d"] = effect
  
  sim = powerSim(mod.sigavg)
  
  effectDat = rbind(effectDat, data.frame(effect = effect, 
                                          power = summary(sim)$mean,
                                          lower = summary(sim)$lower,
                                          upper = summary(sim)$upper))
}

plot(effectDat$effect, effectDat$power)

# 2. Trial level data -----------------------------------------------------


# model for trial level data

# smaller effect size
mod.trial2 = lmer(MeanAmp ~ Race + (Race|Subject) + (1|Subject:Electrode), dat = trialdat) 

fixef(mod.trial2)["RaceWhite"] <- -0.05

mod.trial.extend2 = extend(mod.trial2, along = "Subject", n = 65)
p3 = powerCurve(mod.trial.extend2, along = "Subject", breaks=(1:13)*5)
plot(p3)

# n = 65, extend along effect size
effectDat.trial = NULL
mod.trial = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = trialdat) 
for (effect in c(-.1, -.15, -.2, -.25, -.3, -.35, -.4, -.45, -.5,-.55, -.6,-.65, -.7)) {
  fixef(mod.trial)["Race.d"] = effect
  
  sim = powerSim(mod.trial)
  
  effectDat.trial = rbind(effectDat.trial, data.frame(effect = effect, 
                                                      power = summary(sim)$mean,
                                                      lower = summary(sim)$lower,
                                                      upper = summary(sim)$upper))
}
