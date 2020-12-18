library(dplyr)
library(lme4)
library(lmerTest)
library(simr)
library(ggplot2)

# run power analysis


# 1. Read in data -------------------------------------------------

sigavgdat = read.delim("./Power/ERPfix_catTask_eyesonly_signalAvg.txt")
sigavgdat$Race.d = ifelse(sigavgdat$Race == "Black", -1, 1)

trialdat = read.delim("./Power/ERPfix_catTask_eyesonly_singleTrial.txt")
trialdat$Race.d = ifelse(trialdat$Race == "Black", -1, 1)



# 2. Observed effect size, extend along sub -------------------------------

# model for signal averaged data
mod.sigavg = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = sigavgdat) 

# observed power
powerSim(mod.sigavg)

# extend along sub
mod.sigavg.extend = extend(mod.sigavg, along = "Subject", n = 65)
p1 = powerCurve(mod.sigavg.extend, along = "Subject", breaks=(1:13)*5)
plot(p1) # effect = -.5896
# save
sigavg.alongsub = summary(p1)
sigavg.alongsub = rename(sigavg.alongsub, 
                         numSubs = nlevels,
                         power = mean)
write.table(sigavg.alongsub, "./Power/PowerSim_sigavg_alongsub.txt", sep="\t", row.names=F)

sigavg.alongsub = read.delim("./Power/PowerSim_sigavg_alongsub.txt")
ggplot(sigavg.alongsub, aes(numSubs, power)) +
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=1,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(10,20,30,40,50,60)) +
  xlab("Number of participants") +
  ylab("Power") +
  theme_bw() +
  geom_hline(yintercept=1) +
  geom_hline(yintercept = .8, linetype = "dashed")

ggsave("./Power/PowerSim_extendSub.png", units = "in", width = 5, height = 4)


# Sample = 65, extend along effect size -----------------------------------
effectDat = NULL
for (effect in c(-.15, -.20, -.25, -.30, -.35, -.40, -.45, -.50, -.55, -.60, -.65)) {
  fixef(mod.sigavg)["Race.d"] = effect
  
  sim = powerSim(mod.sigavg)
  
  effectDat = rbind(effectDat, data.frame(effect = effect, 
                                          power = summary(sim)$mean,
                                          lower = summary(sim)$lower,
                                          upper = summary(sim)$upper))
}

effectDat2 = effectDat[order(-effectDat$effect),]
write.table(effectDat2, "./Power/PowerSim_sigavg_alongeffect.txt", sep="\t", row.names=F)

effectDat2 = read.delim("./Power/PowerSim_sigavg_alongeffect.txt")
ggplot(effectDat2, aes(effect, power)) +
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.01,
                position=position_dodge(.9)) +
  scale_x_reverse(breaks=c(-.1, -.2, -.3, -.4, -.5, -.6, -.7)) +
  theme_bw() +
  xlab("Effect") +
  ylab("Power") +
  geom_hline(yintercept=1) +
  geom_hline(yintercept = .8, linetype = "dashed")

ggsave("./Power/PowerSim_extendEffect.png", units = "in", width = 5, height = 4)

# model for trial level data # not working for some reason
mod.trial = lmer(MeanAmp ~ Race.d + (Race.d|Subject) + (1|Subject:Electrode), dat = trialdat) 

# observed power
powerSim(mod.trial)

# extend along sub
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
