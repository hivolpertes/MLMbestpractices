library(dplyr)
library(lme4)
library(lmerTest)

# calculate ICC for subject for single trial and signal averaged data

sigavgdat = read.delim("ERPfix_catTask_eyesonly_signalAvg.txt")
trialdat = read.delim("ERPfix_catTask_eyesonly_singleTrial.txt")

# ICC for signal averaged data
lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), dat = sigavgdat) %>% 
  summary()

# subject
8.314/(8.314+.141+2.306)
# electrode
.141/(8.314+.141+2.306)

# ICC for single trial data
lmer(MeanAmp ~ 1 + (1|Subject) + (1|Electrode), dat = trialdat) %>% 
  summary()

8.338/(8.338+.158+79.43)
