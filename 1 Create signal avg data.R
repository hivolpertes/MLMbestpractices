library(dplyr)

dat = read.delim("Cat_AllSubs_TBTaverages_noBS_groupP2.txt")
length(unique(dat$Subject))

# select only eyes trials
eyesonly = filter(dat, Fix == "eyes")

write.table(eyesonly, "ERPfix_catTask_eyesonly_singleTrial.txt", sep="\t", row.names=F)

# average across trials for signal averaging
sigavg = select(eyesonly, Subject, Electrode, MeanAmp, Race) %>% 
  group_by(Subject, Electrode, Race) %>% 
  summarize_all(mean) %>% 
  as.data.frame()

write.table(sigavg, "ERPfix_catTask_eyesonly_signalAvg.txt", sep="\t", row.names=F)

