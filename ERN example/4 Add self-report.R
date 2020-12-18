library(tidyverse)

# add self-report and convert to long format

# read ERN data
ERP.full = read.delim("./ERN example/Flanker_ERN+CRN_nobe.txt")
length(unique(ERP.full$Subject))

ERP.nobs = read.delim("./ERN example/Flanker_ERN+CRN_nobe_nobs.txt") 
length(unique(ERP.nobs$Subject))

# read qualtrics data
dat = read.delim("./ERN example/DiscrimStudy_Qualtrics_clean_fixedGender_HSdiv_withScaleTotals.txt")
dat = rename(dat, Subject = SubID)

# count gender in full ERN sample
dat.full = dat[dat$Subject %in% unique(ERP.full$Subject),]
count(dat.full, Gender)

# count gender in select ERN sample
dat.nobs = dat[dat$Subject %in% unique(ERP.nobs$Subject),]
count(dat.nobs, Gender)

# convert to long form
ERP.full.long = select(ERP.full, Subject, Condition, FZ:CZ, F3:C4, F1:C2) %>% 
  gather(Electrode, MeanAmp, FZ:C2)

ERP.nobs.long = select(ERP.nobs, Subject, Condition, FZ:CZ, F3:C4, F1:C2) %>% 
  gather(Electrode, MeanAmp, FZ:C2)

# merge data
ERP.full.merge = left_join(ERP.full.long, select(dat, Subject, PHQ_sum, GAD_sum), by = "Subject")
ERP.nobs.merge = left_join(ERP.nobs.long, select(dat, Subject, PHQ_sum, GAD_sum), by = "Subject")

# write data
write.table(ERP.full.merge, "./ERN example/Flanker_ERN+CRN_nobe_long.txt", sep = "\t")
write.table(ERP.nobs.merge, "./ERN example/Flanker_ERN+CRN_nobe_nobs_long.txt", sep = "\t")
