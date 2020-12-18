library(dplyr)
library(tidyr)

# Takes elaborated TBT files and make individual averages
# Quantifies ERN

electrodeList = c("FZ", "FCZ", "CZ", "CPZ", "PZ", "F3", "F4", "FC3", "FC4", "C3", "C4", "CP3", "CP4", "P3", "P4", "F1", "F2", "C1", "C2", "P1", "P2") 
subList = c("001", "003", "005", "006", "008", "011", "012", "017", "018", "019", "020", "023", "024", "028", "031", "032", "034", "035", "036", "037", "039", "040", "041", "042", "043", "044", "045", "046", 
            "047", "050", "051", "052", "054", "057", "060", "061", "062", "063", "064", "066", "067", "068", "069", "070", "071", "072", "073", "079", "080", "087", "088", "089", "091",
            "092", "093", "094", "095", "096", "097", "098", "099", "100", "101", "103", "105", "106", "107", "108", "109", "113", "114", "115", "116")

ERNquantdat = NULL

for (k in subList) {
  
  # read in trial-by-trial point data
  temp = read.delim(paste("./ERN example/3 Elab TBT files/Flanker_", k, "_TBT_acceptedTrials.txt", sep="")) 

  # create individual average files  
  temp.avg = select(temp, Correct, Time, FZ:P2) %>% 
    group_by(Correct, Time) %>% 
    summarise_all(mean) %>% 
    as.data.frame()
  
  temp.avg$Subject = k
  
  # change correct condition
  temp.avg$Condition[temp.avg$Correct == 1] = "Incorrect"
  temp.avg$Condition[temp.avg$Correct == 2] = "Correct"
  temp.avg = select(temp.avg, -Correct)
  
  # take out bad electrodes
  if(k == "052") {
    temp.avg$P2 = NA
  }
  
  write.table(temp.avg, paste("./ERN example/4 Indiv average files/Flanker_", k, "_indavg_nobe.txt", sep=""), sep="\t")
 
  # quantify ERN from individual average (0-100 ms)
  ERNquant = filter(temp.avg, Time >= 0 & Time <= 100) %>% 
    select(-Time, -Subject) %>% 
    group_by(Condition) %>% 
    summarise_all(mean) %>% 
    as.data.frame()
  
  ERNquant$Subject = k
   
  ERNquantdat = rbind(ERNquantdat, ERNquant)
  
}

ERNquantdat$Subject = as.numeric(ERNquantdat$Subject)

write.table(ERNquantdat, "./ERN example/Flanker_ERN+CRN_nobe.txt", sep="\t")

# make data set without subs with low errors
subs= read.delim("./ERN example/3 SubsLowErrors.txt")

ERNquantdat.select = filter(ERNquantdat, !(Subject %in% subs$Subject))

write.table(ERNquantdat.select, "./ERN example/Flanker_ERN+CRN_nobe_nobs.txt", sep="\t")
