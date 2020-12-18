library(dplyr)
library(tidyr)

# Written by Hannah. 4/17/2016
# Edited 11/11/2020
# Takes point data from .eeg files exported from Neuroscan


electrodeList = c("FZ", "FCZ", "CZ", "CPZ", "PZ", "F3", "F4", "FC3", "FC4", "C3", "C4", "CP3", "CP4", "P3", "P4", "F1", "F2", "C1", "C2", "P1", "P2") 
subList = c("001", "003", "005", "006", "008", "011", "012", "017", "018", "019", "020", "023", "024", "028", "031", "032", "034", "035", "036", "037", "039", "040", "041", "042", "043", "044", "045", "046", 
            "047", "050", "051", "052", "054", "057", "060", "061", "062", "063", "064", "066", "067", "068", "069", "070", "071", "072", "073", "079", "080", "087", "088", "089", "091",
            "092", "093", "094", "095", "096", "097", "098", "099", "100", "101", "103", "105", "106", "107", "108", "109", "113", "114", "115", "116")

beginEpoch = -400 # how many ms before onset does epoch start
endEpoch = 600 # how many ms after onset does epoch end

samplePoints = -200:300*2 # sampling rate is 500 Hz
lengthEpoch = length(samplePoints)

NumAcceptedTrials = NULL # will become file that has how many trials were accepted for each subject



# 1. Create elaborated files -----------------------------------------------

NumAcceptedTrials = NULL
checkCounts = NULL

### Labels files with all trial information 
for (k in subList) {
  
  # read in trial-by-trial point data
  temp = read.delim(paste("./ERN example/2 TBT files/Flanker_", k, "_TBT_frontal.txt", sep=""), skip=2, header=FALSE, colClasses = "numeric") 
  names(temp) = electrodeList # replace column names
  temp = temp[,1:length(electrodeList)] # get rid of last NA column
  
  numTrials = nrow(temp)/lengthEpoch # figures out how many trials are in data
  
  # add identifiers
  temp$Subject = k
  temp$Time = rep(samplePoints, numTrials)
  temp$Trial = rep(1:numTrials, each = lengthEpoch)
  
  # read in file of rejected trials
  rej = read.delim(paste("./ERN example/2 TBT files/Flanker_", k, "_REJ_frontal.txt", sep=""), header=FALSE, colClasses = "numeric") 
  # read in event file to add whether response was correct, condition, etc.
  ev2 = read.delim(paste("./ERN example/1 Event files/", k, "_flanker_rev.ev2", sep=""), sep=" ", header=FALSE) # separator is weird when not using ezev2 to create rev ev2 files 
  
  for (i in 1:numTrials) {
    # add rejection status from Scan: 0 = reject, 1 = accept
    temp$Rejected[temp$Trial == i] = rej[i,1] 
    # add whether response was correct:  2 = correct, 1 = incorrect, 3 = timeout
    temp$Correct[temp$Trial == i] = ev2$V3[ev2$V1 == i] 
  }
  
  # check condition counts
  checkCountsTemp = data.frame(Subject = k,
                           numConLeft = nrow(ev2[ev2$V2 == 11,]),
                           numConRight = nrow(ev2[ev2$V2 == 21,]),
                           numInconLeft = nrow(ev2[ev2$V2 == 12,]),
                           numInconRight = nrow(ev2[ev2$V2 == 22,]))
  checkCounts = rbind(checkCounts, checkCountsTemp)
  # 24, 68, 109 don't have full 200 trials because of recording errors
  # 1, 6, 11 have missed up stim codes because stim trigger codes weren't sent
  
  
  # Calculate the number of trials that were correct (if there is a correct response), rejected due to
  # EEG artifacts and how many trials in total that were accepted (correct and no artifacts)
  temp.select = temp[temp$Rejected == 1,] # selects subset of just accepted trials
  
  rej = length(unique(temp$Trial[temp$Rejected == 0]))
  cor = length(unique(temp.select$Trial[temp.select$Correct == 2]))
  inc = length(unique(temp.select$Trial[temp.select$Correct == 1]))
  miss = length(unique(temp.select$Trial[temp.select$Correct == 3]))
  
  numAccTemp = data.frame(Subject = k,
                          numEV2 = nrow(ev2),
                          numTrials = numTrials, # number of trials recorded total
                          numArtifact = rej, # number of trials that were rejected because of EEG artifacts
                          numCorrect = cor,
                          numIncorrect = inc,
                          numMiss = miss,
                          Match = numTrials == rej+cor+inc+miss)
  
  NumAcceptedTrials = rbind(NumAcceptedTrials, numAccTemp)

  
  # add condition information
  ev2names = c("Trial", "Trigger", "Resp", "Acc", "RT", "Onset")
  names(ev2) = ev2names
  
  congruentTrials = ev2$Trial[ev2$Trigger %in% c(11, 21)]
  incongruentTrials = ev2$Trial[ev2$Trigger %in% c(12, 22)]
 
  # subs 001, 006, 011 only have 11 for stim codes because stim codes weren't sent
  temp.select$Congruence[temp.select$Trial %in% congruentTrials] = "congruent"
  temp.select$Congruence[temp.select$Trial %in% incongruentTrials] = "incongruent"
  
  temp.select = temp.select[temp.select$Correct != 3,]
  # write file (only accepted trials, no misses)
  write.table(temp.select, paste("./ERN example/3 Elab TBT files/Flanker_", k, "_TBT_acceptedTrials.txt", sep=""),
              sep="\t", row.names = F)
}

write.table(NumAcceptedTrials, "./ERN example/1 NumberOfAcceptedTrials.txt", sep = "\t", row.names = F)
write.table(checkCounts, "./ERN example/2 NumberOfAcceptTrialsPerCondition.txt", sep = "\t", row.names = F)



# 3. Identify subs with <= 5 errors  --------------------------------------

numTrials = read.delim("./ERN example/1 NumberOfAcceptedTrials.txt")

subs = numTrials$Subject[numTrials$numIncorrect <= 6]
numTrials[numTrials$Subject %in% subs,]

# 9 subs have <=5 errors
# 13 have <=6 errors

write.table(numTrials[numTrials$Subject %in% subs,], "./ERN example/3 SubsLowErrors.txt", sep="\t")





