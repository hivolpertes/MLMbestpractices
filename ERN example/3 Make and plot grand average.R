library(dplyr)
library(ggplot2)

electrodeList = c("FZ", "FCZ", "CZ", "CPZ", "PZ", "F3", "F4", "FC3", "FC4", "C3", "C4", "CP3", "CP4", "P3", "P4", "F1", "F2", "C1", "C2", "P1", "P2") 
subList = c("001", "003", "005", "006", "008", "011", "012", "017", "018", "019", "020", "023", "024", "028", "031", "032", "034", "035", "036", "037", "039", "040", "041", "042", "043", "044", "045", "046", 
            "047", "050", "051", "052", "054", "057", "060", "061", "062", "063", "064", "066", "067", "068", "069", "070", "071", "072", "073", "079", "080", "087", "088", "089", "091",
            "092", "093", "094", "095", "096", "097", "098", "099", "100", "101", "103", "105", "106", "107", "108", "109", "113", "114", "115", "116")
# take out subs with <= 6 errors (manually)
subs= read.delim("./ERN example/3 SubsLowErrors.txt")
subList.select = c("001", "003", "005", "006",        "011", "012", "017", "018", "019", "020", "023",        "028", "031", "032",        "035", "036", "037", "039", "040", "041", "042",       
                   "044", "045",        "047", "050", "051", "052",               "060", "061", "062", "063", "064", "066",        "068",               "071",        "073", "079", "080", 
                   "087", "088",        "091", "092", "093", "094", "095", "096", "097", "098", "099", "100", "101", "103",        "106", "107", "108", "109", "113", "114", "115", "116")

# compile

allsubsDat = NULL
for (k in subList.select) {
  # read in trial-by-trial point data
  temp = read.delim(paste("./ERN example/4 Indiv average files/Flanker_", k, "_indavg_nobe.txt", sep="")) 
  
  allsubsDat = rbind(allsubsDat, temp)
  
}

# make grand average

grand.avg = select(allsubsDat, Condition, Time, FZ:P2) %>% 
  group_by(Condition, Time) %>% 
  summarise_all(mean) %>% 
  as.data.frame()

# plot

ERNbox = annotate("rect",   
                 xmin=5, xmax=100, ymin=-Inf, ymax=Inf,
                 alpha=0,
                 #fill="#F0E0FF",
                 color="black",
                 linetype="dashed")

ERPline = geom_line(lwd=1.1,
                    aes(color = Condition))

none = element_blank() 


# FZ ------------------------------------------------------------------------------------------

ggplot(grand.avg, aes(Time, FZ, group = Condition)) + 
  ERPline +
  ERNbox +
  ggtitle("FZ") +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-400, 600),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     ) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_reverse(limits=c(12, -6)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values = c("black", "gray70"))

ggsave("./ERN example/5 Grand average plots/1 Flanker_FZ.jpg", width=7.5, height=4, units="in")


# FCZ -----------------------------------------------------------------------------------------

ggplot(grand.avg, aes(Time, FCZ, group = Condition)) + 
  ERPline +
  ERNbox +
  ggtitle("FCZ") +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-400, 600),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
  ) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_reverse(limits=c(12, -6)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values = c("black", "gray70"))

ggsave("./ERN example/5 Grand average plots/2 Flanker_FCZ.jpg", width=7.5, height=4, units="in")


# CZ ------------------------------------------------------------------------------------------

ggplot(grand.avg, aes(Time, CZ, group = Condition)) + 
  ERPline +
  ERNbox +
  ggtitle("CZ") +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-400, 600),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
  ) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_reverse(limits=c(12, -6)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values = c("black", "gray70"))

ggsave("./ERN example/5 Grand average plots/3 Flanker_CZ.jpg", width=7.5, height=4, units="in")


# CPZ -----------------------------------------------------------------------------------------

ggplot(grand.avg, aes(Time, CPZ, group = Condition)) + 
  ERPline +
  ERNbox +
  ggtitle("CPZ") +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-400, 600),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
  ) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_reverse(limits=c(12, -6)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values = c("black", "gray70"))

ggsave("./ERN example/5 Grand average plots/4 Flanker_CPZ.jpg", width=7.5, height=4, units="in")


# PZ ------------------------------------------------------------------------------------------

ggplot(grand.avg, aes(Time, PZ, group = Condition)) + 
  ERPline +
  ERNbox +
  ggtitle("PZ") +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 22)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-400, 600),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
  ) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_reverse(limits=c(12, -6)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values = c("black", "gray70"))

ggsave("./ERN example/5 Grand average plots/5 Flanker_PZ.jpg", width=7.5, height=4, units="in")

