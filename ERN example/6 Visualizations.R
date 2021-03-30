library(tidyverse)
library(lme4)
library(shades)
library(foreach)
library(emmeans)


ERNdat = read.delim("./ERN example/Flanker_ERN+CRN_nobe_nobs_long.txt")

# make color-blind friendly palatte
good_colors = NULL
for (i in 1:length(unique(ERNdat$Subject))) {
  # Alter brightness and add colour to array
  good_colors = c(good_colors,
                  lightness("blue", i*1) %>%
    as.character)
}

# Need to match colors to participant ID numbers
names(good_colors) = unique(ERNdat$Subject)

# Look at array of colors
good_colors %>%
  print

# estimate model
mlm_model = lmer(MeanAmp ~ Condition + (Condition|Subject) + (1|Electrode), data = ERNdat) 


# Plot estimated means ------------------------------------------------------------------------

# Find values for plotting
estimated_means = mlm_model %>%
  emmeans(~Condition) %>%
  as_tibble %>%
  mutate(lower_bound = emmean - SE,
         upper_bound = emmean + SE)

# Create the estimated means plot!

line_plot_model = 
  ggplot(estimated_means, aes(x = Condition, y = emmean)) + 
  geom_point() +
  geom_line(aes(group=1)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = .25) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_y_continuous(name = "Mean Amplitude (uV)",
                     limits = c(-2,10),
                     breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# View plot
line_plot_model %>% 
  print

# Save plot
ggsave("./ERN example/Viz1 - Estimated Means.png", line_plot_model, units = "in", width = 5.5, height = 5)

# add observed values for review --------------------------------------------------------------
ERNdat$Subject = factor(ERNdat$Subject)
ggplot(estimated_means, aes(x = Condition, y = emmean)) + 
  geom_point(size = 2) +
  geom_jitter(aes(x = Condition, y = MeanAmp, color = Subject), data = ERNdat, alpha = .3, width = .1) +
  geom_line(aes(group=1), size = 2) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = .1, size = 1) +
  theme_bw() +
  #scale_colour_manual(values = good_colors) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_y_continuous(name = "Mean Amplitude (uV)",
                     limits = c(-9,20),
                     breaks = c(-5, 0, 5, 10, 15, 20))

 

# Plot individual variance --------------------------------------------------------------------

# Get the group-specific linear model parameters by looking at random effects (deviations)
id_estimates = mlm_model %>% 
  coef %>% 
  .$Subject %>% 
  as_tibble %>%
  rename(intercept = `(Intercept)`, slope = ConditionIncorrect) %>%
  mutate(Subject = unique(ERNdat$Subject))

# Joining these together and then using the estimates to estimate means for each level of x
level_2_estimated_means = tibble(Condition = rep(c(0,1), each=length(unique(ERNdat$Subject))),
                                 Subject = rep(unique(ERNdat$Subject), 2)) %>%
  left_join(id_estimates, by="Subject") %>%
  mutate(emmean = intercept + slope * Condition,
         Subject = factor(Subject))

level_2_estimated_means$Condition = ifelse(level_2_estimated_means$Condition == 0, "Correct", "Incorrect")

# Now, creating the spaghetti plot!
spaghetti_plot_model =
  ggplot(level_2_estimated_means, aes(x = Condition, y = emmean)) + 
  geom_point(aes(group = Subject, colour = Subject), size = .5) +
  geom_line(aes(group = Subject, colour = Subject), alpha = .55) +
  scale_colour_manual(values = good_colors) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_y_continuous(name = "Mean Amplitude (uV)",
                     limits = c(-5,18),
                     breaks = c(-5, 0, 5, 10, 15)) 

# View plot
spaghetti_plot_model %>% print

# Save plot
ggsave("./ERN example/Viz2 - Spaghetti Plot.png", spaghetti_plot_model, units = "in", width = 5.5, height = 5)

# add estimated means of fixed effects on top of spaghetti plot

spaghetti_plot_model_fixedmeans =
  ggplot(level_2_estimated_means, aes(x = Condition, y = emmean)) + 
  geom_point(aes(group = Subject, colour = Subject), size = .5) +
  geom_line(aes(group = Subject, colour = Subject), alpha = .55) +
  scale_colour_manual(values = good_colors) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_y_continuous(name = "Mean Amplitude (uV)",
                     limits = c(-5,18),
                     breaks = c(-5, 0, 5, 10, 15)) +

  geom_errorbar(data = estimated_means, aes(ymin = lower_bound, ymax = upper_bound), width = .1, size = 1.5) +
  geom_line(data = estimated_means, aes(x = Condition, y = emmean, group=1), size = 2) +
  geom_point(data = estimated_means, aes(x = Condition, y = emmean), size = 2.5)  


# View plot
spaghetti_plot_model_fixedmeans %>% print

# Save plot
ggsave("./ERN exampleViz3 - Spaghetti Plot with Estimated Means.png", spaghetti_plot_model_fixedmeans, units = "in", width = 5.5, height = 5)



