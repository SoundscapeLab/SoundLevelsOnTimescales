################################################################################
# This is code used to do the analyses and plot the figures in 
# "Examining sound levels across different timescales measured from body worn dosimeters"
# by Erik Jorgensen, Jennifer B. Tufts, & Erika Skoe
#
# This is to plot and examine the individual data
#
# For some reason, I calculate the entropy from the data using Matlab. That code 
# is also on the lab's Github. You need to combine the entropy values with the data 
# if you want to include that.
#
# Code is provided "as-is" without any warranties or guarantees of any kind. 
# The user assumes full responsibility for using this code and any outcomes 
# resulting from its use. The code is provided without any form of support 
# maintenance, or updates. Use at your own risk.
#
# Erik Jorgensen, AuD, PhD
# UW-Madison, 2024
#
################################################################################
# Required Libraries 
################################################################################
library(dplyr)        # For data manipulation (e.g., filter, mutate, group_by, summarize)
library(tidyr)        # For data tidying (e.g., pivot_longer, pivot_wider)
library(ggplot2)      # For creating plots
library(viridis)      # For color scales in ggplot2
library(lubridate)    # For date and time manipulation
library(patchwork)    # For arranging multiple plots
################################################################################

################################################################################

data <- read.csv("datafile.csv") # Load the data here

################################################################################

options(scipen = 0, digits = 7) 
Sys.setenv(TZ="America/Detroit")

data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
data$date <- as.Date(data$actual_times)
data$time <- format(data$actual_times, format = "%H:%M:%S")

# Add days too
data$day <- weekdays(data$actual_times)

################################################################################
# By Week
################################################################################

## LAeq ##

# Proportions < / >= 40 dB LAeq

# Calc props for each run for each ID
prop_zeroOrOne <- fullRuns %>%
  group_by(ID, mainRun) %>%
  summarize(Proportion_1 = mean(zeroOrOne == 1))

# Make wide
prop_prop_zeroOrOne_wide <- pivot_wider(prop_zeroOrOne, names_from = mainRun, values_from = Proportion_1)

# calculate pairwise differences
prop_diffs <- prop_prop_zeroOrOne_wide %>%
  mutate(
    A_B = A - B,
    A_C = A - C,
    B_C = B - C
  )

prop_diffs$abs_A_B <- abs(prop_diffs$A_B)
prop_diffs$abs_A_C <- abs(prop_diffs$A_C)
prop_diffs$abs_B_C <- abs(prop_diffs$B_C)

# Just some calculations to look at the data
prop_diffs$meanDiff <- rowMeans(prop_diffs[, c("abs_A_B", "abs_A_C", "abs_B_C")])
A_B_noNA <- na.omit(prop_diffs$A_B)
max(abs(A_B_noNA))
min(abs(A_B_noNA))
A_C_noNA <- na.omit(prop_diffs$A_C)
max(abs(A_C_noNA))
min(abs(A_C_noNA))
B_C_noNA <- na.omit(prop_diffs$B_C)
max(abs(B_C_noNA))
min(abs(B_C_noNA))
mean(abs(A_B_noNA))
mean(abs(A_C_noNA))
mean(abs(B_C_noNA))
meanDiffs_noNA <- na.omit(prop_diffs$meanDiff)
mean(meanDiffs_noNA)

# Plot individual differences

diff_data <- prop_diffs %>%
  select(ID, A_B, A_C, B_C)

long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

p1 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = .5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Individual Differences Between Weeks") +
  labs(subtitle = "Proportions ≥ 40 dB LAeq Differences (All Samples)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display
p1

################################################################################

# LAeq >= 40 dB 

# Let's just look at complete runs
fullRuns <- data[data$run %in% c("A", "B", "C"), ]

# Get only LAeq about dosimeter threhsold
fullRuns <- fullRuns %>% filter(zeroOrOne == 1)

# get means and sd for each ID for each run
means <- data %>%
  group_by(ID, mainRun) %>%
  summarize(mean_LAeq = mean(LAeq)) #,sd_LAeq = sd(LAeq))

# Pivot the data to wide format
means_wide <- pivot_wider(means, names_from = mainRun, values_from = mean_LAeq)

# Calculate pairwise differences
LAeq_diffs <- means_wide %>%
  mutate(
    A_B = A - B,
    A_C = A - C,
    B_C = B - C
  )

LAeq_diffs$abs_A_B <- abs(LAeq_diffs$A_B)
LAeq_diffs$abs_A_C <- abs(LAeq_diffs$A_C)
LAeq_diffs$abs_B_C <- abs(LAeq_diffs$B_C)
LAeq_diffs$meanDiff <- rowMeans(LAeq_diffs[, c("abs_A_B", "abs_A_C", "abs_B_C")])

# Just some calculations to look at the data
A_B_noNA <- na.omit(LAeq_diffs$A_B)
max(abs(A_B_noNA))
min(abs(A_B_noNA))
A_C_noNA <- na.omit(LAeq_diffs$A_C)
max(abs(A_C_noNA))
min(abs(A_C_noNA))
B_C_noNA <- na.omit(LAeq_diffs$B_C)
max(abs(B_C_noNA))
min(abs(B_C_noNA))
mean(abs(A_B_noNA))
mean(abs(A_C_noNA))
mean(abs(B_C_noNA))
meanDiffs_noNA <- na.omit(LAeq_diffs$meanDiff)
mean(meanDiffs_noNA)
sd(meanDiffs_noNA)

# Plot individual differences

diff_data <- LAeq_diffs %>%
  select(ID, A_B, A_C, B_C)

diff_data_long <- gather(diff_data, key = "RunDiff", value = "LAeqDiff", -ID)

diff_data_long <- diff_data_long[order(diff_data_long$ID, diff_data_long$RunDiff), ]

p2 <- ggplot(diff_data_long, aes(x = RunDiff, y = LAeqDiff, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = .5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("LAeq (dB) Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "") +
  labs(subtitle = "LAeq Differences (Samples ≥ 40 dB LAeq)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display

p2

###############################################################################

## Entropy ##

entropy_diff <- data %>%
  pivot_wider(names_from = run, values_from = H) %>%
  mutate(A_B = A - B,
         A_C = A - C,
         B_C = B - C) %>%
  select(ID, A_B, A_C, B_C)

entropy_diff$abs_A_B <- abs(entropy_diff$A_B)
entropy_diff$abs_A_C <- abs(entropy_diff$A_C)
entropy_diff$abs_B_C <- abs(entropy_diff$B_C)

# Just some calculations to look at the data
entropy_diff$meanDiff <- rowMeans(entropy_diff[, c("abs_A_B", "abs_A_C", "abs_B_C")])
A_B_noNA <- na.omit(entropy_diff$A_B)
max(abs(A_B_noNA))
min(abs(A_B_noNA))
A_C_noNA <- na.omit(entropy_diff$A_C)
max(abs(A_C_noNA))
min(abs(A_C_noNA))
B_C_noNA <- na.omit(entropy_diff$B_C)
max(abs(B_C_noNA))
min(abs(B_C_noNA))
mean(abs(A_B_noNA))
mean(abs(A_C_noNA))
mean(abs(B_C_noNA))
meanDiffs_noNA <- na.omit(entropy_diff$meanDiff)
mean(meanDiffs_noNA)

# Plot individual differences

Hdiff_means <- na.omit(entropy_diff$meanDiff)
mean(Hdiff_means)
max(Hdiff_means)
min(Hdiff_means)

diff_data <- entropy_diff %>%
  select(ID, A_B, A_C, B_C)

long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

p3 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = .5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Entropy Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "") +
  labs(subtitle = "Entropy Differences (Samples ≥ 40 dB LAeq)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display
p3

# Combine and print if you want to
p <- p1 / p2 / p3
p
#ggsave(plot = p, width = 8, height = 12, dpi = 300, filename = "byWeek_individual.png")

################################################################################
# By Season
################################################################################

## LAeq ##

# Proportions < / >= 40 dB LAeq

# Let's just look at complete runs
data$run <- as.character(data$run)
data$run <- trimws(data$run)
fullRuns <- data[data$run %in% c("A", "B", "C"), ]

# Define date ranges for different seasons
fall_start <- as.POSIXct("2018-9-22")
fall_end <- as.POSIXct("2018-12-20")

winter_start <- as.POSIXct("2018-12-21")
winter_end <- as.POSIXct("2019-03-19")

spring_start <- as.POSIXct("2019-3-20")
spring_end <- as.POSIXct("2019-06-21")

# Add a new column "season" with appropriate labels based on date ranges
data <- data %>%
  mutate(
    season = case_when(
      actual_times >= fall_start &  actual_times <= fall_end ~ "Fall",
      actual_times >= winter_start &  actual_times <= winter_end ~ "Winter",
      actual_times >= spring_start &  actual_times <= spring_end ~ "Spring",
      TRUE ~ NA_character_
    )
  )

# calc props for each run for each ID
prop_zeroOrOne <- data %>%
  group_by(ID, season) %>%
  summarize(Proportion_1 = mean(loud == 1))

# make wide
prop_zeroOrOne_wide <- pivot_wider(prop_zeroOrOne, names_from = season, values_from = Proportion_1)

# NA bad
prop_zeroOrOne_wide <- prop_zeroOrOne_wide[, !colnames(prop_zeroOrOne_wide) %in% "NA"]

# calculate pairwise differences
prop_diffs <- prop_zeroOrOne_wide %>%
  mutate(
    F_S = Fall - Spring,
    F_W = Fall - Winter,
    S_W = Spring - Winter
  )

prop_diffs$abs_F_S <- abs(prop_diffs$F_S)
prop_diffs$abs_F_W <- abs(prop_diffs$F_W)
prop_diffs$abs_S_W <- abs(prop_diffs$S_W)

# Just some calculations to look at the data
prop_diffs$meanDiff <- rowMeans(prop_diffs[, c("abs_F_S", "abs_F_W", "abs_S_W")])
F_S_noNA <- na.omit(prop_diffs$F_S)
max(abs(F_S_noNA))
min(abs(F_S_noNA))
F_W_noNA <- na.omit(prop_diffs$F_W)
max(abs(F_W_noNA))
min(abs(F_W_noNA))
S_W_noNA <- na.omit(prop_diffs$S_W)
max(abs(S_W_noNA))
min(abs(S_W_noNA))
mean(abs(F_S_noNA))
mean(abs(F_W_noNA))
mean(abs(S_W_noNA))
meanDiffs_noNA <- na.omit(prop_diffs$meanDiff)
mean(meanDiffs_noNA)

# Plot individual differences

diff_data <- prop_diffs %>%
  select(ID, F_S, F_W, S_W)
colnames(diff_data) <- sub("F_S", "Fall_Spring", colnames(diff_data))
colnames(diff_data) <- sub("F_W", "Fall_Winter", colnames(diff_data))
colnames(diff_data) <- sub("S_W", "Spring_Winter", colnames(diff_data))

long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

p1 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = 0.5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Individual Differences Between Seasons") +
  labs(subtitle = "Proportions ≥ 40 dB LAeq Differences (All Samples)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 

# Display
p1

################################################################################

# LAeq >= 40 dB 

# Get only LAeq about dosimeter threhsold
data <- data %>% filter(zeroOrOne == 1)

# Get means and sd for each ID for each run
means <- data %>%
  group_by(ID, season) %>%
  summarize(mean_LAeq = mean(LAeq)) #,sd_LAeq = sd(LAeq))

means_wide <- pivot_wider(means, names_from = season, values_from = mean_LAeq)

# Calculate pairwise differences
mean_diffs <- means_wide %>%
  mutate(
    F_S = Fall - Spring,
    F_W = Fall - Winter,
    S_W = Spring - Winter
  )
mean_diffs$abs_F_S <- abs(mean_diffs$F_S)
mean_diffs$abs_F_W <- abs(mean_diffs$F_W)
mean_diffs$abs_S_W <- abs(mean_diffs$S_W)

# Just some calculations to look at the data
mean_diffs$meanDiff <- rowMeans(mean_diffs[, c("abs_F_S", "abs_F_W", "abs_S_W")])
F_S_noNA <- na.omit(mean_diffs$F_S)
max(abs(F_S_noNA))
min(abs(F_S_noNA))
F_W_noNA <- na.omit(mean_diffs$F_W)
max(abs(F_W_noNA))
min(abs(F_W_noNA))
S_W_noNA <- na.omit(mean_diffs$S_W)
max(abs(S_W_noNA))
min(abs(S_W_noNA))
mean(abs(F_S_noNA))
mean(abs(F_W_noNA))
mean(abs(S_W_noNA))
meanDiffs_noNA <- na.omit(mean_diffs$meanDiff)
mean(meanDiffs_noNA)

# Plot individual data

diff_data <- mean_diffs %>%
  select(ID, F_S, F_W, S_W)

colnames(diff_data) <- sub("F_S", "Fall_Spring", colnames(diff_data))
colnames(diff_data) <- sub("F_W", "Fall_Winter", colnames(diff_data))
colnames(diff_data) <- sub("S_W", "Spring_Winter", colnames(diff_data))

long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

p2 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = 0.5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("LAeq (dB) Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "") +
  labs(subtitle = "LAeq Differences (Samples ≥ 40 dB LAeq)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display
p2

###############################################################################

## Entropy

# Caluclate differences
entropy_diff <- data %>%
  pivot_wider(names_from = season, values_from = entropy) %>%
  mutate(
    F_S = Fall - Spring,
    F_W = Fall - Winter,
    S_W = Spring - Winter) %>%
  select(ID, F_S, F_W, S_W)

entropy_diff$abs_F_S <- abs(entropy_diff$F_S)
entropy_diff$abs_F_W <- abs(entropy_diff$F_W)
entropy_diff$abs_S_W <- abs(entropy_diff$S_W)

# Just some calculations to look at the data
entropy_diff$meanDiff <- rowMeans(entropy_diff[, c("abs_F_S", "abs_F_W", "abs_S_W")])
F_S_noNA <- na.omit(entropy_diff$F_S)
max(abs(F_S_noNA))
min(abs(F_S_noNA))
F_W_noNA <- na.omit(entropy_diff$F_W)
max(abs(F_W_noNA))
min(abs(F_W_noNA))
S_W_noNA <- na.omit(entropy_diff$S_W)
max(abs(S_W_noNA))
min(abs(S_W_noNA))
mean(abs(F_S_noNA))
mean(abs(F_W_noNA))
mean(abs(S_W_noNA))
meanDiffs_noNA <- na.omit(entropy_diff$meanDiff)
mean(meanDiffs_noNA)

# Plot individual differences

diff_data <- entropy_diff %>%
  select(ID, F_S, F_W, S_W)

colnames(diff_data) <- sub("F_S", "Fall_Spring", colnames(diff_data))
colnames(diff_data) <- sub("F_W", "Fall_Winter", colnames(diff_data))
colnames(diff_data) <- sub("S_W", "Spring_Winter", colnames(diff_data))

long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

p3 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = .5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Entropy Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "") +
  labs(subtitle = "Entropy Differences (Samples ≥ 40 dB LAeq)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display
p3

# Combine and save if you want to
p <- p1 / p2 / p3
p
#ggsave(plot = p, width = 8, height = 12, dpi = 300, filename = "bySeason_individual.png")

################################################################################
# By Day
################################################################################


## LAeq ##

# Proportions < / >= 40 dB LAeq

# Calculate proportions of 0 and 1 for each day
data$zeroOrOne <- ifelse(data$LAeq > 0, 1, 0)

# Calc props for each run for each ID
prop_zeroOrOne <- data %>%
  group_by(ID, day) %>%
  summarize(Proportion_1 = mean(zeroOrOne == 1))

# Make wide
prop_zeroOrOne_wide <- pivot_wider(prop_zeroOrOne, names_from = day, values_from = Proportion_1)

# NA bad
prop_zeroOrOne_wide <- prop_zeroOrOne_wide[, !colnames(prop_zeroOrOne_wide) %in% "NA"]

# Calculate pairwise differences
prop_diffs <- prop_zeroOrOne_wide %>%
  mutate(
    S_M = Sunday - Monday,
    S_T = Sunday - Tuesday,
    S_W = Sunday - Wednesday,
    S_Th = Sunday - Thursday,
    S_F = Sunday - Friday,
    S_Sa = Sunday - Saturday,
    M_T = Monday - Tuesday,
    M_W = Monday - Wednesday,
    M_Th = Monday - Thursday,
    M_F = Monday - Friday,
    M_Sa = Monday - Saturday,
    T_W = Tuesday - Wednesday,
    T_Th = Tuesday - Thursday,
    T_F = Tuesday - Friday,
    T_Sa = Tuesday - Saturday,
    W_Th = Wednesday - Thursday,
    W_F = Wednesday - Friday,
    W_Sa = Wednesday - Saturday,
    Th_F = Thursday - Friday,
    Th_Sa = Thursday - Saturday,
    F_Sa = Friday - Saturday
  )

prop_diffs$abs_S_M <- abs(prop_diffs$S_M)
prop_diffs$abs_S_T <- abs(prop_diffs$S_T)
prop_diffs$abs_S_W <- abs(prop_diffs$S_W)
prop_diffs$abs_S_F <- abs(prop_diffs$S_F)
prop_diffs$abs_S_Sa <- abs(prop_diffs$S_Sa)
prop_diffs$abs_M_T <- abs(prop_diffs$M_T)
prop_diffs$abs_M_W <- abs(prop_diffs$M_W)
prop_diffs$abs_M_Th <- abs(prop_diffs$M_Th)
prop_diffs$abs_M_F <- abs(prop_diffs$M_F)
prop_diffs$abs_M_Sa <- abs(prop_diffs$M_Sa)
prop_diffs$abs_T_W <- abs(prop_diffs$T_W)
prop_diffs$abs_T_Th <- abs(prop_diffs$T_Th)
prop_diffs$abs_T_F <- abs(prop_diffs$T_F)
prop_diffs$abs_T_Sa <- abs(prop_diffs$T_Sa)
prop_diffs$abs_W_Th <- abs(prop_diffs$W_Th)
prop_diffs$abs_W_F <- abs(prop_diffs$W_F)
prop_diffs$abs_W_Sa <- abs(prop_diffs$W_Sa)
prop_diffs$abs_Th_F <- abs(prop_diffs$Th_F)
prop_diffs$abs_Th_Sa <- abs(prop_diffs$Th_Sa)
prop_diffs$abs_F_Sa <- abs(prop_diffs$F_Sa)

# Just some calculations to look at the data
prop_diffs$meanDiff <- rowMeans(prop_diffs[, c("abs_S_M", "abs_S_T", "abs_S_W", "abs_S_F", "abs_S_Sa", "abs_M_T",
                                               "abs_M_W", "abs_M_Th", "abs_M_F", "abs_M_Sa", "abs_T_W", "abs_T_Th", "abs_T_F",
                                               "abs_T_Sa", "abs_W_Th", "abs_W_F", "abs_W_Sa", "abs_Th_F", "abs_Th_Sa", "abs_F_Sa")])

max(prop_diffs$abs_S_M)
min(prop_diffs$abs_S_M)
mean(prop_diffs$abs_S_M)

max(prop_diffs$abs_S_T)
min(prop_diffs$abs_S_T)
mean(prop_diffs$abs_S_T)

max(prop_diffs$abs_S_W) 
min(prop_diffs$abs_S_W) 
mean(prop_diffs$abs_S_W) 

max(prop_diffs$abs_S_F) 
min(prop_diffs$abs_S_F) 
mean(prop_diffs$abs_S_F)

max(prop_diffs$abs_S_Sa) 
min(prop_diffs$abs_S_Sa) 
mean(prop_diffs$abs_S_Sa) 

max(prop_diffs$abs_M_T)
min(prop_diffs$abs_M_T)
mean(prop_diffs$abs_M_T)

max(prop_diffs$abs_M_W) 
min(prop_diffs$abs_M_W) 
mean(prop_diffs$abs_M_W) 

max(prop_diffs$abs_M_Th)
min(prop_diffs$abs_M_Th)
mean(prop_diffs$abs_M_Th)

max(prop_diffs$abs_M_F) 
min(prop_diffs$abs_M_F)
mean(prop_diffs$abs_M_F) 

max(prop_diffs$abs_M_Sa) 
min(prop_diffs$abs_M_Sa) 
mean(prop_diffs$abs_M_Sa) 

max(prop_diffs$abs_T_W) 
min(prop_diffs$abs_T_W) 
mean(prop_diffs$abs_T_W)

max(prop_diffs$abs_T_Th)
min(prop_diffs$abs_T_Th) 
mean(prop_diffs$abs_T_Th)

max(prop_diffs$abs_T_F) 
min(prop_diffs$abs_T_F) 
mean(prop_diffs$abs_T_F)

max(prop_diffs$abs_T_Sa) 
min(prop_diffs$abs_T_Sa) 
mean(prop_diffs$abs_T_Sa) 

max(prop_diffs$abs_W_Th)
min(prop_diffs$abs_W_Th)
mean(prop_diffs$abs_W_Th)

max(prop_diffs$abs_W_F)
min(prop_diffs$abs_W_F)
mean(prop_diffs$abs_W_F)

max(prop_diffs$abs_W_Sa)
min(prop_diffs$abs_W_Sa)
mean(prop_diffs$abs_W_Sa)

max(prop_diffs$abs_Th_F)
min(prop_diffs$abs_Th_F)
mean(prop_diffs$abs_Th_F)

max(prop_diffs$abs_Th_Sa)
min(prop_diffs$abs_Th_Sa)
mean(prop_diffs$abs_Th_Sa)

max(prop_diffs$abs_F_Sa)
min(prop_diffs$abs_F_Sa)
mean(prop_diffs$abs_F_Sa)

mean(prop_diffs$meanDiff)

#

# Plot individual differences

# Shorten day names using two-letter abbreviations
diff_data <- prop_diffs %>%
  select(ID, S_M, S_T, S_W, S_Th, S_F, S_Sa, M_T, M_W, M_Th, M_F, M_Sa, T_W, T_Th, T_F, T_Sa, W_Th, W_F, W_Sa, Th_F, Th_Sa, F_Sa)

# Update the column names with shorter abbreviations
colnames(diff_data) <- sub("S_Th", "Su_Th", colnames(diff_data))
colnames(diff_data) <- sub("M_Th", "Mo_Th", colnames(diff_data))
colnames(diff_data) <- sub("T_Th", "Tu_Th", colnames(diff_data))
colnames(diff_data) <- sub("W_Th", "We_Th", colnames(diff_data))
colnames(diff_data) <- sub("W_Sa", "We_Sa", colnames(diff_data))
colnames(diff_data) <- sub("Th_F", "Th_Fr", colnames(diff_data))
colnames(diff_data) <- sub("Th_Sa", "Th_Sa", colnames(diff_data))
colnames(diff_data) <- sub("F_Sa", "Fr_Sa", colnames(diff_data))
colnames(diff_data) <- sub("S_Sa", "Su_Sa", colnames(diff_data))
colnames(diff_data) <- sub("M_Sa", "Mo_Sa", colnames(diff_data))
colnames(diff_data) <- sub("T_Sa", "Tu_Sa", colnames(diff_data))
colnames(diff_data) <- sub("S_M", "Su_Mo", colnames(diff_data))
colnames(diff_data) <- sub("S_T", "Su_Tu", colnames(diff_data))
colnames(diff_data) <- sub("S_W", "Su_We", colnames(diff_data))
colnames(diff_data) <- sub("S_F", "Su_Fr", colnames(diff_data))
colnames(diff_data) <- sub("M_T", "Mo_Tu", colnames(diff_data))
colnames(diff_data) <- sub("M_W", "Mo_We", colnames(diff_data))
colnames(diff_data) <- sub("M_F", "Mo_Fr", colnames(diff_data))
colnames(diff_data) <- sub("T_W", "Tu_We", colnames(diff_data))
colnames(diff_data) <- sub("T_F", "Tu_Fr", colnames(diff_data))
colnames(diff_data) <- sub("W_F", "We_Fr", colnames(diff_data))

# Reshape the data to long format
long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

# Define the factor levels with shortened abbreviations
long_data <- long_data %>%
  arrange(ID, factor(Comparison, levels = c(
    "Su_Mo", "Su_Tu", "Su_We", "Su_Th", "Su_Fr", "Su_Sa",
    "Mo_Tu", "Mo_We", "Mo_Th", "Mo_Fr", "Mo_Sa",
    "Tu_We", "Tu_Th", "Tu_Fr", "Tu_Sa",
    "We_Th", "We_Fr", "We_Sa",
    "Th_Fr", "Th_Sa",
    "Fr_Sa"
  )))

# Convert Comparison column to factor with the same order
long_data$Comparison <- factor(long_data$Comparison, levels = c(
  "Su_Mo", "Su_Tu", "Su_We", "Su_Th", "Su_Fr", "Su_Sa",
  "Mo_Tu", "Mo_We", "Mo_Th", "Mo_Fr", "Mo_Sa",
  "Tu_We", "Tu_Th", "Tu_Fr", "Tu_Sa",
  "We_Th", "We_Fr", "We_Sa",
  "Th_Fr", "Th_Sa",
  "Fr_Sa"
))

p1 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = .5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x), expand = c(0.1, 0)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Individual Differences Between Days") +
  labs(subtitle = "Proportion < / ≥ 40 dB LAeq Differences (All Samples)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display
p1

################################################################################

# LAeq >= 40 dB 

data <- data %>% filter(zeroOrOne == 1)

# Get means and sd for each ID for each run
means <- data %>%
  group_by(ID, day) %>%
  summarize(mean_LAeq = mean(LAeq)) #,sd_LAeq = sd(LAeq))

means_wide <- pivot_wider(means, names_from = day, values_from = mean_LAeq)

# Calculate pairwise differences
mean_diffs <- means_wide %>%
  mutate(
    S_M = Sunday - Monday,
    S_T = Sunday - Tuesday,
    S_W = Sunday - Wednesday,
    S_Th = Sunday - Thursday,
    S_F = Sunday - Friday,
    S_Sa = Sunday - Saturday,
    M_T = Monday - Tuesday,
    M_W = Monday - Wednesday,
    M_Th = Monday - Thursday,
    M_F = Monday - Friday,
    M_Sa = Monday - Saturday,
    T_W = Tuesday - Wednesday,
    T_Th = Tuesday - Thursday,
    T_F = Tuesday - Friday,
    T_Sa = Tuesday - Saturday,
    W_Th = Wednesday - Thursday,
    W_F = Wednesday - Friday,
    W_Sa = Wednesday - Saturday,
    Th_F = Thursday - Friday,
    Th_Sa = Thursday - Saturday,
    F_Sa = Friday - Saturday
  )

mean_diffs$abs_S_M <- abs(mean_diffs$S_M)
mean_diffs$abs_S_T <- abs(mean_diffs$S_T)
mean_diffs$abs_S_W <- abs(mean_diffs$S_W)
mean_diffs$abs_S_F <- abs(mean_diffs$S_F)
mean_diffs$abs_S_Sa <- abs(mean_diffs$S_Sa)
mean_diffs$abs_M_T <- abs(mean_diffs$M_T)
mean_diffs$abs_M_W <- abs(mean_diffs$M_W)
mean_diffs$abs_M_Th <- abs(mean_diffs$M_Th)
mean_diffs$abs_M_F <- abs(mean_diffs$M_F)
mean_diffs$abs_M_Sa <- abs(mean_diffs$M_Sa)
mean_diffs$abs_T_W <- abs(mean_diffs$T_W)
mean_diffs$abs_T_Th <- abs(mean_diffs$T_Th)
mean_diffs$abs_T_F <- abs(mean_diffs$T_F)
mean_diffs$abs_T_Sa <- abs(mean_diffs$T_Sa)
mean_diffs$abs_W_Th <- abs(mean_diffs$W_Th)
mean_diffs$abs_W_F <- abs(mean_diffs$W_F)
mean_diffs$abs_W_Sa <- abs(mean_diffs$W_Sa)
mean_diffs$abs_Th_F <- abs(mean_diffs$Th_F)
mean_diffs$abs_Th_Sa <- abs(mean_diffs$Th_Sa)
mean_diffs$abs_F_Sa <- abs(mean_diffs$F_Sa)

# Just some calculations to look at the data
mean_diffs$meanDiff <- rowMeans(mean_diffs[, c("abs_S_M", "abs_S_T", "abs_S_W", "abs_S_F", "abs_S_Sa", "abs_M_T",
                                               "abs_M_W", "abs_M_Th", "abs_M_F", "abs_M_Sa", "abs_T_W", "abs_T_Th", "abs_T_F",
                                               "abs_T_Sa", "abs_W_Th", "abs_W_F", "abs_W_Sa", "abs_Th_F", "abs_Th_Sa", "abs_F_Sa")])

max(mean_diffs$abs_S_M)
min(mean_diffs$abs_S_M)
mean(mean_diffs$abs_S_M)

max(mean_diffs$abs_S_T)
min(mean_diffs$abs_S_T)
mean(mean_diffs$abs_S_T)

max(mean_diffs$abs_S_W) 
min(mean_diffs$abs_S_W) 
mean(mean_diffs$abs_S_W) 

max(mean_diffs$abs_S_F) 
min(mean_diffs$abs_S_F) 
mean(mean_diffs$abs_S_F)

max(mean_diffs$abs_S_Sa) 
min(mean_diffs$abs_S_Sa) 
mean(mean_diffs$abs_S_Sa) 

max(mean_diffs$abs_M_T)
min(mean_diffs$abs_M_T)
mean(mean_diffs$abs_M_T)

max(mean_diffs$abs_M_W) 
min(mean_diffs$abs_M_W) 
mean(mean_diffs$abs_M_W) 

max(mean_diffs$abs_M_Th)
min(mean_diffs$abs_M_Th)
mean(mean_diffs$abs_M_Th)

max(mean_diffs$abs_M_F) 
min(mean_diffs$abs_M_F)
mean(mean_diffs$abs_M_F) 

max(mean_diffs$abs_M_Sa) 
min(mean_diffs$abs_M_Sa) 
mean(mean_diffs$abs_M_Sa) 

max(mean_diffs$abs_T_W) 
min(mean_diffs$abs_T_W) 
mean(mean_diffs$abs_T_W)

max(mean_diffs$abs_T_Th)
min(mean_diffs$abs_T_Th) 
mean(mean_diffs$abs_T_Th)

max(mean_diffs$abs_T_F) 
min(mean_diffs$abs_T_F) 
mean(mean_diffs$abs_T_F)

max(mean_diffs$abs_T_Sa) 
min(mean_diffs$abs_T_Sa) 
mean(mean_diffs$abs_T_Sa) 

max(mean_diffs$abs_W_Th)
min(mean_diffs$abs_W_Th)
mean(mean_diffs$abs_W_Th)

max(mean_diffs$abs_W_F)
min(mean_diffs$abs_W_F)
mean(mean_diffs$abs_W_F)

max(mean_diffs$abs_W_Sa)
min(mean_diffs$abs_W_Sa)
mean(mean_diffs$abs_W_Sa)

max(mean_diffs$abs_Th_F)
min(mean_diffs$abs_Th_F)
mean(mean_diffs$abs_Th_F)

max(mean_diffs$abs_Th_Sa)
min(mean_diffs$abs_Th_Sa)
mean(mean_diffs$abs_Th_Sa)

max(mean_diffs$abs_F_Sa)
min(mean_diffs$abs_F_Sa)
mean(mean_diffs$abs_F_Sa)

mean(mean_diffs$meanDiff)

# Plot individual differences 

# Shorten day names using two-letter abbreviations
diff_data <- prop_diffs %>%
  select(ID, S_M, S_T, S_W, S_Th, S_F, S_Sa, M_T, M_W, M_Th, M_F, M_Sa, T_W, T_Th, T_F, T_Sa, W_Th, W_F, W_Sa, Th_F, Th_Sa, F_Sa)

# Update the column names with shorter abbreviations
colnames(diff_data) <- sub("S_Th", "Su_Th", colnames(diff_data))
colnames(diff_data) <- sub("M_Th", "Mo_Th", colnames(diff_data))
colnames(diff_data) <- sub("T_Th", "Tu_Th", colnames(diff_data))
colnames(diff_data) <- sub("W_Th", "We_Th", colnames(diff_data))
colnames(diff_data) <- sub("W_Sa", "We_Sa", colnames(diff_data))
colnames(diff_data) <- sub("Th_F", "Th_Fr", colnames(diff_data))
colnames(diff_data) <- sub("Th_Sa", "Th_Sa", colnames(diff_data))
colnames(diff_data) <- sub("F_Sa", "Fr_Sa", colnames(diff_data))
colnames(diff_data) <- sub("S_Sa", "Su_Sa", colnames(diff_data))
colnames(diff_data) <- sub("M_Sa", "Mo_Sa", colnames(diff_data))
colnames(diff_data) <- sub("T_Sa", "Tu_Sa", colnames(diff_data))
colnames(diff_data) <- sub("S_M", "Su_Mo", colnames(diff_data))
colnames(diff_data) <- sub("S_T", "Su_Tu", colnames(diff_data))
colnames(diff_data) <- sub("S_W", "Su_We", colnames(diff_data))
colnames(diff_data) <- sub("S_F", "Su_Fr", colnames(diff_data))
colnames(diff_data) <- sub("M_T", "Mo_Tu", colnames(diff_data))
colnames(diff_data) <- sub("M_W", "Mo_We", colnames(diff_data))
colnames(diff_data) <- sub("M_F", "Mo_Fr", colnames(diff_data))
colnames(diff_data) <- sub("T_W", "Tu_We", colnames(diff_data))
colnames(diff_data) <- sub("T_F", "Tu_Fr", colnames(diff_data))
colnames(diff_data) <- sub("W_F", "We_Fr", colnames(diff_data))

# Reshape the data to long format
long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

# Define the factor levels with shortened abbreviations
long_data <- long_data %>%
  arrange(ID, factor(Comparison, levels = c(
    "Su_Mo", "Su_Tu", "Su_We", "Su_Th", "Su_Fr", "Su_Sa",
    "Mo_Tu", "Mo_We", "Mo_Th", "Mo_Fr", "Mo_Sa",
    "Tu_We", "Tu_Th", "Tu_Fr", "Tu_Sa",
    "We_Th", "We_Fr", "We_Sa",
    "Th_Fr", "Th_Sa",
    "Fr_Sa"
  )))

# Convert Comparison column to factor with the same order
long_data$Comparison <- factor(long_data$Comparison, levels = c(
  "Su_Mo", "Su_Tu", "Su_We", "Su_Th", "Su_Fr", "Su_Sa",
  "Mo_Tu", "Mo_We", "Mo_Th", "Mo_Fr", "Mo_Sa",
  "Tu_We", "Tu_Th", "Tu_Fr", "Tu_Sa",
  "We_Th", "We_Fr", "We_Sa",
  "Th_Fr", "Th_Sa",
  "Fr_Sa"
))

p2 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = 0.5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("LAeq (dB) Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x), expand = c(0.1, 0)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "") +
  labs(subtitle = "LAeq Differences (Samples ≥ 40 dB LAeq)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Display
p2

################################################################################

## Entropy

data <- data %>%
  group_by(ID, day, run) %>%
  summarise(AvgEntropy = mean(entropy, na.rm = TRUE)) %>%
  group_by(ID, Day) %>%
  summarise(AvgEntropy = mean(AvgEntropy, na.rm = TRUE))

# Calculate pairwise differences
entropy_diff <- data %>%
  pivot_wider(names_from = day, values_from = AvgEntropy) %>%
  mutate(
    S_M = Sunday - Monday,
    S_T = Sunday - Tuesday,
    S_W = Sunday - Wednesday,
    S_Th = Sunday - Thursday,
    S_F = Sunday - Friday,
    S_Sa = Sunday - Saturday,
    M_T = Monday - Tuesday,
    M_W = Monday - Wednesday,
    M_Th = Monday - Thursday,
    M_F = Monday - Friday,
    M_Sa = Monday - Saturday,
    T_W = Tuesday - Wednesday,
    T_Th = Tuesday - Thursday,
    T_F = Tuesday - Friday,
    T_Sa = Tuesday - Saturday,
    W_Th = Wednesday - Thursday,
    W_F = Wednesday - Friday,
    W_Sa = Wednesday - Saturday,
    Th_F = Thursday - Friday,
    Th_Sa = Thursday - Saturday,
    F_Sa = Friday - Saturday
  )

diff_data <- entropy_diff %>%
  select(ID, S_M, S_T, S_W, S_Th, S_F, S_Sa, M_T, M_W, M_Th, M_F, M_Sa, T_W, T_Th, T_F, T_Sa, W_Th, W_F, W_Sa, Th_F, Th_Sa, F_Sa)

diff_data$abs_S_M <- abs(diff_data$S_M)
diff_data$abs_S_T <- abs(diff_data$S_T)
diff_data$abs_S_W <- abs(diff_data$S_W)
diff_data$abs_S_F <- abs(diff_data$S_F)
diff_data$abs_S_Sa <- abs(diff_data$S_Sa)
diff_data$abs_M_T <- abs(diff_data$M_T)
diff_data$abs_M_W <- abs(diff_data$M_W)
diff_data$abs_M_Th <- abs(diff_data$M_Th)
diff_data$abs_M_F <- abs(diff_data$M_F)
diff_data$abs_M_Sa <- abs(diff_data$M_Sa)
diff_data$abs_T_W <- abs(diff_data$T_W)
diff_data$abs_T_Th <- abs(diff_data$T_Th)
diff_data$abs_T_F <- abs(diff_data$T_F)
diff_data$abs_T_Sa <- abs(diff_data$T_Sa)
diff_data$abs_W_Th <- abs(diff_data$W_Th)
diff_data$abs_W_F <- abs(diff_data$W_F)
diff_data$abs_W_Sa <- abs(diff_data$W_Sa)
diff_data$abs_Th_F <- abs(diff_data$Th_F)
diff_data$abs_Th_Sa <- abs(diff_data$Th_Sa)
diff_data$abs_F_Sa <- abs(diff_data$F_Sa)


# Just some calculations to look at the data
diff_data$meanDiff <- rowMeans(mean_diffs[, c("abs_S_M", "abs_S_T", "abs_S_W", "abs_S_F", "abs_S_Sa", "abs_M_T",
                                               "abs_M_W", "abs_M_Th", "abs_M_F", "abs_M_Sa", "abs_T_W", "abs_T_Th", "abs_T_F",
                                               "abs_T_Sa", "abs_W_Th", "abs_W_F", "abs_W_Sa", "abs_Th_F", "abs_Th_Sa", "abs_F_Sa")])

max(diff_data$abs_S_M)
min(diff_data$abs_S_M)
mean(diff_data$abs_S_M)

max(diff_data$abs_S_T)
min(diff_data$abs_S_T)
mean(diff_data$abs_S_T)

max(diff_data$abs_S_W) 
min(diff_data$abs_S_W) 
mean(diff_data$abs_S_W) 

max(diff_data$abs_S_F) 
min(diff_data$abs_S_F) 
mean(diff_data$abs_S_F)

max(diff_data$abs_S_Sa) 
min(diff_data$abs_S_Sa) 
mean(diff_data$abs_S_Sa) 

max(diff_data$abs_M_T)
min(diff_data$abs_M_T)
mean(diff_data$abs_M_T)

max(diff_data$abs_M_W) 
min(diff_data$abs_M_W) 
mean(diff_data$abs_M_W) 

max(diff_data$abs_M_Th)
min(diff_data$abs_M_Th)
mean(diff_data$abs_M_Th)

max(diff_data$abs_M_F) 
min(diff_data$abs_M_F)
mean(diff_data$abs_M_F) 

max(diff_data$abs_M_Sa) 
min(diff_data$abs_M_Sa) 
mean(diff_data$abs_M_Sa) 

max(diff_data$abs_T_W) 
min(diff_data$abs_T_W) 
mean(diff_data$abs_T_W)

max(diff_data$abs_T_Th)
min(diff_data$abs_T_Th) 
mean(diff_data$abs_T_Th)

max(diff_data$abs_T_F) 
min(diff_data$abs_T_F) 
mean(diff_data$abs_T_F)

max(diff_data$abs_T_Sa) 
min(diff_data$abs_T_Sa) 
mean(diff_data$abs_T_Sa) 

max(diff_data$abs_W_Th)
min(diff_data$abs_W_Th)
mean(diff_data$abs_W_Th)

max(diff_data$abs_W_F)
min(diff_data$abs_W_F)
mean(diff_data$abs_W_F)

max(diff_data$abs_W_Sa)
min(diff_data$abs_W_Sa)
mean(diff_data$abs_W_Sa)

max(diff_data$abs_Th_F)
min(diff_data$abs_Th_F)
mean(diff_data$abs_Th_F)

max(diff_data$abs_Th_Sa)
min(diff_data$abs_Th_Sa)
mean(diff_data$abs_Th_Sa)

max(diff_data$abs_F_Sa)
min(diff_data$abs_F_Sa)
mean(diff_data$abs_F_Sa)

mean(diff_data$meanDiff)

# Plot individual differences
  
# Update the column names with shorter abbreviations
colnames(diff_data) <- sub("S_Th", "Su_Th", colnames(diff_data))
colnames(diff_data) <- sub("M_Th", "Mo_Th", colnames(diff_data))
colnames(diff_data) <- sub("T_Th", "Tu_Th", colnames(diff_data))
colnames(diff_data) <- sub("W_Th", "We_Th", colnames(diff_data))
colnames(diff_data) <- sub("W_Sa", "We_Sa", colnames(diff_data))
colnames(diff_data) <- sub("Th_F", "Th_Fr", colnames(diff_data))
colnames(diff_data) <- sub("Th_Sa", "Th_Sa", colnames(diff_data))
colnames(diff_data) <- sub("F_Sa", "Fr_Sa", colnames(diff_data))
colnames(diff_data) <- sub("S_Sa", "Su_Sa", colnames(diff_data))
colnames(diff_data) <- sub("M_Sa", "Mo_Sa", colnames(diff_data))
colnames(diff_data) <- sub("T_Sa", "Tu_Sa", colnames(diff_data))
colnames(diff_data) <- sub("S_M", "Su_Mo", colnames(diff_data))
colnames(diff_data) <- sub("S_T", "Su_Tu", colnames(diff_data))
colnames(diff_data) <- sub("S_W", "Su_We", colnames(diff_data))
colnames(diff_data) <- sub("S_F", "Su_Fr", colnames(diff_data))
colnames(diff_data) <- sub("M_T", "Mo_Tu", colnames(diff_data))
colnames(diff_data) <- sub("M_W", "Mo_We", colnames(diff_data))
colnames(diff_data) <- sub("M_F", "Mo_Fr", colnames(diff_data))
colnames(diff_data) <- sub("T_W", "Tu_We", colnames(diff_data))
colnames(diff_data) <- sub("T_F", "Tu_Fr", colnames(diff_data))
colnames(diff_data) <- sub("W_F", "We_Fr", colnames(diff_data))

# Remove columns that start with "abs_"
diff_data <- diff_data %>%
  select(-starts_with("abs_"))

# Convert data to long format for plotting
long_data <- gather(diff_data, key = "Comparison", value = "Value", -ID)

# Filter out rows where Comparison is "meanDiff" or Value is NA
long_data <- long_data %>%
  filter(Comparison != "meanDiff" & !is.na(Value))

# Order the data by ID and Comparison
long_data <- long_data[order(long_data$ID, long_data$Comparison), ]

# Define the factor levels with shortened abbreviations
long_data <- long_data %>%
  arrange(ID, factor(Comparison, levels = c(
    "Su_Mo", "Su_Tu", "Su_We", "Su_Th", "Su_Fr", "Su_Sa",
    "Mo_Tu", "Mo_We", "Mo_Th", "Mo_Fr", "Mo_Sa",
    "Tu_We", "Tu_Th", "Tu_Fr", "Tu_Sa",
    "We_Th", "We_Fr", "We_Sa",
    "Th_Fr", "Th_Sa",
    "Fr_Sa"
  )))

long_data$Comparison <- factor(long_data$Comparison, levels = c(
  "Su_Mo", "Su_Tu", "Su_We", "Su_Th", "Su_Fr", "Su_Sa",
  "Mo_Tu", "Mo_We", "Mo_Th", "Mo_Fr", "Mo_Sa",
  "Tu_We", "Tu_Th", "Tu_Fr", "Tu_Sa",
  "We_Th", "We_Fr", "We_Sa",
  "Th_Fr", "Th_Sa",
  "Fr_Sa"
))

p3 <- ggplot(long_data, aes(x = Comparison, y = Value, group = ID, color = factor(ID))) +
  geom_path(size = .6, alpha = .5) +
  geom_point(aes(color = factor(ID))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Entropy Difference") +
  xlab("") +
  scale_x_discrete(labels = function(x) gsub("_", "-", x), expand = c(0.1, 0)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "") +
  labs(subtitle = "Entropy Differences (Samples ≥ 40 dB LAeq)") + 
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 

# Display
p3

# Combine the plots and save if you want to
p <- p1 / p2 / p3
p
#ggsave(plot = p, width = 10, height = 12, dpi = 300, filename = "byDay_individual.png")

################################################################################
# By Second
################################################################################

strptime(data$time[1], format = "%H:%M:%S")

# Create a new column for time of day as numeric values
data$time_numeric <- as.numeric(as.POSIXlt(data$time, format = "%H:%M:%S"))

# You need to check this!
plot_breaks <- seq(from = 1701925200, by = 3600, length.out = 24)

# Plot individual differences

p <- ggplot(data, aes(x = time_numeric, y = LAeq, color = as.factor(ID))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, linewidth = .6, alpha = .5) +  # Fit 3rd degree polynomial regressions
  labs(x = "Time of Day", y = "LAeq (dB)") +
  scale_x_continuous(
    breaks = plot_breaks,  # Specify the tick positions
    labels = c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00", "7:00 AM",
               "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM",
               "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Individual Regressions: LAeq by Time") +
  theme(plot.title = element_text(face = "bold", size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display
# Save if you want to
#ggsave(plot = p, width = 10, height = 10, dpi = 300, filename = "byTime_Individual.png")
