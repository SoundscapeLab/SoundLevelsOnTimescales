################################################################################
# This is code used to do the analyses and plot the figures in 
# "Examining sound levels across different timescales measured from body worn dosimeters"
# by Erik Jorgensen, Jennifer B. Tufts, & Erika Skoe
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

library(lme4)        # For mixed effects models
library(lmerTest)    # For p-values in mixed models
library(ggplot2)     # For plotting
library(patchwork)   # For combining plots
library(emmeans)     # For estimated marginal means
library(dplyr)       # For data manipulation
library(tidyr)       # For data reshaping
library(kableExtra)  # For table formatting
library(magrittr)    # For pipe operations
library(ggpattern)   # For pattern fills in plots
library(viridis)     # For color palettes
library(performance) # For model diagnostics

################################################################################

data <- read.csv("datafile.csv") # Load the data here

################################################################################
# Basic stuff 
################################################################################

# You need to set the timezone correctly
options(scipen = 0, digits = 7) 
Sys.setenv(TZ="America/Detroit")

# Convert date and time formats 
data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
data$date <- as.Date(data$actual_times)
data$time <- format(data$actual_times, format = "%H:%M:%S")

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

# Add days too
data$day_of_week <- weekdays(data$actual_times)

################################################################################
# Compliance etc
################################################################################

# Count subjects
num_subjects <- data %>%
  summarize(num_unique_subjects = n_distinct(ID))

# Get compliance data
subject_counts <- data %>%
  group_by(ID) %>%
  summarize(count = n())

comp_perc <- subject_counts %>%
  mutate_all(~ . / 8064)

mean_comp <- mean(comp_perc$count)
std_err_comp <- sd(comp_perc$count) / sqrt(length(comp_perc$count))

# Calculate the mean count of datapoints across all subjects
mean_count_datapoints <- mean(subject_counts$count)
# Calculate the standard error of the mean count of datapoints
std_err_count_datapoints <- sd(subject_counts$count) / sqrt(length(subject_counts$count))
# Print the mean count of datapoints and standard error
cat("Mean count of datapoints:", mean_count_datapoints, "\n")
cat("Standard Error:", std_err_count_datapoints, "\n")

# Get compliance data for non-zero data if you want
data_no0 <- subset(data, LAeq > 0) #no zeros

# Get compliance data
subject_counts <- data_no0 %>%
  group_by(ID) %>%
  summarize(count = n())

# Calculate the mean count of datapoints across all subjects
mean_count_datapoints <- mean(subject_counts$count)
# Calculate the standard error of the mean count of datapoints
std_err_count_datapoints <- sd(subject_counts$count) / sqrt(length(subject_counts$count))
# Print the mean count of datapoints and standard error
cat("Mean count of datapoints:", mean_count_datapoints, "\n")
cat("Standard Error:", std_err_count_datapoints, "\n")

# Let's just look at complete runs
data$run <- as.character(data$run)
data$run <- trimws(data$run)
fullRuns <- data[data$run %in% c("A", "B", "C"), ]

# Check ID count
unique_runs <- unique(fullRuns$run)
num_unique_runs <- length(unique_runs)

# Count subjects
num_subjects <- fullRuns %>%
  summarize(num_unique_subjects = n_distinct(ID))

# Get compliance data
subject_counts <- fullRuns %>%
  group_by(ID) %>%
  summarize(count = n())

comp_perc <- subject_counts %>%
  mutate_all(~ . / 8064)

# Calculate the mean count of datapoints across all subjects
mean_comp <- mean(comp_perc$count)
# Calculate the standard error of the mean count of datapoints
std_err_comp <- sd(comp_perc$count) / sqrt(length(comp_perc$count))

# Calculate the mean count of datapoints across all subjects
mean_count_datapoints <- mean(subject_counts$count)
# Calculate the standard error of the mean count of datapoints
std_err_count_datapoints <- sd(subject_counts$count) / sqrt(length(subject_counts$count))
# Print the mean count of datapoints and standard error
cat("Mean count of datapoints:", mean_count_datapoints, "\n")
cat("Standard Error:", std_err_count_datapoints, "\n")

################################################################################
# Plot all data for full runs
################################################################################

selected_colors <- c("#440154", "#2a788e", "#7ad151") 

p <- ggplot(fullRuns, aes(x=date, y=LAeq, color = run, shape = run)) +
  geom_point(alpha = 0.05) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("LAeq (dB)") +
  xlab("") +
  labs(title ="All LAeq Values by Week and Date") +
  scale_color_manual(values = setNames(selected_colors, unique(fullRuns$run))) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  geom_vline(xintercept = as.Date(c("2018-09-22", "2018-12-21", "2019-03-21")), linetype="dotted", color="black") +
  annotate("text", x = as.Date("2018-09-22"), y = 25, label = "Fall", vjust = -0.9, angle = 90, color="black",size = 6) +
  annotate("text", x = as.Date("2018-12-21"), y = 25, label = "Winter", vjust = -0.9, angle = 90, color="black",size = 6) +
  annotate("text", x = as.Date("2019-03-21"), y = 25, label = "Spring", vjust = -0.9, angle = 90, color="black",size = 6)

# Display 
p

# If you want to save it 
#ggsave(plot = p, width = 15, height = 5, dpi = 300, filename = "plot.png")

################################################################################
################################################################################
# By Week
################################################################################

## LAeq ##

# Proportions < / >= 40 dB LAeq

# Use fullRuns

# Calculate subject means and then means by run
proportions_by_subject_within_run <- fullRuns %>%
  group_by(run, ID, zeroOrOne) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = zeroOrOne, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = `0` + `1`,
    Proportion_zero = `0` / Total,
    Proportion_one = `1` / Total
  ) 

proportions_by_subject_within_run <- proportions_by_subject_within_run[c("run", "ID", "Proportion_zero", "Proportion_one")]

# Calculate the mean proportions of zeros and ones for each run
mean_proportions_by_run <- proportions_by_subject_within_run %>%
  group_by(run) %>%
  summarise(
    Zero = mean(Proportion_zero),
    One = mean(Proportion_one)
  ) %>%
  ungroup()

print(mean_proportions_by_run)

prop_data <- mean_proportions_by_run %>%
  pivot_longer(cols = c(Zero, One),
               names_to = "zeroOrOne",
               values_to = "proportion")

# GLMM for proportion differences by weeks 
m1 <- glmer(zeroOrOne ~ run + (1 + run | ID), data = fullRuns, family = binomial(link="logit"))
summary(m1)
tidy(m1)
performance(m1)

# Pariwise comparisons with FDR corrections if you want
#emmeans(m1, list(pairwise ~ run), type="response", reverse= "TRUE", adjust = "fdr") #, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

# ICCs if you want
#calc ICC
#var_components <- as.data.frame(VarCorr(m1))
#residual_variance <- attr(VarCorr(m1), "sc")^2
#icc <- var_components$vcov / (var_components$vcov + residual_variance)

# Median odds ratio if you want
mor_effect <- effect("run", m1)
mor_value <- median(mor_effect$fit)

# Model diagnostics if you want to check

#plot(m1)
#residuals <- resid(m1, type = "pearson")
#qqnorm(residuals)
#qqline(residuals)

# Create a side-by-side barplot
p1 <- ggplot(prop_data, aes(x = run, y = proportion, fill = zeroOrOne)) +
  geom_col_pattern(width = 0.7, position = "dodge", pattern = c('stripe', 'none', 'stripe', 'none', 'stripe', 'none'),pattern_density = .1,
                   pattern_spacing = .04,
                   pattern_fill = 'black') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion") +
  xlab("") +
  labs(title ="Proportions ≥ 40 dB LAeq by Week") +
  labs(subtitle = "All Samples") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("Zero" = "#9f2a63", "One" = "#fc9f07"),
                    breaks = c("Zero", "One"),
                    labels = c("< 40 dB LAeq", "≥ 40 dB LAeq")) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm")) + # Adjust the legend margin
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("stripe", "none"),
                                 pattern_spacing = .04,
                                 pattern_angle = c(0, 30)
                               )))

# Display
p1

# Tidy it up and make a nice table
model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

dt <- dust(model1) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:4, round = 3) %>%
  sprinkle (col = 5, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Week A)", "Week B", "Week C")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "z-ratio",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  add_header_above(c("Fixed Effects: Proportions of Samples ≥ 40 dB LAeq by Week" = 5L), font_size=11) %>%
  save_kable(file = "byRun_Props.png", bs_theme = "flatly", zoom = 25)

###############################################################################

# LAeq >= 40 dB 

selected_colors <- c("#440154", "#2a788e", "#7ad151") 

# Filter non-zero rows
fullRuns_no0 <- subset(fullRuns, LAeq > 0) 

# Means within subjects only for visualizing
mean_by_subject <-fullRuns_no0 %>%
  group_by(ID, run) %>%
  summarize(LAeqMean = mean(LAeq, na.rm = TRUE))

# LMM 
m2 <- lmer(LAeq ~ run + (1 + run | ID), data = fullRuns_no0)
summary(m2)
tidy(m2)
performance(m2)

# Pariwise comparisons with FDR corrections if you want
#emmeans(m2, list(pairwise ~ run), adjust = "fdr") #, lmer.df = "kenward-roger")

# Diagnostics if you want 
#plot(m2)
#residuals <- resid(m2, type = "pearson")
#qqnorm(residuals)
#qqline(residuals)

# ICCs if you want
#var_components <- as.data.frame(VarCorr(m2))
#residual_variance <- attr(VarCorr(m1), "sc")^2
#random_intercept_variance <- var_components$vcov[1, 1]
#icc <- random_intercept_variance / (random_intercept_variance + residual_variance)

# These are the modeled means--you could also extract them from m2 if you want to do it that way
meanA <- 70.9 
meanB <- 69.6
meanC <- 67.9

mean_values <- data.frame(
  run = c("A", "B", "C"),
  mean = c(meanA, meanB, meanC)
)

# Make a boxplot

p2 <- ggplot(mean_by_subject, aes(x=run, y=LAeqMean)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = run),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_values, aes(x = run, y = mean),
             color = "black", shape = 4, size = 10, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("LAeq (dB)") +
  xlab("") +
  scale_color_manual(values = setNames(selected_colors, unique(mean_by_subject$run))) +
  labs(title ="LAeq by Week") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin


# Display
p2

# Tidy it up and make a nice table
model2 <- tidy(m2, effects = c("fixed"))
model2 <- subset(model2, select = -c(1))

dt <- dust(model2) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:5, round = 3) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Week A)", "Week B", "Week C")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq (Samples ≥ 40 dB) by Week" = 6L), font_size=11) %>%
  save_kable(file = "byRun_LAeq.png", bs_theme = "flatly", zoom = 25)

################################################################################

## Entropy ##

# LMM for entropy (H)
m3 <- lmer(H ~ run + (1 | ID), data = fullRuns_no0)
summary(m3)
anova(m3)
tidy(m3)
performance(m3)

# Pariwise comparisons with FDR corrections if you want
emmeans(m3, list(pairwise ~ run),adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m3, reverse = TRUE)

# These are the modeled means--you could also extract them from m2 if you want to do it that way
meanHA <- 3.56
meanHB <- 3.54
meanHC <- 3.55

mean_valuesH <- data.frame(
  run = c("A", "B", "C"),
  mean = c(meanHA, meanHB, meanHC)
)

# Tidy it up and make a nice table

model3 <- tidy(m3, effects = c("fixed"))
model3 <- select(model3, -c(effect))

# Tidy it up and make a nice table
dt <- dust(model3) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:5, round = 3) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Week A)", "Week B", "Week C")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "9em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq Entropy (Samples ≥ 40 dB) by Week" = 6L), font_size=11) %>%
  save_kable(file = "byRun_LAeHq.png", bs_theme = "flatly", zoom = 25)

# LAeq density

# Plot densities
p3 <- ggplot(fullRuns_no0, aes(x = LAeq, color = run)) +
  stat_density(geom="line", position="identity", aes(linetype = run)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(fullRuns_no0$run))) +
  ylab("Density") +
  xlab("LAeq (dB)") +
  labs(title ="LAeq Densities by Week") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, r = 2.5, l = 0, unit = "cm"))  # Adjust the legend margin

# Display
p3

# Set factor levels for run
fullRuns_no0$H  <- factor(fullRuns_no0$H, levels = c("A", "B", "C"))

# Plot entropy boxplots
p4 <- ggplot(fullRuns_no0, aes(x=run, y=H)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = run),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesH, aes(x = run, y = mean),
             color = "black", shape = 4, size = 10, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(byRun_excludeZero_H$run))) +
  ylab("Entropy") +
  xlab("") +
  labs(title ="Entropy by Week") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin

# Display
p4

# Combine the plots and save if you want
p <- (p1 + p2) / (p3 + p4)
p
#ggsave(plot = p, width = 15, height = 12, dpi = 300, filename = "byWeek.png")

################################################################################
################################################################################
# By Season
################################################################################

## LAeq ##

# Proportions < / >= 40 dB LAeq

# Use fullRuns

selected_colors <- c("#440154", "#2a788e", "#7ad151") 

# Count observations per season
season_counts <- fullRuns%>%
  group_by(season) %>%
  summarise(count = n())

print(season_counts)

fullRuns$season <- factor(data$season, levels = c("Fall", "Winter", "Spring"))

# Calculate subject means and then means by season
proportions_by_subject_within_season <- fullRuns %>%
  group_by(season, ID, zeroOrOne) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = zeroOrOne, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = `0` + `1`,
    Proportion_zero = `0` / Total,
    Proportion_one = `1` / Total
  ) 

proportions_by_subject_within_season <- proportions_by_subject_within_season[c("run", "ID", "Proportion_zero", "Proportion_one")]

# Calculate the mean proportions of zeros and ones for each season
mean_proportions_by_season <- proportions_by_subject_within_season %>%
  group_by(run) %>%
  summarise(
    Zero = mean(Proportion_zero),
    One = mean(Proportion_one)
  ) %>%
  ungroup()

print(mean_proportions_by_season)

prop_data <- mean_proportions_by_season %>%
  pivot_longer(cols = c(Zero, One),
               names_to = "zeroOrOne",
               values_to = "proportion")

# GLMM for proportion differences by weeks 
m1 <- glmer(zeroOrOne ~ season + (1 + season | ID), data = fullRuns, family = binomial(link="logit"))
summary(m1)
tidy(m1)
performance(m1)

# Diagnostics if you want
#plot(m1)
#residuals <- resid(m1, type = "pearson")
#qqnorm(residuals)
#qqline(residuals)

# Pariwise comparisons with FDR corrections if you want
#emmeans(m1, list(pairwise ~ season), type="response", reverse= "TRUE", adjust = "fdr") #, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

# ICCs if you want
#calc ICC
#var_components <- as.data.frame(VarCorr(m1))
#residual_variance <- attr(VarCorr(m1), "sc")^2
#icc <- var_components$vcov / (var_components$vcov + residual_variance)

# Median odds ratio if you want
mor_effect <- effect("season", m1)
mor_value <- median(mor_effect$fit)


# Create a side-by-side barplot
p1 <- ggplot(prop_data, aes(x = season, y = proportion, fill = zeroOrOne)) +
  geom_col_pattern(width = 0.7, position = "dodge", pattern = c('stripe', 'none', 'stripe', 'none', 'stripe', 'none'), pattern_density = .1,
                   pattern_spacing = .04,
                   pattern_fill = 'black') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion") +
  xlab("") +
  labs(title ="Proportions ≥ 40 dB LAeq by Season") +
  labs(subtitle = "All Samples") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("Quiet" = "#9f2a63", "Sound" = "#fc9f07"),
                    breaks = c("Quiet", "Sound"),
                    labels = c("< 40 dB LAeq", "≥ 40 dB LAeq")) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin

# Display
p1

# Tidy it up and make a nice table
model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

dt <- dust(model1) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:4, round = 3) %>%
  sprinkle (col = 5, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Fall)", "Spring", "Winter")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "z-statistic",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "9em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  add_header_above(c("Fixed Effects: Proportions of Samples ≥ 40 dB LAeq by Season" = 5L), font_size=11) %>%
  save_kable(file = "bySeason_Prop.png", bs_theme = "flatly", zoom = 25)

################################################################################

# LAeq >= 40 dB 

# LMM
m2 <- lmer(LAeq ~ season + (1 + season | ID), data = fullRuns_no0)
tidy(m2)
performance(m2)
summary(m2)

# Diagnoistics if you need to
#plot(m2)
#residuals <- resid(m2, type = "pearson")
#qqnorm(residuals)
#qqline(residuals)

# Pariwise comparisons with FDR corrections if you want
emmeans(m2, list(pairwise ~ season), adjust = "fdr")#, lmer.df = "kenward-roger")

# These are the modeled means--you could also extract them from m2 if you want to do it that way
meanF <- 71.8
meanW <- 70.4
meanS <- 68.2

mean_valuesS <- data.frame(
  season = c("Fall", "Winter", "Spring"),
  mean = c(meanF, meanW, meanS)
)

# Get means for each subject for each season
mean_by_subject <- fullRuns_no0 %>%
  group_by(ID, season) %>%
  summarize(LAeqMean = mean(LAeq, na.rm = TRUE))

# Create a boxplot
p2 <- ggplot(mean_by_subject, aes(x=season, y=LAeqMean)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = season),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesS, aes(x = season, y = mean),
             color = "black", shape = 4, size = 10, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("LAeq (dB)") +
  xlab("") +
  scale_color_manual(values = setNames(selected_colors, unique(mean_by_subject$season))) +
  labs(title ="LAeq by Season") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin

# Display
p2

# Tidy it up and make a nice table
model2 <- tidy(m2, effects = c("fixed"))
model2 <- subset(model2, select = -c(1))

dt <- dust(model2) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:5, round = 3) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Fall)", "Winter", "Spring")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "9em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq (Samples ≥ 40 dB) by Season" = 6L), font_size=11) %>%
  save_kable(file = "bySeason_LAeq.png", bs_theme = "flatly", zoom = 25)

################################################################################

## Entropy ##

# LAeq density

# Plot densities
p3 <- ggplot(fullRuns_no0, aes(x = LAeq, color = season)) +
  stat_density(geom="line", position="identity", aes(linetype = season)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(data$season))) +
  #scale_color_viridis(discrete = TRUE) +
  ylab("Density") +
  xlab("LAeq (dB)") +
  labs(title ="LAeq Densities by Season") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, r = 2.5, l = 0, unit = "cm"))  # Adjust the legend margin

# Display
p3

# LMM 
m3 <- lmer(H ~ season + (1|ID), data = fullRuns_no0)
summary(m3)
anova(m3)
tidy(m3)
performance(m3)

# Pairwise comparisons if you want to
emmeans(m3, list(pairwise ~ season), adjust = "fdr")#, lmer.df = "kenward-roger")

# Tidy it up and make a nice table
model3 <- tidy(m3, effects = c("fixed"))
model3 <- subset(model3, select = -c(1))

dt <- dust(model3) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:4, round = 3) %>%
  sprinkle (col = 5, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Fall)", "Spring", "Winter")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "z-statistic",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "9em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq Entropy (Samples ≥ 40 dB) by Season" = 5L), font_size=11) %>%
  save_kable(file = "bySeason_LAeqH.png", bs_theme = "flatly", zoom = 25)

# These are the modeled means--you could also extract them from m2 if you want to do it that way
meanHF <- 3.61
meanHW <- 3.55
meanHS <- 3.58

mean_valuesHS <- data.frame(
  season = c("Fall", "Winter", "Spring"),
  mean = c(meanHF, meanHW, meanHS)
)

# Make a boxplot
p4 <- ggplot(fullRuns_no0, aes(x=season, y=H)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = season),
    position = position_jitter(width = 0.15),
    shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesHS, aes(x = season, y = mean),
             color = "black", shape = 4, size = 10, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(fullRuns_no0$season))) +
  ylab("Entropy") +
  xlab("") +
  labs(title ="Entropy by Season") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin

# Display
p4

# Combine plots if you want to and save
p <- (p1 + p2) / (p3 + p4)
p
#ggsave(plot = p, width = 15, height = 12, dpi = 300, filename = "bySeason.png")

################################################################################
################################################################################
# By Day
################################################################################

## LAeq ##

# Proportions < / >= 40 dB LAeq

# Use fullRuns

selected_colors <- c("#440154", "#482878", "#3e4989", "#31688e","#26828e", "#1f9e89", "#35b779", "#6ece58", "#b5de2b") 

#count observations per day
day_counts <- data %>%
  group_by(day) %>%
  summarise(count = n())

print(day_counts)

# Calculate subject means and then means by day
proportions_by_subject_within_day <- data %>%
  group_by(day, ID, zeroOrOne) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = zeroOrOne, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = `0` + `1`,
    Proportion_zero = `0` / Total,
    Proportion_one = `1` / Total
  ) 

proportions_by_subject_within_day <- proportions_by_subject_within_day[c("day", "ID", "Proportion_zero", "Proportion_one")]

# Calculate the mean proportions of zeros and ones for each season
mean_proportions_by_day <- proportions_by_subject_within_day %>%
  group_by(run) %>%
  summarise(
    Zero = mean(Proportion_zero),
    One = mean(Proportion_one)
  ) %>%
  ungroup()

print(mean_proportions_by_day)

prop_data <- mean_proportions_by_day %>%
  pivot_longer(cols = c(Zero, One),
               names_to = "zeroOrOne",
               values_to = "proportion")

order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
prop_data$day <- factor(prop_data$day, levels = order)
data$day <- factor(data$day, levels = order)

# GLMM
m1 <- glmer(zeroOrOne ~ day + (1 + day | ID), data = data, family = binomial(link="logit"))
summary(m1)
t <- tidy(m1)
print(t, n = Inf)
performance(m1)

# Diagnoistics if you need to
#plot(m1)
#residuals <- resid(m1, type = "pearson")
#qqnorm(residuals)
#qqline(residuals)

# Pairwise comparisons if you want to
emmeans(m1, list(pairwise ~ day), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

# ICCs if you want to
var_components <- as.data.frame(VarCorr(m1))
residual_variance <- attr(VarCorr(m1), "sc")^2
icc <- var_components$vcov / (var_components$vcov + residual_variance)

# Median Odds Ratio
mor_effect <- effect("day", m1)
mor_value <- median(mor_effect$fit)

# Create A side-by-side barplot
p1 <- ggplot(prop_data, aes(x = day, y = proportion, fill = zeroOrOne)) +
  geom_col_pattern(width = 0.7, position = "dodge", pattern = c('stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none'), pattern_density = .1,
                   pattern_spacing = .04,
                   pattern_fill = 'black') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion") +
  xlab("") +
  labs(title ="Proportions < / ≥ 40 dB LAeq by day") +
  labs(subtitle = "All Samples") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  scale_fill_manual(values = c("Quiet" = "#9f2a63", "Sound" = "#fc9f07"),
                    breaks = c("Quiet", "Sound"),
                    labels = c("< 40 dB LAeq", "≥ 40 dB LAeq")) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm")) + # Adjust the legend margin 
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display
p1

# Tidy it up and make a nice table
model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

dt <- dust(model1) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:4, round = 3) %>%
  sprinkle (col = 5, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Sunday)", "Monday", "Tueseday", "Wednesday", "Thursday", "Friday", "Saturday")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  add_header_above(c("Fixed Effects: Proportions of Samples ≥ 40 dB by day" = 5L), font_size=11) %>%
  save_kable(file = "byday_prop.png", bs_theme = "flatly", zoom = 25)

################################################################################

# LAeq >= 40 dB 

# use data_no0

# LMM for days
m2 <- lmer(LAeq ~ day + (1 | ID), data = data_no0)
summary(m2)
t <- tidy(m2)
print(t, n = Inf)
performance(m2)

# Pariwise comparisons wtih FDR corrections if you want to
emmeans(m2, list(pairwise ~ day), adjust = "fdr")#, lmer.df = "kenward-roger")

# Model diagnostics if you want to
plot(m2)
residuals <- resid(m2, type = "pearson")
qqnorm(residuals)
qqline(residuals)

# These are the modeled means--you could also extract them from m2 if you want to do it that way
meanSun <- 70.3
meanMon <- 69.3
meanTues <- 68.4
meanWed <- 68.3
meanThurs <- 68.4
meanFri <- 70.6
meanSat <- 71.1

mean_valuesW <- data.frame(
  day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  mean = c(meanSun, meanMon, meanTues, meanWed, meanThurs, meanFri, meanSat)
)

mean_by_subject <- fullRuns_no0 %>%
  group_by(ID, day) %>%
  summarize(LAeqMean = mean(LAeq, na.rm = TRUE))

# Make a bxcplot
p2 <- ggplot(mean_by_subject, aes(x=day, y=LAeqMean)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = day),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesW, aes(x = day, y = mean),
             color = "black", shape = 4, size = 5, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(mean_by_subject$day))) +
  ylab("LAeq (dB)") +
  xlab("") +
  labs(title ="LAeq by day") +
  labs(subtitle = "Samples ≥ 40 dB") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm")) + # Adjust the legend margin
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display
p2

# Tidy it up and make a nice table
model2 <- tidy(m2, effects = c("fixed"))
model2 <- subset(model2, select = -c(1))

dt <- dust(model1) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:5, round = 3) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Sunday)", "Monday", "Tueseday", "Wednesday", "Thursday", "Friday", "Saturday")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq (Samples ≥ 40 dB) by day" = 6L), font_size=11) %>%
  save_kable(file = "byday_LAeq.png", bs_theme = "flatly", zoom = 25)

################################################################################

## Entropy ##

# Plot densities
p3 <- ggplot(data_no0, aes(x = LAeq, color = day)) +
  stat_density(geom="line", position="identity", aes(linetype = day)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(days_no0$day))) +
  ylab("Density") +
  xlab("LAeq (dB)") +
  labs(title ="LAeq Densities by day") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin

# Display
p3

# LMM for entropy
m3 <- lmer(H ~ day + (1 | ID), data = data_no0)
summary(m3)
tidy(m3)
performance(m3)
# Pairwise comparisons with FDR corrections if you want to
emmeans(m3, list(pairwise ~ day), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")

# Tidy it up and make a nice table
dt <- dust(model3) %>%
  sprinkle(col = 2, round = 2) %>%
  sprinkle(col = 3:5, round = 3) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept (Sunday)", "Monday", "Tueseday", "Wednesday", "Thursday", "Friday", "Saturday")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: Entropy (Samples ≥ 40 dB) by day" = 6L), font_size=11) %>%
  save_kable(file = "byday_Entropy.png", bs_theme = "flatly", zoom = 25)

# These are the modeled means--you could also extract them from m2 if you want to do it that way
meanSun <- 2.81
meanMon <- 2.71
meanTues <- 2.91
meanWed <- 3.14
meanThurs <- 3.16
meanFri <- 3.22
meanSat <- 2.82

mean_valuesW <- data.frame(
  day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  mean = c(meanSun, meanMon, meanTues, meanWed, meanThurs, meanFri, meanSat)
)

hDatadays <- data_no0 %>%
  group_by(ID, day, run) %>%
  summarise(AvgEntropy = mean(H, na.rm = TRUE)) %>%
  group_by(ID, day) %>%
  summarise(AvgEntropyAcrossRuns = mean(AvgEntropy, na.rm = TRUE))

p4 <- ggplot(hDatadays, aes(x=day, y=AvgEntropyAcrossRuns)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = day),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesW, aes(x = day, y = mean),
             color = "black", shape = 4, size = 5, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(desired_colors, desired_order)) +
  ylab("Entropy") +
  xlab("") +
  labs(title ="Entropy by day") +
  labs(subtitle = "Samples ≥ 40 dB LAeq") + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title= element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.position = "right",  # Adjust the legend position
        legend.margin = margin(t = 0, unit = "cm"))  # Adjust the legend margin

# Display
p4

# Combine the plots if you want and save
p <- (p1 + p2) / (p3 + p4)
p
#ggsave(plot = p, width = 15, height = 12, dpi = 300, filename = "byday.png")

################################################################################
################################################################################
# Across the day
################################################################################

# Use data

data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
# Extract hour and minute without seconds
data$time <- format(data$actual_times, format = "%H:%M")

# Create a new column for time of day as numeric values (only hours and minutes)
time_parts <- strsplit(data$time, ":")
data$time_numeric <- as.numeric(sapply(time_parts, function(x) {
  as.numeric(x[1]) * 60 + as.numeric(x[2])
}))

# Here you need to figure out what codes in the data correspond to Midnight and midnight and 11:59 PM
#1709701200
#1701925200

# Set hour plot breaks
plot_breaks <- seq(from = 0, by = 60, length.out = 24)

selected_colors <- c("#440154", "#2a788e", "#7ad151")

# Create a scatter plot by time of day
p <- ggplot(data, aes(x = time_numeric, y = LAeq)) +
  geom_point(aes(color = run, shape = run), alpha = 0.03) +
  labs(x = "Time of day", y = "LAeq (dB)") +
  scale_x_continuous(
    breaks = plot_breaks,  # Specify the tick positions
    labels = c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00", "7:00 AM",
               "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM",
               "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(data$run))) +
  ylim(0, 125) +
  labs(title ="LAeq by Time of day") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text(size=20)) +
  theme(legend.title= element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

# Add the regression to the plot
p <- p + geom_smooth(formula = (y+40) ~ poly(x, 3), se = TRUE, color = "black")

# Display
p

# Save if you want do
#ggsave(plot = p, width = 10, height = 10, dpi = 300, filename = "byTime.png")

# Find the peaks of the function
fit <- lm(data$LAeq ~ poly(data$time_numeric, 3))
data$fitted_values <- predict(fit)

# Find the row where fitted values is equal to the maximum value
max_value <- max(data_noNA$fitted_values)
row_with_max_value <- data_noNA[data_noNA$fitted_values == max_value, ]
# Print the row with the maximum value
print(row_with_max_value)
# Find the row where fitted values is equal to the minimum value
min_value <- min(data$fitted_values)
row_with_min_value <- data[data$fitted_values == min_value, ]
# Print the row with the maximum value
print(row_with_min_value)

# Hour indices -- you may need to change the coded value here
x <- seq(from = 1709701200, by = 3600, length.out = 24)
#
hourly_values <- data %>%
  filter(time_numeric %in% x) %>%
  group_by(time_numeric) %>%
  summarize(avg_fitted_values = mean(fitted_values))
# Print the results
print(hourly_values)
differences <- numeric(nrow(hourly_values) - 1)

# Calculate differences between consecutive values
for (i in 1:(nrow(hourly_values) - 1)) {
  differences[i] <- hourly_values$avg_fitted_values[i + 1] - hourly_values$avg_fitted_values[i]
}
# Print the differences
print(differences)

#############

# Now we are going to calculate two linear regressions for day and night

# Define the time ranges for day and night
#day_range <- c(1709713320, 1709765580) #3:22 - 5:53 ; 52260
day_range <- c(202, 1073) #3:22 - 5:53 ; 52260
night_range1 <- c(0, 201) #12:00 - 3:21 : 12119
night_range2 <- c(1074, 1440) #5:53:01 - 11:59 : 22018

# Subset the data into "dayData" and "nightData"
dayData <- data %>%
  filter(time_numeric >= day_range[1] & time_numeric <= day_range[2])
nightData <- data %>%
  filter((time_numeric >= night_range1[1] & time_numeric <= night_range1[2]) |
           (time_numeric >= night_range2[1] & time_numeric <= night_range2[2]))
nightData <- nightData %>%
  mutate(time_numeric = ifelse(time_numeric <= 1709701200, time_numeric + 86400, time_numeric))

# Set to zero and wrap
dayData$time_numeric_zero <- dayData$time_numeric - 1709713319
nightData$time_numeric_zero <- nightData$time_numeric - 1709713319

# Print the resulting datasets
print("dayData:")
print(dayData)

print("nightData:")
print(nightData)

# LMMs for LAeq across tow piecewise linear functions for day and night

# day
m1 <- lmer(LAeq ~ time_numeric + (1 | ID), data = dayData)
summary(m1)
tidy(m1)
performance(m1)

# Tidy it up and make a nice table

model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

options(scipen = 999, digits = 10)

dt <- dust(model1) %>%
  sprinkle(col = 2, rows = 1, round = 2) %>%
  sprinkle(col = 2, rows = 2, round = 4) %>%
  sprinkle(col = 3, rows = 1, round = 3) %>%
  sprinkle(col = 3, rows = 2, round = 6) %>%
  sprinkle(col = 4:5, round = 3) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept", "Timestamp (minute)")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "12em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq (daytime) by Time" = 6L), font_size=11) %>%
  save_kable(file = "byTimestamp_day.png", bs_theme = "flatly", zoom = 25)

# Night

m2 <- lmer(LAeq ~ time_numeric + (1 | ID), data = nightData)
summary(m2)

tidy(m2)
performance(m2)

model2 <- tidy(m2, effects = c("fixed"))
model2 <- subset(model2, select = -c(1))

dt <- dust(model2) %>%
  sprinkle(col = 2, rows = 1, round = 2) %>%
  sprinkle(col = 2, rows = 2, round = 4) %>%
  sprinkle(col = 3, rows = 1, round = 3) %>%
  sprinkle(col = 3, rows = 2, round = 6) %>%
  sprinkle(col = 4:5, round = 2) %>%
  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
  sprinkle(cols = "term", 
           replace = c("Intercept", "Timestamp (minute)")) %>%
  sprinkle_colnames(term='Effect',
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "t-statistic",
                    df = "df",
                    p.value = "p-value")

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "12em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: LAeq (Nighttime) by Time" = 6L), font_size=11) %>%
  save_kable(file = "byTimestamp_Night.png", bs_theme = "flatly", zoom = 25)

###############################################################################