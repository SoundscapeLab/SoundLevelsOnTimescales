library(lme4)
library(lmerTest)
library(ggplot2)
library(patchwork)
library(emmeans) 
library(ggpubr)
library(MuMIn)
library(dplyr)
library(tidyr)
library(pixiedust)
library(broom.mixed)
library(kableExtra)
library(dplyr)
library(magrittr)
library(ggsignif)
library(ggpattern)
library(magick)
library(lubridate)
library(scales)
library(nlme)
library(viridis)
library(arm)
library(predictmeans)
library(sjstats)
library(MASS)
library(performance)
################################################################################

#datasets required: 
#mainData
#byRun_excludeZero_H
#DEData_with_Season
#byDayCombined
#DE_entropyByDay
#DE_seasonH
#DE_daysPropsPairwise
#DE_daysLAeqPairwise
#DE_daysEntropyPairwise

setwd("/Users/erikjorgenjorgensen/Desktop/Working Projects/Entropy_Dosimeter_Project/DE_dataSets")
mainData <- read.csv("mainData.csv")
byRun_excludeZero_H <- read.csv("byRun_excludeZero_H.csv")
DEData_with_Season <- read.csv("DEData_with_Season.csv")
byDayCombined <- read.csv("byDayCombined.csv")
DE_entropyByDay <- read.csv("DE_entropyByDay.csv")
DE_seasonH <- read.csv("DE_seasonH.csv")
DE_daysPropsPairwise <- read.csv("DE_daysPropsPairwise.csv")
DE_daysLAeqPairwise <- read.csv("DE_daysLAeqPairwise.csv")
DE_daysEntropyPairwise <- read.csv("DE_daysEntropyPairwise.csv")

setwd("/Users/erikjorgenjorgensen/Desktop/")

options(scipen = 0, digits = 7) 
Sys.setenv(TZ="America/Detroit")

data <- mainData
data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
data$date <- as.Date(data$actual_times)
data$time <- format(data$actual_times, format = "%H:%M:%S")

#count subjects
num_subjects <- data %>%
  summarize(num_unique_subjects = n_distinct(ID))

#get compliance data
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

fullData_no0 <- subset(mainData, LAeq > 0) #no zeros

#get compliance data
subject_counts <- fullData_no0 %>%
  group_by(ID) %>%
  summarize(count = n())

# Calculate the mean count of datapoints across all subjects
mean_count_datapoints <- mean(subject_counts$count)

# Calculate the standard error of the mean count of datapoints
std_err_count_datapoints <- sd(subject_counts$count) / sqrt(length(subject_counts$count))

# Print the mean count of datapoints and standard error
cat("Mean count of datapoints:", mean_count_datapoints, "\n")
cat("Standard Error:", std_err_count_datapoints, "\n")

#let's just look at complete runs
data$run <- as.character(data$run)
data$run <- trimws(data$run)
fullRuns <- data[data$run %in% c("A", "B", "C"), ]

#check ID count
unique_runs <- unique(fullRuns$run)
num_unique_runs <- length(unique_runs)

#count subjects
num_subjects <- fullRuns %>%
  summarize(num_unique_subjects = n_distinct(ID))

#get compliance data
subject_counts <- fullRuns %>%
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

selected_colors <- c("#440154", "#2a788e", "#7ad151") 

#all full run data
library(ggplot2)

# Your existing plot code
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

#display 
p
ggsave(plot = p, width = 15, height = 5, dpi = 300, filename = "byRunAll.png")

##############################################################################

#proportions

# Calculate proportions of 0 and 1 for each run
#prop_data <- fullRuns %>%
#  group_by(run) %>%
#  summarize(Quiet = mean(loud == 0),
#            Sound = mean(loud == 1))

#prop_data <- prop_data %>%
#  pivot_longer(cols = c(Quiet, Sound),
#               names_to = "loud",
#               values_to = "proportion")

data <- fullRuns

# first calculate subject means and then means by run
proportions_by_subject_within_run <- data %>%
  group_by(run, ID, loud) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = loud, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = `0` + `1`,
    Proportion_Quiet = `0` / Total,
    Proportion_Loud = `1` / Total
  ) 

proportions_by_subject_within_run <- proportions_by_subject_within_run[c("run", "ID", "Proportion_Quiet", "Proportion_Loud")]

# Step 2: Calculate the mean proportions of quiet and loud for each run
mean_proportions_by_run <- proportions_by_subject_within_run %>%
  group_by(run) %>%
  summarise(
    Quiet = mean(Proportion_Quiet),
    Sound = mean(Proportion_Loud)
  ) %>%
  ungroup()

print(mean_proportions_by_run)

prop_data <- mean_proportions_by_run %>%
  pivot_longer(cols = c(Quiet, Sound),
               names_to = "loud",
               values_to = "proportion")

# Create the side-by-side barplot

p1 <- ggplot(prop_data, aes(x = run, y = proportion, fill = loud)) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
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
  scale_fill_manual(values = c("Quiet" = "#9f2a63", "Sound" = "#fc9f07"),
                    breaks = c("Quiet", "Sound"),
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

p1
ggsave(plot = p1, width = 14, height = 12, dpi = 300, filename = "props_byWeek.png")

#stats
m1 <- glmer(loud ~ run + (1 + run | ID), data = fullRuns,family = binomial(link="logit"))
summary(m1)

tidy(m1)
performance(m1)

#plot(m1)
#residuals <- resid(m1, type = "pearson")
#qqnorm(residuals)
#qqline(residuals)

#calc ICC
#var_components <- as.data.frame(VarCorr(m1))
#residual_variance <- attr(VarCorr(m1), "sc")^2
#icc <- var_components$vcov / (var_components$vcov + residual_variance)

mor_effect <- effect("run", m1)
# Extract the median odds ratio
mor_value <- median(mor_effect$fit)


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
  save_kable(file = "byRun_prop.png", bs_theme = "flatly", zoom = 25)

emmeans(m1, list(pairwise ~ run), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)


###############################################################################

fullRuns_no0 <- subset(fullRuns, LAeq > 0) #no zeros

#get compliance data
subject_counts <- fullRuns_no0 %>%
  group_by(ID) %>%
  summarize(count = n())

# Calculate the mean count of datapoints across all subjects
mean_count_datapoints <- mean(subject_counts$count)

# Calculate the standard error of the mean count of datapoints
std_err_count_datapoints <- sd(subject_counts$count) / sqrt(length(subject_counts$count))

# Print the mean count of datapoints and standard error
cat("Mean count of datapoints:", mean_count_datapoints, "\n")
cat("Standard Error:", std_err_count_datapoints, "\n")

mean_by_subject <-fullRuns_no0 %>%
  group_by(ID, run) %>%
  summarize(LAeqMean = mean(LAeq, na.rm = TRUE))

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
  #scale_color_viridis(discrete = TRUE, option = 'turbo') +
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

p2

#stats
m1 <- lmer(LAeq ~ run + (1 + run | ID), data = fullRuns_no0)
summary(m1)
emmeans(m1, list(pairwise ~ run), adjust = "fdr")#, lmer.df = "kenward-roger")

meanA <- 70.9
meanB <- 69.6
meanC <- 67.9

mean_values <- data.frame(
  run = c("A", "B", "C"),
  mean = c(meanA, meanB, meanC)
)

tidy(m1)
performance(m1)

plot(m1)
residuals <- resid(m1, type = "pearson")
qqnorm(residuals)
qqline(residuals)

#calc ICC
var_components <- as.data.frame(VarCorr(m1))
residual_variance <- attr(VarCorr(m1), "sc")^2
random_intercept_variance <- var_components$vcov[1, 1]
icc <- random_intercept_variance / (random_intercept_variance + residual_variance)

model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

dt <- dust(model1) %>%
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

emmeans(m1, list(pairwise ~ run), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

###############################################################################
#density

selected_colors <- c("#440154", "#2a788e", "#7ad151") 

p3 <- ggplot(fullRuns_no0, aes(x = LAeq, color = run)) +
  #geom_density(aes(linetype = run)) +
  stat_density(geom="line", position="identity", aes(linetype = run)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(fullRuns_no0$run))) +
  #scale_color_viridis(discrete = TRUE) +
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
  #theme(legend.position = "none")

p3

byRun_excludeZero_H$run <- factor(byRun_excludeZero_H$run, levels = c("A", "B", "C"))
selected_colors <- c("#2a788e","#440154", "#7ad151") 

p4 <- ggplot(byRun_excludeZero_H, aes(x=run, y=H)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = run),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesH, aes(x = run, y = mean),
             color = "black", shape = 4, size = 10, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(byRun_excludeZero_H$run))) +
  #scale_color_viridis(discrete = TRUE) +
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
  #theme(legend.position = "none")

p4

p <- (p1 + p2) / (p3 + p4)
ggsave(plot = p, width = 15, height = 12, dpi = 300, filename = "byRun.png")

#stats
m1 <- lmer(H ~ run + (1 | ID), data = byRun_excludeZero_H)
summary(m1)
anova(m1)
tidy(m1)
performance(m1)
emmeans(m1, list(pairwise ~ run),adjust = "fdr")#, lmer.df = "kenward-roger")

meanHA <- 3.56
meanHB <- 3.54
meanHC <- 3.55

mean_valuesH <- data.frame(
  run = c("A", "B", "C"),
  mean = c(meanHA, meanHB, meanHC)
)

#pairs(m1, reverse = TRUE)

#model1 <- tidy(m1, effects = c("fixed"))
#model1 <- select(model1, -c(effect))

#dt <- dust(model1) %>%
#  sprinkle(col = 2, round = 2) %>%
#  sprinkle(col = 3:5, round = 3) %>%
#  sprinkle (col = 6, fn = quote(pvalString(value))) %>%
#  sprinkle(cols = "term", 
#           replace = c("Intercept (Week A)", "Week B", "Week C")) %>%
#  sprinkle_colnames(term='Effect',
#                    estimate = "Estimate",
#                    std.error = "SE",
#                    statistic = "t-statistic",
#                    df = "df",
#                    p.value = "p-value")

#kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
#  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
#  column_spec(1, width = "9em") %>%
#  column_spec(2, width = "5em") %>%
#  column_spec(3, width = "5em") %>%
#  column_spec(4, width = "6em") %>%
#  column_spec(5, width = "5em") %>%
#  column_spec(6, width = "5em") %>%
#  add_header_above(c("Fixed Effects: LAeq (Samples ≥ 40 dB) by Week" = 6L), font_size=11) %>%
#  save_kable(file = "byRun_LAeq.png", bs_theme = "flatly", zoom = 25)

#emmeans(m1, list(pairwise ~ run), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)


################################################################################

#seasonal proportions
data = fullRuns
data$actual_times <- as.POSIXct(data$actual_times)

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

#prop_data <- data %>%
#  group_by(season) %>%
#  summarize(Quiet = mean(loud == 0),
#            Sound = mean(loud == 1))

#prop_data <- prop_data %>%
#  pivot_longer(cols = c(Quiet, Sound),
#               names_to = "loud",
#               values_to = "proportion")

#prop_data <- na.omit(prop_data) 

# first calculate subject means and then means by season
proportions_by_subject_within_season <- data %>%
  group_by(season, ID, loud) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = loud, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = `0` + `1`,
    Proportion_Quiet = `0` / Total,
    Proportion_Loud = `1` / Total
  )

proportions_by_subject_within_season <- proportions_by_subject_within_season[c("season", "ID", "Proportion_Quiet", "Proportion_Loud")]


# Step 2: Calculate the mean proportions of quiet and loud for each season
mean_proportions_by_season <- proportions_by_subject_within_season %>%
  group_by(season) %>%
  summarise(
    Quiet = mean(Proportion_Quiet),
    Sound = mean(Proportion_Loud)
  ) %>%
  ungroup()


print(mean_proportions_by_season)

prop_data <- mean_proportions_by_season %>%
  pivot_longer(cols = c(Quiet, Sound),
               names_to = "loud",
               values_to = "proportion")
prop_data <- na.omit(prop_data) 

desired_order <- c("Fall", "Winter", "Spring")
prop_data$season <- factor(prop_data$season, levels = desired_order)

# Create the side-by-side barplot
p1 <- ggplot(prop_data, aes(x = season, y = proportion, fill = loud)) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
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

p1

#stats
data$loud <- as.factor(data$loud)
m1 <- glmer(loud ~ season + (1 + season | ID), data = data,family = binomial(link="logit"))
summary(m1)

tidy(m1)
performance(m1)

plot(m1)
residuals <- resid(m1, type = "pearson")
qqnorm(residuals)
qqline(residuals)

#calc ICC
var_components <- as.data.frame(VarCorr(m1))
residual_variance <- attr(VarCorr(m1), "sc")^2
icc <- var_components$vcov / (var_components$vcov + residual_variance)

mor_effect <- effect("season", m1)
# Extract the median odds ratio
mor_value <- median(mor_effect$fit)

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
  save_kable(file = "bySeason_prop.png", bs_theme = "flatly", zoom = 25)

emmeans(m1, list(pairwise ~ season), type="response", reverse= "FALSE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

##########################################################
#seasonal LAeq

data = fullRuns_no0
data$actual_times <- as.POSIXct(data$actual_times)

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

####################################
data <- DEData_with_Season
data <- na.omit(data)

desired_order <- c("Fall", "Winter", "Spring")
data$season <- factor(data$season, levels = desired_order)

mean_by_subject <- data %>%
  group_by(ID, season) %>%
  summarize(LAeqMean = mean(LAeq, na.rm = TRUE))

selected_colors <- c("#440154", "#2a788e", "#7ad151") 

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
  #scale_color_viridis(discrete = TRUE) +
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

p2

#stats
m1 <- lmer(LAeq ~ season + (1 + season | ID), data = data)
summary(m1)
emmeans(m1, list(pairwise ~ season), adjust = "fdr")#, lmer.df = "kenward-roger")

meanF <- 71.8
meanW <- 70.4
meanS <- 68.2

mean_valuesS <- data.frame(
  season = c("Fall", "Winter", "Spring"),
  mean = c(meanF, meanW, meanS)
)


tidy(m1)
performance(m1)

plot(m1)
residuals <- resid(m1, type = "pearson")
qqnorm(residuals)
qqline(residuals)

model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

dt <- dust(model1) %>%
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

emmeans(m1, list(pairwise ~ season), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

#####
#Season Entropy

#density
desired_order <- c("Fall", "Winter", "Spring")
data$season <- factor(data$season, levels = desired_order)

p3 <- ggplot(data, aes(x = LAeq, color = season)) +
  #geom_density() +
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

p3

desired_order <- c("Fall", "Winter", "Spring")
DE_seasonH$season <- factor(DE_seasonH$season, levels = desired_order)

p4 <- ggplot(DE_seasonH, aes(x=season, y=entropy)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = season),
    position = position_jitter(width = 0.15),
    shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesHS, aes(x = season, y = mean),
             color = "black", shape = 4, size = 10, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(DE_seasonH$season))) +
  #scale_color_viridis(discrete = TRUE) +
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

p4

p <- (p1 + p2) / (p3 + p4)
ggsave(plot = p, width = 15, height = 12, dpi = 300, filename = "bySeason.png")

#stats
m1 <- lmer(entropy ~ season + (1|ID), data = DE_seasonH)
summary(m1)
anova(m1)
tidy(m1)
performance(m1)

emmeans(m1, list(pairwise ~ season), adjust = "fdr")#, lmer.df = "kenward-roger")

meanHF <- 3.61
meanHW <- 3.55
meanHS <- 3.58

mean_valuesHS <- data.frame(
  season = c("Fall", "Winter", "Spring"),
  mean = c(meanHF, meanHW, meanHS)
)




###############

#count observations per season
season_counts <- data %>%
  group_by(season) %>%
  summarise(count = n())

print(season_counts)

################################################################################
#Day
#byDay Proportions
data <- mainData
data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
data$date <- as.Date(data$actual_times)
data$time <- format(data$actual_times, format = "%H:%M:%S")

selected_colors <- c("#440154", "#482878", "#3e4989", "#31688e","#26828e", "#1f9e89", "#35b779", "#6ece58", "#b5de2b") 

#all full run day
p <- ggplot(data, aes(x=date, y=LAeq, color = run)) +
  geom_point(alpha = 0.075) +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %Y") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(data$run))) +
  #scale_color_viridis(discrete = TRUE) +
  ylab("LAeq (dB)") +
  xlab("") +
  labs(title ="All LAeq Values by Week and Date") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 25)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.title= element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

#display 
p
ggsave(plot = p, width = 15, height = 5, dpi = 300, filename = "byRunAllwithPartials.png")


# Calculate proportions of 0 and 1 for each day
byDayCombined$loud <- ifelse(byDayCombined$LAeq > 0, 1, 0)

#prop_data <- byDayCombined %>%
#  group_by(Day) %>%
#  summarize(Quiet = mean(loud == 0),
#            Sound = mean(loud == 1))

data <- byDayCombined

# first calculate subject means and then means by run
proportions_by_subject_within_day <- data %>%
  group_by(Day, ID, loud) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = loud, values_from = Count, values_fill = 0) %>%
  mutate(
    Total = `0` + `1`,
    Proportion_Quiet = `0` / Total,
    Proportion_Loud = `1` / Total
  )

proportions_by_subject_within_day <- proportions_by_subject_within_day[c("Day", "ID", "Proportion_Quiet", "Proportion_Loud")]


# Step 2: Calculate the mean proportions of quiet and loud for each day
mean_proportions_by_day <- proportions_by_subject_within_day %>%
  group_by(Day) %>%
  summarise(
    Quiet = mean(Proportion_Quiet),
    Sound = mean(Proportion_Loud)
  ) %>%
  ungroup()

print(mean_proportions_by_day)

prop_data <- mean_proportions_by_day %>%
  pivot_longer(cols = c(Quiet, Sound),
               names_to = "loud",
               values_to = "proportion")

desired_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
prop_data$Day <- factor(prop_data$Day, levels = desired_order)

# Create the side-by-side barplot
p1 <- ggplot(prop_data, aes(x = Day, y = proportion, fill = loud)) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_col_pattern(width = 0.7, position = "dodge", pattern = c('stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none', 'stripe', 'none'), pattern_density = .1,
                   pattern_spacing = .04,
                   pattern_fill = 'black') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Proportion") +
  xlab("") +
  labs(title ="Proportions < / ≥ 40 dB LAeq by Day") +
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

p1

#stats

byDayCombined$Day <- factor(byDayCombined$Day, levels = desired_order)

m1 <- glmer(loud ~ Day + (1 + Day | ID), data = byDayCombined,family = binomial(link="logit"))
summary(m1)

t <- tidy(m1)
print(t, n = Inf)
performance(m1)

#calc ICC
var_components <- as.data.frame(VarCorr(m1))
residual_variance <- attr(VarCorr(m1), "sc")^2
icc <- var_components$vcov / (var_components$vcov + residual_variance)

mor_effect <- effect("Day", m1)
# Extract the median odds ratio
mor_value <- median(mor_effect$fit)

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
  add_header_above(c("Fixed Effects: Proportions of Samples ≥ 40 dB by Day" = 5L), font_size=11) %>%
  save_kable(file = "byDay_prop.png", bs_theme = "flatly", zoom = 25)

emmeans(m1, list(pairwise ~ Day), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

#table pairwise
#dt <- read.csv('DE_daysPropsPairwise.csv', header=TRUE, check.names=FALSE)
dt <- DE_daysPropsPairwise
kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "12em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  add_header_above(c("Pairwise Comparisons: Proportions of Samples ≥ 40 dB by Day" = 5L), font_size=11) %>%
  save_kable(file = "byDay_props_pairwise.png", bs_theme = "flatly", zoom = 25)

#count observations per day
day_counts <- byDayCombined %>%
  group_by(Day) %>%
  summarise(count = n())

print(day_counts)

##########

days_no0 <- subset(byDayCombined, LAeq > 0) #no zeros
days_no0$Day <- factor(days_no0$Day, levels = desired_order)

mean_by_subject <- days_no0 %>%
  group_by(ID, Day) %>%
  summarize(LAeqMean = mean(LAeq, na.rm = TRUE))

selected_colors <- c("#440154", "#46327e", "#365c8d", "#277f8e","#1fa187", "#4ac16d", "#a0da39") 

p2 <- ggplot(mean_by_subject, aes(x=Day, y=LAeqMean)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = Day),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesW, aes(x = Day, y = mean),
             color = "black", shape = 4, size = 5, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(mean_by_subject$Day))) +
  #scale_color_viridis(discrete = TRUE) +
  ylab("LAeq (dB)") +
  xlab("") +
  labs(title ="LAeq by Day") +
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

p2

#day sound environments stats
m1 <- lmer(LAeq ~ Day + (1 | ID), data = days_no0)
summary(m1)
emmeans(m1, list(pairwise ~ Day),adjust = "fdr")#, lmer.df = "kenward-roger")

meanSun <- 70.3
meanMon <- 69.3
meanTues <- 68.4
meanWed <- 68.3
meanThurs <- 68.4
meanFri <- 70.6
meanSat <- 71.1
  
  
mean_valuesW <- data.frame(
  Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  mean = c(meanSun, meanMon, meanTues, meanWed, meanThurs, meanFri, meanSat)
)


t <- tidy(m1)
print(t, n = Inf)
performance(m1)

plot(m1)
residuals <- resid(m1, type = "pearson")
qqnorm(residuals)
qqline(residuals)

model1 <- tidy(m1, effects = c("fixed"))
model1 <- subset(model1, select = -c(1))

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
  add_header_above(c("Fixed Effects: LAeq (Samples ≥ 40 dB) by Day" = 6L), font_size=11) %>%
  save_kable(file = "byDay_LAeq.png", bs_theme = "flatly", zoom = 25)



#table pairwise
#dt <- read.csv('DE_daysLAeqPairwise.csv', header=TRUE, check.names=FALSE)
dt <- DE_daysLAeqPairwise
kable(dt, align=c('l', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "12em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  add_header_above(c("Pairwise Comparisons: LAeq (Samples ≥ 40 dB) by Day" = 5L), font_size=11) %>%
  save_kable(file = "byDay_LAeq_pairwise.png", bs_theme = "flatly", zoom = 25)

#density
p3 <- ggplot(days_no0, aes(x = LAeq, color = Day)) +
  #geom_density() +
  stat_density(geom="line", position="identity", aes(linetype = Day)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(days_no0$Day))) +
  #scale_color_viridis(discrete = TRUE) +
  ylab("Density") +
  xlab("LAeq (dB)") +
  labs(title ="LAeq Densities by Day") +
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

p3

data <- DE_entropyByDay %>%
  group_by(ID, Day, run) %>%
  summarise(AvgEntropy = mean(entropy, na.rm = TRUE)) %>%
  group_by(ID, Day) %>%
  summarise(AvgEntropyAcrossRuns = mean(AvgEntropy, na.rm = TRUE))

data$Day <- factor(data$Day, levels = desired_order)

# First, define the desired order of days and corresponding colors
desired_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
desired_colors <- c("#440154", "#46327e", "#365c8d", "#277f8e","#1fa187", "#4ac16d", "#a0da39")

# Then, reorder the levels of 'Day' based on the desired order
data$Day <- factor(data$Day, levels = desired_order)

p4 <- ggplot(data, aes(x=Day, y=AvgEntropyAcrossRuns)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color = Day),
             position = position_jitter(width = 0.15),
             shape = 16, size = 2, alpha = 0.75) +
  geom_point(data = mean_valuesW, aes(x = Day, y = mean),
             color = "black", shape = 4, size = 5, stroke = 1.5) +  # shape = 4 is an "x"
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(desired_colors, desired_order)) +
  #scale_color_viridis(discrete = TRUE) +
  ylab("Entropy") +
  xlab("") +
  labs(title ="Entropy by Day") +
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

p4

p <- (p1 + p2) / (p3 + p4)
ggsave(plot = p, width = 15, height = 12, dpi = 300, filename = "byDay.png")

#entropy stats
DE_entropyByDay$Day <- factor(DE_entropyByDay$Day, levels = desired_order)
m1 <- lmer(entropy ~ Day + (1 | ID), data = DE_entropyByDay)
summary(m1)
emmeans(m1, list(pairwise ~ Day), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")

meanSun <- 2.81
meanMon <- 2.71
meanTues <- 2.91
meanWed <- 3.14
meanThurs <- 3.16
meanFri <- 3.22
meanSat <- 2.82


mean_valuesW <- data.frame(
  Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
  mean = c(meanSun, meanMon, meanTues, meanWed, meanThurs, meanFri, meanSat)
)

tidy(m1)
performance(m1)

model1 <- tidy(m1, effects = c("fixed"))
model1 <- model1[ , -1]

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

kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Fixed Effects: Entropy (Samples ≥ 40 dB) by Day" = 6L), font_size=11) %>%
  save_kable(file = "byDay_Entropy.png", bs_theme = "flatly", zoom = 25)

emmeans(m1, list(pairwise ~ Day), type="response", reverse= "TRUE", adjust = "fdr")#, lmer.df = "kenward-roger")
#pairs(m1, reverse = TRUE)

#pairwise table
#dt <- read.csv('DE_daysEntropyPairwise.csv', header=TRUE, check.names=FALSE)
dt <- DE_daysEntropyPairwise
kable(dt, align=c('l', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kable_styling(font_size = 11, full_width = FALSE, html_font = 'arial', htmltable_class = 'lightable-classic-2') %>%
  column_spec(1, width = "12em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "6em") %>%
  column_spec(5, width = "5em") %>%
  column_spec(6, width = "5em") %>%
  add_header_above(c("Pairwise Comparisons: Entropy (Samples ≥ 40 dB LAeq) by Day" = 6L), font_size=11) %>%
  save_kable(file = "byDay_EntropyPairwise.png", bs_theme = "flatly", zoom = 25)

################################################################################
#by second

data <- mainData
#data <- subset(mainData, LAeq > 0) #no zeros
#data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
#data$time <- format(as.POSIXct(data$actual_times, origin = "1970-01-01", tz = "America/Detroit"), format = "%H:%M:%S")
#data <- na.omit(data)
#strptime(data$time[1], format = "%H:%M:%S")

# Create a new column for time of day as numeric values
#data$time_numeric <- as.numeric(as.POSIXlt(data$time, format = "%H:%M:%S"))

# Convert actual times to POSIXct format
data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")

# Extract hour and minute without seconds
data$time <- format(data$actual_times, format = "%H:%M")

# Create a new column for time of day as numeric values (only hours and minutes)
time_parts <- strsplit(data$time, ":")
data$time_numeric <- as.numeric(sapply(time_parts, function(x) {
  as.numeric(x[1]) * 60 + as.numeric(x[2])
}))



#1709701200
#1701925200

#plot_breaks <- seq(from = 1713844800, by = 3600, length.out = 24)
plot_breaks <- seq(from = 0, by = 60, length.out = 24)

#selected_colors <- c("#440154", "#482878", "#3e4989", "#31688e","#26828e", "#1f9e89", "#35b779", "#6ece58", "#b5de2b") 

selected_colors <- c("#440154", "#2a788e", "#7ad151")

# Create a scatter plot by time of day
scatter_plot <- ggplot(data, aes(x = time_numeric, y = LAeq)) +
  geom_point(aes(color = mainRun, shape = mainRun), alpha = 0.03) +
  labs(x = "Time of Day", y = "LAeq (dB)") +
  scale_x_continuous(
    breaks = plot_breaks,  # Specify the tick positions
    labels = c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", "4:00 AM", "5:00 AM", "6:00", "7:00 AM",
               "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM",
               "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM", "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = setNames(selected_colors, unique(data$mainRun))) +
  #scale_color_viridis(discrete = TRUE) +
  ylim(0, 125) +
  labs(title ="LAeq by Time of Day") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 25)) +
  theme(axis.title = element_text(size = 20)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text(size=20)) +
  theme(legend.title= element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

scatter_plot

# Fit a 3rd-degree polynomial regression
#regression_model <- lm(LAeq ~ poly(time_numeric, 3), data = data)

# Add the regression line to the plot
scatter_plot_with_regression <- scatter_plot +
  geom_smooth(formula = (y+40) ~ poly(x, 3), se = TRUE, color = "black")

#scatter_plot_with_regression <- scatter_plot +
#  geom_smooth(formula = (y + 40) ~ poly(x, 3), se = TRUE, color = "black")

# Display the plot
print(scatter_plot_with_regression)

ggsave(plot = scatter_plot_with_regression, width = 10, height = 10, dpi = 300, filename = "byTime_2.png")

#ggsave(plot = scatter_plot, width = 10, height = 10, dpi = 300, filename = "scatter.png")
########

data_noNA <-  data %>% filter(!is.na(LAeq))
data_noNA <-  data_noNA %>% filter(!is.na(time_numeric))

fit <- lm(data_noNA$LAeq ~ poly(data_noNA$time_numeric, 3))
data_noNA$fitted_values <- predict(fit)

#get derivatives
pc <- coef(fit)
x <- seq(from = 1709701200, by = 3600, length.out = 24)
#x <- seq(1, 267420, 1000)
d1 <- g(x, pc, 1)
print(d1)

df <- data.frame(x,d1)

ggplot(df, aes(x=x, y=d1)) +
  geom_point()

######################################################

fit <- lm(data$LAeq ~ poly(data$time_numeric, 3))
data$fitted_values <- predict(fit)

# Find the row where fitted_values is equal to the maximum value
max_value <- max(data_noNA$fitted_values)
row_with_max_value <- data_noNA[data_noNA$fitted_values == max_value, ]
# Print the row with the maximum value
print(row_with_max_value)

# Find the row where fitted_values is equal to the maximum value
min_value <- min(data$fitted_values)
row_with_min_value <- data[data$fitted_values == min_value, ]

# Print the row with the maximum value
print(row_with_min_value)

#hour indices
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

# Define the time ranges for day and night
#day_range <- c(1709713320, 1709765580) #3:22 - 5:53 ; 52260
#night_range1 <- c(1709701200, 1709713319) #12:00 - 3:21 : 12119
#night_range2 <- c(1709765581, 1709787599) #5:53:01 - 11:59 : 22018

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

#set to zero and wrap
dayData$time_numeric_zero <- dayData$time_numeric - 1709713319
nightData$time_numeric_zero <- nightData$time_numeric - 1709713319


# Print the resulting datasets
print("dayData:")
print(dayData)

print("nightData:")
print(nightData)

m1 <- lmer(LAeq ~ time_numeric + (1 | ID), data = dayData)
summary(m1)

tidy(m1)
performance(m1)

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
  add_header_above(c("Fixed Effects: LAeq (Daytime) by Time" = 6L), font_size=11) %>%
  save_kable(file = "byTimestamp_Day.png", bs_theme = "flatly", zoom = 25)

###################################

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

data <- mainData

data$actual_times <- as.POSIXct(data$actual_times, format = "%d-%b-%Y %H:%M:%S")
data$time <- format(as.POSIXct(data$actual_times, origin = "1970-01-01", tz = "America/Detroit"), format = "%H:%M:%S")

# Arrange the data by ID and actual_times
data <- data %>%
  arrange(ID, actual_times)

# Filter for the first data point in each mainRun for each ID
first_points <- data %>%
  group_by(ID, mainRun) %>%
  filter(row_number() == 1)

# Calculate the time difference between consecutive first data points for each ID
time_diffs <- first_points %>%
  group_by(ID) %>%
  mutate(time_diff = actual_times - lag(actual_times))

# Print the resulting data frame
print(time_diffs)

# get average for diff between A and B
mainRunB <- time_diffs %>%
  filter(mainRun == "B")

mean(mainRunB$time_diff)
sd(mainRunB$time_diff)
min(mainRunB$time_diff)
max(mainRunB$time_diff)

# get average for diff between A and B
mainRunC <- time_diffs %>%
  filter(mainRun == "C")

mean(mainRunC$time_diff)
sd(mainRunC$time_diff)
min(mainRunC$time_diff)
max(mainRunC$time_diff)
