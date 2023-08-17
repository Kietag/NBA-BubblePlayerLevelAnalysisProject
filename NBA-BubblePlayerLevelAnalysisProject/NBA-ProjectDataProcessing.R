# NBA-BubblePlayerLevelAnalysisProject

# This script is for the data cleaning and processing of scraped data followed by analysis.
# This is the 0.2 version.

# Load relevant libraries
library(rvest)
library(dplyr)
library(tidyr)
library(forcats)
library(lmerTest)
library(ggplot2)
library(lme4)
library(lmtest)
library(car)
library(here)
library(effects)
library(performance)
library(lattice)


# Load the 'Advanced' .rda file which has been scraped using the datascraper script.
here::here()
load(file.path("data" , "Advanced.rda"))

#### Data Cleaning and Processing ####

# Create a new data frame for processed data from the raw scraped data
AdvancedProcessed <- Advanced

# Turn age into a numeric value
AdvancedProcessed$Age <- as.numeric(as.character(AdvancedProcessed$Age))

# Remove NA values from data frame
AdvancedProcessed <- AdvancedProcessed[!is.na(AdvancedProcessed$Age), ]

# Remove the headings from BasketballReference that have been scraped
AdvancedProcessed <- subset(AdvancedProcessed, !grepl("Career", AdvancedProcessed$Season))
AdvancedProcessed <- subset(AdvancedProcessed, !grepl("season", AdvancedProcessed$Season))
AdvancedProcessed <- subset(AdvancedProcessed, !grepl("Season", AdvancedProcessed$Season))

# Remove NA columns as well as irrelevant columns of various statistics
AdvancedProcessed <- subset(AdvancedProcessed, select = -c(3,4,9:29))

# Removes entries from any player who did not compete in the 19/20 season
AdvancedProcessed <- AdvancedProcessed %>%  group_by(Name) %>% filter(any(Season == "2019-20")) %>% ungroup()

# Rename season entries to be a single four-digit number, set it as a numeric
AdvancedProcessed$Season <- substr(AdvancedProcessed$Season, 1, 4)
AdvancedProcessed$Season <- as.numeric(AdvancedProcessed$Season)

# Remove entries after 2020 as they will not be used in the analysis
AdvancedProcessed <- AdvancedProcessed[!(AdvancedProcessed$Season >= 2020),]

# Calculate player experience as the range of a given players' seasons
AdvancedProcessed <- AdvancedProcessed %>% group_by(Name) %>% mutate(Exp = (Season - min(Season))) %>% ungroup()

# Minutes Played and Games Played are colinear. A new variable which tracks average number of minutes per game
# 'MPperG' was calcuated as Minutes Played divided by Games played.
AdvancedProcessed$MPperG <- AdvancedProcessed$MP/AdvancedProcessed$G

# Dummy code the 19/20 season seperately from non-covid 19 seasons
# Seasons are denoted by the year the season started, hence '2019' = 19/20
# 19/20 playoff data are entirely games played in the NBA bubble, so only games marked as 2019 and 'Playoff'
# will be summy coded as COVID statistics.
AdvancedProcessed$COVID <- ifelse(AdvancedProcessed$Season == 2019 & AdvancedProcessed$LeaguePlayoff == "Playoff", 1, 0)
# 1 = COVID, 0 = Non-COVID

# Convert relevant columns to numeric or factors
AdvancedProcessed$Age <- as.numeric(AdvancedProcessed$Age)
AdvancedProcessed$PER <- as.numeric(AdvancedProcessed$PER)
AdvancedProcessed$G <- as.numeric(AdvancedProcessed$G)
AdvancedProcessed$MP <- as.numeric(AdvancedProcessed$MP)

AdvancedProcessed$Pos <- as.factor(AdvancedProcessed$Pos)

# Omit any entries that have NA values in the PER column
na.omit(AdvancedProcessed$PER)

# Remove players' season with too little minutes played or too few games played
AdvancedProcessed <- subset(AdvancedProcessed, !AdvancedProcessed$MP < 25)
AdvancedProcessed <- subset(AdvancedProcessed, !AdvancedProcessed$G < 5)

# Filter out any duplicated entries by grouping by player and removing entries with duplicate MP
AdvancedProcessed <- AdvancedProcessed %>% group_by(Name) %>% filter(!duplicated(MP)) %>% ungroup()

# Due to an error with some data on the basketball-reference site, some duplicate observations are attributed to
# multiple players. Using the 'duplicated' function, all observation which are exact duplicated except for
# the name of the player are dropped.
AdvancedProcessed <- AdvancedProcessed[!duplicated(AdvancedProcessed[-8]),]

# Collapse the position factor of players who have played multiple positions to a condensed classification
AdvancedProcessed$Pos <- fct_collapse(AdvancedProcessed$Pos, "Cen./Pow. Forward" = c("C", "PF"), "Guards" = c("SG", "PG"), "Small Forward" = c("SF"))
AdvancedProcessed <- AdvancedProcessed %>% group_by(Name) %>%  filter (Pos == 'Cen./Pow. Forward' |Pos == 'Guards' |Pos == 'Small Forward') %>% ungroup()

# Convert COVID column to factor
AdvancedProcessed$COVID <- as.factor(AdvancedProcessed$COVID)
AdvancedProcessed$Name <- as.factor(AdvancedProcessed$Name)
AdvancedProcessed$LeaguePlayoff <- as.factor(AdvancedProcessed$LeaguePlayoff)

# Filter out any players who upon filtering have no entries for the 19/20 playoff season
AdvancedProcessed <- AdvancedProcessed %>% group_by(Name) %>% filter(any(COVID == 1)) %>% ungroup()

# Save the 'AdvancedProcessed' data to the data subfolder
here::here()
save(AdvancedProcessed, file = file.path("data","AdvancedProcessed.rda"))
 
######## Data Analysis ########

# Set options to display decimal rather than scientific notation
options(scipen=999)

# Descriptive Statistics of each variable.

# Overall descriptive statistics
AdvancedProcessed %>% summarize(min = min(Age),
                                q1 = quantile(Age, 0.25),
                                median = median(Age),
                                mean = mean(Age),
                                sd = sd(Age),
                                q3 = quantile(Age, 0.75),
                                max = max(Age),
                                IQR = IQR(Age)) %>% as.data.frame()

AdvancedProcessed %>% summarize(min = min(MP),
                                q1 = quantile(MP, 0.25),
                                median = median(MP),
                                mean = mean(MP),
                                sd = sd(MP),
                                q3 = quantile(MP, 0.75),
                                max = max(MP),
                                IQR = IQR(MP)) %>% as.data.frame()


AdvancedProcessed %>% summarize(min = min(PER),
                                q1 = quantile(PER, 0.25),
                                median = median(PER),
                                mean = mean(PER),
                                sd = sd(PER),
                                q3 = quantile(PER, 0.75),
                                max = max(PER),
                                IQR = IQR(PER)) %>% as.data.frame()


AdvancedProcessed %>% summarize(min = min(Exp),
                                q1 = quantile(Exp, 0.25),
                                median = median(Exp),
                                mean = mean(Exp),
                                sd = sd(Exp),
                                q3 = quantile(Exp, 0.75),
                                max = max(Exp),
                                IQR = IQR(Exp)) %>% as.data.frame()


AdvancedProcessed %>% summarize(min = min(MPperG),
                                q1 = quantile(MPperG, 0.25),
                                median = median(MPperG),
                                mean = mean(MPperG),
                                sd = sd(MPperG),
                                q3 = quantile(MPperG, 0.75),
                                max = max(MPperG),
                                IQR = IQR(MPperG)) %>% as.data.frame()


# Descriptive statistics grouped by COVID

AdvancedProcessed %>% group_by(COVID) %>%  summarize(min = min(Age),
                                                     q1 = quantile(Age, 0.25),
                                                     median = median(Age),
                                                     mean = mean(Age),
                                                     sd = sd(Age),
                                                     q3 = quantile(Age, 0.75),
                                                     max = max(Age),
                                                     IQR = IQR(Age)) %>% as.data.frame()


AdvancedProcessed %>% group_by(COVID) %>% summarize(min = min(MP),
                                                     q1 = quantile(MP, 0.25),
                                                     median = median(MP),
                                                     mean = mean(MP),
                                                     sd = sd(MP),
                                                     q3 = quantile(MP, 0.75),
                                                     max = max(MP),
                                                    IQR = IQR(MP)) %>% as.data.frame()


AdvancedProcessed %>% group_by(COVID) %>%  summarize(min = min(PER),
                                                     q1 = quantile(PER, 0.25),
                                                     median = median(PER),
                                                     mean = mean(PER),
                                                     sd = sd(PER),
                                                     q3 = quantile(PER, 0.75),
                                                     max = max(PER),
                                                     IQR = IQR(PER)) %>% as.data.frame()


AdvancedProcessed %>% group_by(COVID) %>%   summarize(min = min(Exp),
                                                     q1 = quantile(Exp, 0.25),
                                                     median = median(Exp),
                                                     mean = mean(Exp),
                                                     sd = sd(Exp),
                                                     q3 = quantile(Exp, 0.75),
                                                     max = max(Exp),
                                                     IQR = IQR(Exp)) %>% as.data.frame()


AdvancedProcessed %>% group_by(COVID) %>%  summarize(min = min(MPperG),
                                                     q1 = quantile(MPperG, 0.25),
                                                     median = median(MPperG),
                                                     mean = mean(MPperG),
                                                     sd = sd(MPperG),
                                                     q3 = quantile(MPperG, 0.75),
                                                     max = max(MPperG),
                                                     IQR = IQR(MPperG)) %>% as.data.frame()

# Histogram to explore distributiion of PER
ggplot(AdvancedProcessed, aes(x = PER)) +
  geom_histogram(bins = 50) +
  theme_classic() +
  labs(x = "PER", y = "Observations (N)") +
  scale_y_continuous(expand = c(0,0), limits = c(0,131))+
  scale_x_continuous(expand = c(0,0))
here::here()
ggsave(filename = "AdvancedProcessedPERHistogram.png", plot = last_plot(),
       device = png,
       path = file.path("figures"), width = 4, height = 4)


# Density plot to explore distribution of PER  
ggplot(AdvancedProcessed, aes(x = PER)) +
  geom_density(linewidth = 0.8) +
  theme_classic() +
  labs(x = "PER", y = "Density") +
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))
here::here()
ggsave(filename = "AdvancedProcessedPERDensityPlot.png", plot = last_plot(),
       device = png,
       path = file.path("figures"), width = 4, height = 4)


# Mixed Regression

# Null model
lmer1 <- lmer(PER ~ 1 + (1|Name), data = AdvancedProcessed)
summary(lmer1)

# Reduced model to compare to null model
lmer2 <- lmer(PER ~ COVID + (1|Name), data = AdvancedProcessed)
summary(lmer2)

# Compare the two using 'anova'
anova(lmer1, lmer2)

# Full Model with summary, confidence intervals, intraclass correlation and variance inflation factor
lmer3 <- lmer(PER ~ COVID + Exp + Pos + MP + MPperG + (1|Name), data = AdvancedProcessed)
summary(lmer3)
confint(lmer3, oldNames=FALSE)
icc(lmer3)
vif(lmer3)

# Full model excluding 'COVID' fixed factor
lmer4 <- lmer(PER ~ Exp + Pos + MP + MPperG + (1|Name), data = AdvancedProcessed)
summary(lmer4)

# Compare the two models using LRT and anova
lrtest(lmer3, lmer4)
anova(lmer4, lmer3)

# Diagnostic plots for mixed model
# Check for linearity
ggplot(data = AdvancedProcessed, aes(y = residuals(lmer3), x = fitted(lmer3))) +
  geom_point()+
  geom_smooth(colour = "red")+
  theme_classic()+
  labs(title = "Fitted vs. Residual Values", x = "Fitted Values", y = "Model Residuals") +
  geom_hline(yintercept = 0, linewidth = 1, colour = "dark blue", linetype = "dashed") 
ggsave(filename = "MixedModelFittedVsResiduals.png", plot = last_plot(),
       device = png,
       path = file.path("figures"), width = 4, height = 4)

# Check for normally distributed residuals
ggplot(data = AdvancedProcessed, aes(sample = residuals(lmer3))) +
  geom_qq() +
  geom_qq_line()+
  theme_classic()+
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
ggsave(filename = "MixedModelQQplot.png", plot = last_plot(),
       device = png,
       path = file.path("figures"), width = 4, height = 4)

# Model Visualisation
# Sample 9 players for the purposes of visualisation.
# This WILL change each time.
AdvancedProcessedSample9 <- AdvancedProcessed %>%
  filter(Name %in% sample(Name,9))

# Create a set of box-plots from the sample of players.
ggplot(AdvancedProcessedSample9,aes(x=COVID,y=PER)) +
  geom_boxplot(alpha=0.2, size=0.6) + 
  facet_wrap(~Name) +
  theme_classic() +
  theme(legend.position = "none") +
  stat_summary(fun=mean, geom="line", aes(group=1), size = 0.7) +
  geom_jitter(alpha = 0.6, position = "jitter")
ggsave(filename = "MixedModelSampleBoxPlots.png", plot = last_plot(),
       device = png,
       path = file.path("figures"), width = 4, height = 4)


# Plot the effect of the effect of COVID on PER from the mixed model
plot(effect('COVID', lmer3), main = "")

# Plot the random effect of Player using a boxplot
dotplot(ranef(lmer3), condVar=TRUE)
        
# Compare PER for League and Playoff games PER statistics
AdvancedProcessed %>% group_by(LeaguePlayoff) %>%  summarize(min = min(PER),
                                                     q1 = quantile(PER, 0.25),
                                                     median = median(PER),
                                                     mean = mean(PER),
                                                     sd = sd(PER),
                                                     q3 = quantile(PER, 0.75),
                                                     max = max(PER),
                                                     IQR = IQR(PER)) %>% as.data.frame()

# Mixed Models for Playoff-only data

# Filter out league data
AdvancedProcessedPlayoff <- AdvancedProcessed %>%  filter(AdvancedProcessed$LeaguePlayoff == "Playoff")

# Save the 'AdvancedProcessedPlayoff' data to the data subfolder
here::here()
save(AdvancedProcessedPlayoff, file = file.path("data","AdvancedProcessedPlayoff.rda"))


# Fit the full model using playoff-only data including summary, confidence intervals,
# ICC and VIF
lmer5 <- lmer(PER ~ COVID + Exp + Pos + MP + MPperG + (1|Name), data = AdvancedProcessedPlayoff)
summary(lmer5)
confint(lmer5, oldNames=FALSE)
icc(lmer5)
vif(lmer5)

