# Data Cleaning
# This script is for processing of the data as well as analysis.

library(rvest)
library(dplyr)
library(janitor)
library(tidyr)
library(forcats)
library(lmerTest)
library(ggplot2)
library(lme4)

RFPerGame <- PerGame
RFAdvanced <- Advanced

RFPerGame <- subset(RFPerGame, !grepl("Did Not Play", RFPerGame$Tm))

RFPerGame$Age <- as.numeric(as.character(RFPerGame$Age))
RFAdvanced$Age <- as.numeric(as.character(RFAdvanced$Age))

RFPerGame <- RFPerGame[!is.na(RFPerGame$Age), ]
RFAdvanced <- RFAdvanced[!is.na(RFAdvanced$Age), ]



RFPerGame <- subset(RFPerGame, !grepl("Career", RFPerGame$Season))
RFPerGame <- subset(RFPerGame, !grepl("season", RFPerGame$Season))
RFPerGame <- subset(RFPerGame, !grepl("season", RFPerGame$Season))


RFAdvanced<- subset(RFAdvanced, !grepl("Career", RFAdvanced$Season))
RFAdvanced<- subset(RFAdvanced, !grepl("season", RFAdvanced$Season))
RFAdvanced<- subset(RFAdvanced, !grepl("Season", RFAdvanced$Season))

RFAdvanced <- subset(RFAdvanced, select = -c(20,25))

#################################################################################

# Removes entries from any player who did not compete in the 19/20 season
SCAdvanced <- RFAdvanced %>%  group_by(Name) %>% filter(any(Season == "2019-20"))
SCPerGame <- RFPerGame %>%  group_by(Name) %>% filter(any(Season == "2019-20"))

# Renames each season to the first year of each season; this is then turned into a numeric column
SCAdvanced$Season <- substr(SCAdvanced$Season, 1, 4)
SCAdvanced$Season <- as.numeric(SCAdvanced$Season)

SCPerGame$Season <- substr(SCPerGame$Season, 1, 4)
SCPerGame$Season <- as.numeric(SCPerGame$Season)

# Removes entries after 2020; these entries are irrelevant to the present analysis
SCAdvanced <- SCAdvanced[!(SCAdvanced$Season > 2020),]
SCPerGame <- SCPerGame[!(SCPerGame$Season > 2020),]

# Calculate player experience as the range of a given players seasons
SCAdvanced <- SCAdvanced %>% group_by(Name) %>% mutate(Exp = (Season - min(Season))) %>% ungroup()
SCPerGame <- SCPerGame %>% group_by(Name) %>% mutate(Exp = (Season - min(Season))) %>% ungroup()

# Code 2019 seasons as during COVID-19
SCAdvanced$COVID <- ifelse(SCAdvanced$Season == 2020, 1, 0)
SCPerGame$COVID <- ifelse(SCPerGame$Season == 2020, 1, 0)
# 1 = COVID, 0 = Non-Covid

# Convert relevant columns to numeric or factors
SCAdvanced$Age <- as.numeric(SCAdvanced$Age)
SCAdvanced$PER <- as.numeric(SCAdvanced$PER)
SCAdvanced$G <- as.numeric(SCAdvanced$G)
SCAdvanced$MP <- as.numeric(SCAdvanced$MP)

SCAdvanced$Pos <- as.factor(SCAdvanced$Pos)
levels(SCAdvanced$Pos)

SCPerGame$Age <- as.numeric(SCPerGame$Age)
SCPerGame$PER <- as.numeric(SCPerGame$PER)
SCPerGame$G <- as.numeric(SCPerGame$G)
SCPerGame$MP <- as.numeric(SCPerGame$MP)

SCPerGame$Pos <- as.factor(SCPerGame$Pos)
levels(SCPerGame$Pos)



# Create a df arranged by season, grouped by player
ArrSCAdvanced <- SCAdvanced %>% group_by(Name) %>% arrange(SCAdvanced, Season, .by_group = TRUE)
ArrSCPerGame <- SCPerGame %>% group_by(Name) %>% arrange(SCPerGame, Season, .by_group = TRUE)


##########################################################################################################################

# Create a df containing only league fixtures by grouping by both player and season, and taking only where minutes are higher
ArrSCAdvancedLea <- ArrSCAdvanced %>% group_by(Name, Season) %>% slice_max(MP)
# Create a playoff df, created as only differing rows from the full df and league df
ArrSCAdvancedPlay <- setdiff(ArrSCAdvanced, ArrSCAdvancedLea)

# 
na.omit(ArrSCAdvanced$PER)
ArrSCAdvanced <- subset(ArrSCAdvanced, !ArrSCAdvanced$MP < 25)
ArrSCAdvanced <- subset(ArrSCAdvanced, !ArrSCAdvanced$G < 5)
ArrSCAdvanced <- ArrSCAdvanced %>% group_by(Name) %>% filter(!duplicated(MP))


##########################################################################################################################

# Collapse the position factor to a more condensed set of positions. Some players play multiple positions.
ArrSCAdvanced$Pos <- fct_collapse(ArrSCAdvanced$Pos, C = c("C", "C,PF", "C,SF"), PF = c("PF", "PF,SF", "PF,SG"), PG = c("PG", "PG,SF,SG", "PG,SG"), SF = c("SF", "SF,SG"), SG = c("SG"))
ArrSCPerGame$Pos <- fct_collapse(ArrSCPerGame$Pos, C = c("C", "C,PF", "C,SF"), PF = c("PF", "PF,SF", "PF,SG"), PG = c("PG", "PG,SF,SG", "PG,SG"), SF = c("SF", "SF,SG"), SG = c("SG"))

# Convert COVID identifier column into factor
ArrSCAdvanced$COVID <- as.factor(ArrSCAdvanced$COVID)
levels(ArrSCAdvanced$COVID)

ArrSCPerGame$COVID <- as.factor(ArrSCPerGame$COVID)
levels(ArrSCPerGame$COVID)

ArrSCAdvancedLea[c("PER", "Age", "MP", "Exp")] <- scale(ArrSCAdvancedLea[c("PER", "Age", "MP", "Exp")], center = TRUE, scale = TRUE)
ArrSCAdvancedPlay[c("PER", "Age", "MP", "Exp")] <- scale(ArrSCAdvancedPlay[c("PER", "Age", "MP", "Exp")], center = TRUE, scale = TRUE)


#################################################################################### Data Analysis

# Create frames for only columns with relevant descriptive statistics
DescStatsAdv <- c("Age", "G", "MP", "PER", "Exp")
DescStatsArrSCAdvanced <- ArrSCAdvanced[DescStatsAdv]
summary(DescStatsArrSCAdvanced)
sapply(DescStatsArrSCAdvanced, sd)

DescStatsPer <- c("")
DescStatsArrSCPerGame <- ArrSCPerGame[DescStatsPer]
summary(DescStatsArrSCPerGame)
sapply(DescStatsArrSCPerGame, sd)



hist(ArrSCAdvanced$PER)

lm1 <- lm(PER ~ COVID + Age + Exp + MP + Pos, data = ArrSCAdvanced)
summary(lm1)

lmer1 <- lmer(PER ~ COVID + Age + Exp + MP + (1|Name), data = ArrSCAdvanced)
summary(lmer1)
print(summary(lmer1), correlation = TRUE)
lmerTest::ranova(lmer1)


plot(lm1)


