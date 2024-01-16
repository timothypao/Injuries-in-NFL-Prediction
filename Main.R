#-----------------------Finding 1: Position Matters-----------------------
library(tidyverse)
library(moments)
library(arules)
library(ggplot2)
library(readxl)
library(dplyr)
install.packages("ggplot2")

Concussion_data <- read_csv("Concussion Injuries 2012-2014.csv")
colnames(Concussion_data)

NFL_Combine_Data <- read_excel("NFL Combine Data.xlsx")
colnames(NFL_Combine_Data)

colnames(NFL_Combine_Data) <- c('Year', 'Name', 'College', "Position' , 'Height','Weight','Hand Size', 'Arm Length', 'Wonderlic', '40 Yard', 'Bench Press', 'Vertical Leap,ID', 'Player', 'Team', "ID', 'Player', 'Team''ID', 'Player', 'Team', "ID', 'Player', 'Team')
#View(NFL_Combine_Data)

SportsVizSunday <- read_excel("Feb 2020 SportsVizSunday.xlsx")
#View(SportsVizSunday)

# Load necessary libraries
library(dplyr)

concussion_rate <- Concussion_data %>%
  group_by(Position) %>%
  summarize(ConcussionCount = n()) %>%
  arrange(desc(ConcussionCount))

print(concussion_rate)
barplot(concussion_rate$ConcussionCount)

ggplot(concussion_rate, aes(x = reorder(Position, +ConcussionCount), y = ConcussionCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # To make the bar plot horizontal
  labs(title = "Concussion Counts by Position in Football", 
       x = "Position", y = "Concussion Count") +
  theme_minimal()

offensive_positions <- c("QB", "WR", "RB", "TE", "OL")
defensive_positions <- c("LB", "CB", "S", "DL")

# Add Category Column
concussion_rate <- concussion_rate %>% 
  mutate(Category = case_when(
    Position %in% offensive_positions ~ "Offense",
    Position %in% defensive_positions ~ "Defense",
    TRUE ~ "Special Teams" # assuming any other position is special teams or undefined
  ))

# Update your offensive and defensive positions lists with full names
offensive_positions <- c("Quarterback", "Wide Receiver", "Running Back", "Tight End", "Offensive Lineman", "Center", "Offensive Tackle", "Guard")
defensive_positions <- c("Linebacker", "Comerback", "Safety", "Defensive Lineman", "Defensive Tackle", "Defensive End")

# Recategorize positions in concussion_rate
concussion_rate <- concussion_rate %>% 
  mutate(Category = case_when(
    Position %in% offensive_positions ~ "Offense",
    Position %in% defensive_positions ~ "Defense",
    TRUE ~ "Special Teams"
  ))

# Now recreate the bar plot
ggplot(concussion_rate, aes(x = reorder(Position, +ConcussionCount), y = ConcussionCount, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Concussion Counts by Position in Football (Categorized by Offense, Defense, Special Teams)", 
       x = "Position", y = "Concussion Count") +
  scale_fill_manual(values = c("Offense" = 'red', "Defense" = "blue", "Special Teams" = "green")) +
  theme_minimal()

team_concussion_rate <- df %>%
  group_by(Team) %>%
  summarize(TeamCount = n()) %>%
  arrange(desc(TeamCount))

print(team_concussion_rate)





#-----------------------Finding 2: Playtime Decreases Post-Injury-----------------------
library(tidyverse)
concussion <- read_csv("Concussion Injuries 2012-2014.csv")

#HISTOGRAM OF PLAY TIME B4 VS AFTER INJURY
concussion$`Average Playtime Before Injury` <- gsub(" downs","",concussion$`Average Playtime Before Injury`)
concussion$`Play Time After Injury` <- gsub(" downs","",concussion$`Play Time After Injury`)
concussion$`Average Playtime Before Injury`<- as.numeric(concussion$`Average Playtime Before Injury`)
concussion$`Play Time After Injury` <- as.numeric(concussion$`Play Time After Injury`)
hist(concussion$`Average Playtime Before Injury`,
     main="Average Playtime Before Injury",
     xlab='Playtime (Downs)',
     col="darkslategray1")
hist(concussion$`Play Time After Injury`,
     main="Average Playtime After Injury",
     xlab='Playtime (Downs)',
     col="deeppink1")





#-----------------------Finding 3: Most Likely to Get Injured Halfway Through Season-----------------------
#BOXPLOT OF DISTRIBUTION OF WEEK OF INJURY
boxplot(concussion$`Week of Injury`,
        main = "Distribution of Week of Injury",
        xlab = "Week of Season",
        col = "mediumspringgreen",
        border = "seagreen4",
        horizontal = TRUE)





#-----------------------Finding 4: Team Success is Directly Correlated With Injury-----------------------
library(tidyverse)
library(dplyr)
concussion <- read.csv("Concussion Injuries 2012-2014.csv")

names(concussion) <- gsub("\\.", "", names(concussion))
concussion$AveragePlaytimeBeforeInjury <- as.numeric(gsub(" downs", "", concussion$AveragePlaytimeBeforeInjury))
concussion$PlayTimeAfterInjury <- as.numeric(gsub(" downs", "", concussion$PlayTimeAfterInjury))
concussion <- mutate(concussion, WinningTeam_numeriuc = ifelse(WinningTeam == "Yes",1,0))

concussion$Position <- as.factor(concussion$Position)
concussion$PreSeasonInjury <- as.factor(concussion$PreSeasonInjury)
concussion$WinningTeam <- as.factor(concussion$WinningTeam)
concussion$ReportedInjuryType <- as.factor(concussion$ReportedInjuryType)

concussion <- concussion %>%
  mutate(GamesMissed = ifelse(is.na(GamesMissed), mean(GamesMissed, na.rm = TRUE), GamesMissed),
         PlayTimeAfterInjury = ifelse(is.na(PlayTimeAfterInjury), mean(PlayTimeAfterInjury, na.rm = TRUE), PlayTimeAfterInjury),
         AveragePlaytimeBeforeInjury = ifelse(is.na(AveragePlaytimeBeforeInjury), mean(AveragePlaytimeBeforeInjury, na.rm = TRUE), AveragePlaytimeBeforeInjury))

RNGversion("3.5.2")
set.seed(123)
train_sample <- sample(nrow(concussion), 300)

train <- concussion[train_sample, ]
test <- concussion[-train_sample, ]

prop.table(table(train$WinningTeam))
prop.table(table(test$WinningTeam))

train$Position <- as.factor(train$WinningTeam)
test$Position <- as.factor(test$WinningTeam)

library(C50)

model <- C5.0(train[-12], train$WinningTeam, train$WinningTeam,
              control = C5.0Control(minCases = 30))
summary(model)