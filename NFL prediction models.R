#install.packages("tidyverse")
#install.packages("nflfastR")
#install.packages("ggimage")
#install.packages("gt")
#install.packages("gtExtras")
#install.packages("vip")
#install.packages("ranger")
#install.packages("ggthemes")
#install.packages("DiagrammeR")

# Load the packages we installed
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(vip)
library(ggthemes)
library(ranger)
library(caret)
library(xgboost)
library(randomForest)
library(xgboost)
options(scipen = 9999)


pbp <- nflfastR::load_pbp(2002:2021) %>% 
  filter(rush == 1 | pass == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "") %>% 
  select(game_id, epa, posteam, defteam, home_team, away_team, roof, temp, wind, success, season, pass, fumble_lost, interception)

#set temps (70 target) and wind conditions for indoor games

pbp$temp[is.na(pbp$temp)] <- 70
pbp$wind[is.na(pbp$wind)] <- 0

# Getting Offensive and Defensive EPA 

offense <- pbp %>% 
  group_by(game_id, posteam, pass, temp, wind, roof, home_team, away_team, season) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>% 
  rename(off_pass_epa = '1', off_rush_epa = '0')

defense <- pbp %>% 
  group_by(game_id, defteam, pass, temp, wind, roof, home_team, away_team, season) %>% 
  summarize(epa = mean(epa)) %>% 
  pivot_wider(names_from = pass, values_from = epa) %>% 
  rename(def_pass_epa = '1', def_rush_epa = '0')

#Create fumbles df
fumbles <- pbp %>% 
  group_by(game_id, posteam) %>% 
  summarize(fumble_lost = sum(fumble_lost)) %>% 
  pivot_wider(names_from = fumble_lost, values_from = fumble_lost) %>% 
  rename(lost_fumble = '1', no_fumble = '0')

def_fumbles <- pbp %>% 
  group_by(game_id, defteam) %>% 
  summarize(fumble_lost = sum(fumble_lost)) %>% 
  pivot_wider(names_from = fumble_lost, values_from = fumble_lost) %>% 
  rename(fumble_won = '1', no_fumble = '0')

#Create interception df
interception <- pbp %>% 
  group_by(game_id, posteam) %>% 
  summarize(interception = sum(interception)) %>% 
  pivot_wider(names_from = interception, values_from = interception) %>% 
  rename(interception = '1', no_interception = '0')

def_interception <- pbp %>% 
  group_by(game_id, defteam) %>% 
  summarize(interception = sum(interception)) %>% 
  pivot_wider(names_from = interception, values_from = interception) %>% 
  rename(interception_made = '1', no_interception = '0')

# Team wins each season

games <- nflreadr::load_schedules(2002:2021)
str(games)

# Get team wins from a home team perspective
home <- games %>% 
  filter(game_type == 'REG') %>% 
  select(game_id, season, week, home_team, result) %>% 
  rename(team = home_team)
home %>% head(5)

# Get team wins from a road team perspective

away <- games %>% 
  filter(game_type == 'REG') %>% 
  select(game_id, season, week, away_team, result) %>% 
  rename(team = away_team) %>% 
  mutate(result = -result)
away %>% head(5)

# Bind rows 

results <- bind_rows(home, away) %>% 
  arrange(week) %>% 
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )

# Get team wins by game
team_wins_game <- results %>% 
  group_by(game_id, team, season) %>% 
  summarize(
    wins = sum(win),
    point_diff = sum(result)) %>% 
  ungroup()

team_wins_game %>% 
  arrange(-wins) %>% 
  head(5)


#Fix Raiders, Rams, Chargers who have moved cities
team_wins_game <- team_wins_game %>% 
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )


#team wins by season
team_wins_season <- results %>% 
  group_by(team, season) %>% 
  summarize(
    wins = sum(win),
    point_diff = sum(result)) %>% 
  ungroup()



#Fix Raiders, Rams, Chargers who have moved cities
team_wins_season <- team_wins_season %>% 
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )


# joinning data sets
data <- team_wins_game %>% 
  left_join(offense, by = c('team' = 'posteam', 'game_id')) %>% 
  left_join(defense, by = c('team' = 'defteam', 'game_id'))


# Remove duplicate columns using subset
data <- subset(data, select = -c(temp.y, wind.y, home_team.y, away_team.y, roof.y, season.y, season))

#clean up data some more
data <- data %>% 
  rename('season' = 'season.x', 'temp' = 'temp.x', 'wind' = 'wind.x', 'roof' = 'roof.x', 'home_team' = 'home_team.x', 'away_team' = 'away_team.x' )

# Join offensive fumbles and interceptions to Data
data <- data %>% 
  left_join(fumbles, by = c('game_id', 'team' = 'posteam')) %>% 
  left_join(interception, by = c('game_id', 'team' = 'posteam'))

#Remove unneeded columns using subset
data <- subset(data, select = -c(no_fumble, NA.x, no_interception, NA.y))

# Rename columns. 
data <- data %>% 
  rename('fumble_1' = 'lost_fumble', 'fumble_2' = '2.x', 'fumble_3' = '3.x', 'fumble_4' = '4.x', 'fumble_5' = '5.x', 'interception_2' = '2.y', 'interception_3' = '3.y', 'interception_4' = '4.y', 'interception_5' = '5.y', 'interception_6' = '6')


#change NA values to 0
data <- data %>% 
  mutate_at(c('fumble_1', 'fumble_2', 'fumble_3', 'fumble_4', 'fumble_5', 'interception', 'interception_2', 'interception_3', 'interception_4', 'interception_5', 'interception_6'), ~replace_na(.,0))

#combine offensive fumbles and interceptions into total fumbles and total interceptions

data$total_fumbles_lost <- rowSums(data[startsWith(names(data), "fumble")])

data$total_interceptions_thrown <- rowSums(data[startsWith(names(data), "interception")])

#total the offensive turnovers
data$off_turnovers <- rowSums(data[startsWith(names(data), "total")])

#convert offensive turnovers to negative numbers
data$off_turnovers = data$off_turnovers*(-1)

# Rename defensive turnover columns. 

def_fumbles <- def_fumbles %>% 
  rename('fumble_won_1' = 'fumble_won', 'fumble_won_2' = '2', 'fumble_won_3' = '3', 'fumble_won_4' = '4', 'fumble_won_5' = '5')

def_interception <- def_interception %>% 
  rename('interception_made_1' = 'interception_made', 'interception_made_2' = '2', 'interception_made_3' = '3', 'interception_made_4' = '4', 'interception_made_5' = '5', 'interception_made_6' = '6')

# Join defensive fumbles and interceptions to Data

data <- data %>% 
  left_join(def_fumbles, by = c('game_id', 'team' = 'defteam'))

data <- data %>% 
  left_join(def_interception, by = c('game_id', 'team' = 'defteam'))

#Remove unneeded columns using subset
data <- subset(data, select = -c(no_fumble, NA.x, no_interception, NA.y))

#change defensive turnover NA values to 0
data <- data %>% 
  mutate_at(c('fumble_won_1', 'fumble_won_2', 'fumble_won_3', 'fumble_won_4', 'fumble_won_5', 'interception_made_1', 'interception_made_2', 'interception_made_3', 'interception_made_4', 'interception_made_5', 'interception_made_6'), ~replace_na(.,0))


#combining defensive turnovers (MAKE SURE COLUMN NUMBERS ARE CORRECT BEFORE PROCEEDING)
data$total_fumbles_won <- rowSums(data[,29:33])

data$total_interceptions_made <- rowSums(data[,34:39])

#total the defensive turnovers
data$def_turnovers <- rowSums(data[,40:41])

#calculate the turnover +/-

data$turnover_diff <- data$off_turnovers + data$def_turnovers

# coding of home and away
data <- data %>% 
  mutate(home = ifelse(home_team == team, 1, 0))

#coding of roof
data <- data %>% 
  mutate(dome = ifelse(roof %in% c('closed', 'dome'), 1, 0))

#removing columns
data_final <- subset(data, select = -c(home_team, away_team, roof, fumble_1, fumble_2, fumble_3, fumble_4, fumble_5, interception, interception_2, interception_3, interception_4, interception_5, interception_6, total_fumbles_lost, total_interceptions_thrown, fumble_won_1, fumble_won_2, fumble_won_3, fumble_won_4, fumble_won_5, interception_made_1, interception_made_2, interception_made_3, interception_made_4, interception_made_5, interception_made_6, total_fumbles_won, total_interceptions_made))
names(data_final)
#rearrange columns
data_final <- data_final %>% 
  select(game_id, season, team, point_diff, wins, turnover_diff, off_pass_epa, off_rush_epa, def_pass_epa, def_rush_epa, home, dome, temp, wind)

write.csv(data_final, "nfldataproject.csv", row.names = FALSE)

# Training and Testing Models

set.seed(42)

data_final_train <- sample(1:nrow(data_final), as.integer(nrow(data_final) * .8))
data_final_test <- setdiff(1:nrow(data_final), data_final_train)

data_final_train_factor <- data[data_final_train, ]
data_final_test_factor <- data[data_final_test, ]

#Logistic Regression Model
log_wins <- glm(wins ~ home + turnover_diff + off_pass_epa + off_rush_epa + def_pass_epa + def_rush_epa, data = data_final)

log_wins
summary(log_wins)
vip(log_wins)

#checking for covariance
log_wins_co <- glm(wins ~ (home + turnover_diff + off_pass_epa + off_rush_epa + def_pass_epa + def_rush_epa)^2,
                   data = data_final)
summary(log_wins_co)

log_wins$fitted.values

#testing example
data_final %>% 
  mutate(pred_prob = log_wins$fitted.values) %>% 
  ggplot(aes(x = team)) +
  geom_smooth(aes(y = pred_prob), color = "black", size = 2) +
  geom_point(aes(y = wins, color = ifelse(wins == 1, "darkgreen", "darkred")),
             alpha = 0.8) +
  scale_color_identity() +
  theme_minimal() +
  labs(x = "Team",
       y = "Chance team will win (0-1)")

#combine results into data frame
data_final1 <- data_final %>% 
  mutate(pred_prob = log_wins$fitted.values) %>% 
  mutate(wins_oe = wins - pred_prob)

#create data frame with predictions
wins_preds_glm <- data_final1 %>% 
  select(game_id, season, team, point_diff, wins, pred_prob, home, turnover_diff, off_pass_epa, off_rush_epa, def_pass_epa, def_rush_epa)

#write data frame to csv
write.csv(wins_preds_glm, "glmgamepred.csv", row.names = FALSE)


#Random Forest Model

#random forest without training and test sets
wins_rf <- randomForest(as.factor(wins) ~ home + turnover_diff + dome + temp + wind + off_pass_epa + off_rush_epa + def_pass_epa + def_rush_epa, data = data_final, family = binomial)

#check the random forest
wins_rf

#check the features
vip(wins_rf)

#get predictions
wins_preds <- predict(wins_rf, type="prob")

#combine predictions to data_final
wins_preds_rf <- cbind(data_final, wins_preds)

#rename prediction probs
wins_preds_rf <- wins_preds_rf %>% 
  rename('pred_loss' = '0', 'pred_tie' = '0.5', 'pred_win' = '1')

#re-organizing columns
wins_preds_rf <- wins_preds_rf %>%
  select(game_id, season, team, wins, pred_win, pred_tie, pred_loss, home, point_diff, turnover_diff, dome, temp, wind, off_pass_epa, off_rush_epa, def_pass_epa, def_rush_epa)

#write prediction to csv
write.csv(wins_preds_rf, "randomforestgamepred.csv", row.names = FALSE)


#attempting to build RF with training and test sets
#wins_rf2 <- randomForest(as.factor(wins) ~ home + turnover_diff + dome + temp + wind + off_pass_epa + off_rush_epa + def_pass_epa + def_rush_epa, data = data_final, subset = data_final_train, family = binomial)

#pred_rf_wins <- predict(wins_rf2, data_final[data_final_test,])
#pred_rf_wins

#df <- data.frame(label = data_final[data_final_test, 'wins'],
                 #rf = pred_rf_wins)

#ROC_func(df, 1, 2)

#Xgboost Model

data_final_matrix <- subset(data_final, select = -c(game_id, season, team, point_diff))

smp_size <- floor(0.80 * nrow(data_final_matrix))
set.seed(2011) #go lions
ind <- sample(seq_len(nrow(data_final_matrix)), size = smp_size)
train <- as.matrix(data_final_matrix[ind, ])
test <- as.matrix(data_final_matrix[-ind, ])

dim(train)
colnames(train)

#build model
wins_xg <-
  xgboost(
    data = train[, 2:10],
    label = train[, 1],
    nrounds = 1000,
    objective = "binary:logistic",
    early_stopping_rounds = 3,
    max_depth = 12,
    eta = .25
  )   

#plot tree
xgb.plot.tree(model = wins_xg, trees = 5)

#get predictions
pred_xgb <- predict(wins_xg, test[, 2:10], type = "prob")

head(pred_xgb, 1)

#see RMSE
yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

#Create prediction data
wins_preds_xg <- as.data.frame(
  matrix(predict(wins_xg, as.matrix(data_final_matrix %>% select(-wins))))
) %>% 
  dplyr::rename(pred_win = V1)

#combine with data_final to see predictions
xg_prob <- cbind(data_final, wins_preds_xg)

#rearrange columns
xg_prob <- xg_prob %>% 
  select(game_id, season, team, point_diff, wins, pred_win, home, turnover_diff, dome, temp, wind, off_pass_epa, off_rush_epa, def_pass_epa, def_rush_epa)

choose_cutoff <- function(preds, acts, start = .5) {
  counter <- start
  best_acc <- 0
  best_cutoff <- start
  while (counter >= 0) {
    test_preds <- ifelse(preds > counter, 1, 0)
    test_acc <- sum(test_preds == acts) / length(acts)
    print(paste(test_acc, counter))
    if (test_acc > best_acc) {
      best_acc <- test_acc
      best_cutoff <- counter
    }
    counter <- counter - .01
  }
  return(best_cutoff)
}

choose_cutoff(yhat, y)

#write xg boost pred to csv
write.csv(xg_prob, "xgboostgamepred.csv", row.names = FALSE)
