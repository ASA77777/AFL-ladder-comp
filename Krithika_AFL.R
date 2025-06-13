#Loading libraries ----
library(dplyr)
library(ggplot2)
library(tidyverse)
library(elo)
library(rsample)
library(caret)
library(fitzRoy) # For AFL data
library(parsnip)
library(tidymodels)
library(rpart.plot)
library(vip)
library(ranger)
library(sjPlot)


#Fetch data using FitzRoy package functions----
#See FitzRoy documentation 
afl_data <- fitzRoy::fetch_player_stats_afltables(season = 2000:2024)
colnames(afl_data)

afl_data <- afl_data %>%
  group_by(Home.team) %>%
  select(Round, Kicks, Marks, Handballs, Disposals, Goals, Tackles, Home.team, Away.team, Home.score, Away.score) %>%
  mutate(Margin = Home.score - Away.score) %>%
  mutate(Result = ifelse(Margin > 0, 1, ifelse(Margin < 0, 0, 0.5)))

str(afl_data)
colSums(is.na(afl_data))

afl_test <- fetch_results_afltables(season = 2000:2024)
colSums(is.na(afl_test))
afl_test$Result <- ifelse(afl_test$Margin > 0, 1, ifelse(afl_test$Margin < 0, 0, 0.5))



# Data Manipulation s
str(afl_test)




#Basic Elo (from here) ---- 
afl_test$Result <- ifelse(afl_test$Margin > 0, 1, ifelse(afl_test$Margin < 0, 0, 0.5))

elo_basic <- elo::elo.run(formula = Result ~ Home.Team + Away.Team,
                          data = afl_test,
                          initial.elos = 1500,
                          k = 20,
                          history = T) %>%
  as.data.frame()

# Write a function
elo_score <- function(initial_elos, k, data){
  
  # obtain elo ratings
  elo <- elo::elo.run(formula = Result ~ Home.Team + Away.Team,
                      initial_elos = initial_elos,
                      k = k,
                      data = data) %>%
    as.data.frame()
  
  data <- data %>% 
    mutate(p.A = elo$p.A) %>% 
    mutate(pred = ifelse(p.A > .5, 1, 0))
  
  cm <- caret::confusionMatrix(data = factor(data$pred, levels = c(0,0.5,1)),
                               reference = factor(data$Result, levels = c(0, 0.5,1)))
  
  return(list(cm))
  
}

#Round.Number, Home.Goals, Home.Behinds, Home.Points, Home.Team, Venue, Season

# Function to restructure data so each game has two entries one for home and one for away
restructure_afl_data <- function(afl_test) {
  afl_home <- data.frame(
    Date = afl_test$Date,
    Season = afl_test$Season,
    Round = afl_test$Round.Number,
    Venue = afl_test$Venue,
    Goals = afl_test$Home.Goals,
    Team = afl_test$Home.Team,
    Opponent = afl_test$Away.Team,
    Result = ifelse(afl_test$Home.Points > afl_test$Away.Points, "W",
                    ifelse(afl_test$Home.Points < afl_test$Away.Points, "L", "D")),
    Points_For = afl_test$Home.Points,
    Points_Against = afl_test$Away.Points,
    Spread = afl_test$Home.Points - afl_test$Away.Points,
    Home = TRUE,
    Game_ID = afl_test$Game
  )
  
  afl_away <- data.frame(
    Date = afl_test$Date,
    Season = afl_test$Season,
    Round = afl_test$Round.Number,
    Venue = afl_test$Venue,
    Goals = afl_test$Away.Goals,
    Team = afl_test$Away.Team,
    Opponent = afl_test$Home.Team,
    Result = ifelse(afl_test$Away.Points > afl_test$Home.Points, "W",
                    ifelse(afl_test$Away.Points < afl_test$Home.Points, "L", "D")),
    Points_For = afl_test$Away.Points,
    Points_Against = afl_test$Home.Points,
    Spread = afl_test$Away.Points - afl_test$Home.Points,
    Home = FALSE,
    Game_ID = afl_test$Game
  )
  
  bind_rows(afl_home, afl_away)
}

afl_test <- restructure_afl_data(afl_test)

#Incase I want to test with D later
afl_test <- afl_test %>% filter(Result != "D")
# So we can see we have added ELO, boolean values for if the row is related to the home or away team and a boolean result column in the form of W and L
# Let us make everything numeric and get rid of NA for ELO

afl_test <- afl_test %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)

# Creating a teams data frame with ELO -- We will have to deal with team mergers at some point here because of the extensive data but it shouldn't matter too much
# Extract unique teams from the combined dataset
teams_elo <- unique(c(afl_test$Team))
teams_elo
# Create a dataframe with teams and assign everyone an ELO of 1500 as a base ELO
# We use 1500 as it is the middle of typical ELO ranges and assumes all teams start equally skilled - When teams join, GCS, GWS we may give them a lower ELO to reflect under performance
teams_elo <- data.frame(
  Team = teams_elo,
  ELO = 1500,
  stringsAsFactors = FALSE
)

# Sort by team name alphabetically
teams_elo <- teams_elo[order(teams_elo$Team), ]



# The creation of ELO
library(elo)
# Creating the ELO logic
# My two datasets I have used here are results and teams_elo, results has all the data and teams_elo is the teams and their elo
for(i in 1:nrow(afl_test)){
  if(i %% 2 != 0){ 
    print(i)
    
    Team_A <- afl_test$Team[i]
    Team_B <- afl_test$Team[i+1]
    
    Result_A <- afl_test$Result[i]
    Result_B <- afl_test$Result[i+1]
    
    ## Get Current ELO ##
    
    ELO_A <- as.numeric(teams_elo[teams_elo$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teams_elo[teams_elo$Team == Team_B, "ELO"])
    
    ## Load current ELO into the main data set 
    
    afl_test$ELO[i] <- ELO_A
    afl_test$Opp_ELO[i] <- ELO_B
    
    afl_test$ELO[i+1] <- ELO_B
    afl_test$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELOs
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 12 * (Result_A - E_A) # Our K value here is 20, the K value controls how much ELO moves after each game, higher more dramatically and lower less. 
    Elo_Updated_B <- ELO_B + 12 * (Result_B - E_B)
    
    ## Update Team ELOs
    
    teams_elo[teams_elo$Team == Team_A, "ELO"] <- Elo_Updated_A
    teams_elo[teams_elo$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}


# Add ELO Difference to the data set
afl_test$Elo_diff <- round(afl_test$ELO - afl_test$Opp_ELO, 2)


#data manipulation
#Recent form - last 3 - 5 games (no of wins/avg wins) and margin 
#Rest time/ (travel time) - time since last match - afl might be alirght as there is a match played every weekend, cant be two in a week
#Both elos into consideration, instead of just the difference
#Big player absences
#Weather
#Player avg stats - aggregate of avg of goals, tackles, disposals for each team
#squad change not sure if this is accountable)
#ladder position before the match (accounts for pressure in later rounds as well?)

#How can the model predict upsets?
str(afl_test)


#Adding two additional columns for last 3 wins and avg margin of the last 3 games
afl_test <- afl_test %>%
  group_by(Team) %>%
  arrange(Date) %>%
  mutate(
    Last3Wins = lag(Result, 1) + lag(Result, 2) + lag(Result, 3),
    AvgLast3Margin = (lag(Spread, 1) + lag(Spread, 2) + lag(Spread, 3))/3
  )


#Al the first few gams played have NA's as there werent any matches played before. Just substituting it with zeros for now
afl_test$Last3Wins[is.na(afl_test$Last3Wins)] <- 0
afl_test$AvgLast3Margin[is.na(afl_test$AvgLast3Margin)] <- 0

#Variables to discard
#last3wins is probably redundant and not that significant
#Round - has a slight effect on the later rounds but doesnt make too much of an impact

# GLM ---- 
# We use logistic regression as it is a classification model, there are two outcomes, win or lose therefore it will output a probability between 0-1. 
split <- initial_split(afl_test)
train <- training(split)
test <- testing(split)
model1 <- glm(Result ~ Home + Elo_diff + Round + Last3Wins + AvgLast3Margin, data = afl_test, family = binomial(link = "logit"))
tab_model(model1)
summary(model1)
#Making predictions on this model
probabilities <- model1 %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > .5, 1, 0)

#Accuracy
accuracy <- mean(predicted.classes == test$Result)
print(paste("Accuracy:", round(accuracy, 4)))


library(pROC)

#AUC
roc_obj <- roc(test$Result, probabilities)
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 4)))

#ROC curve
plot(roc_obj, col = "blue", main = "ROC Curve")

#Random Forest
#Splitting the data ----
rf_data <- afl_test
rf_data <- afl_test %>%
  select(Home, Elo_diff, Round, Last3Wins, AvgLast3Margin, Result)
str(rf_data)
rf_data <- rf_data %>%
  select(Home, Elo_diff, ELO, Opp_ELO,Date, Team, Opponent, Result)
rf_data$Home <- factor(rf_data$Home)
rf_data$Elo_diff <- factor(rf_data$Elo_diff)
rf_data$Result <- factor(rf_data$Result)
rf_data$ELO <- factor(rf_data$ELO)
rf_data$Opp_ELO <- factor(rf_data$Opp_ELO)
rf_data$Team <- factor(rf_data$Team)
rf_data$Opponent <- factor(rf_data$Opponent)


rf_data$Result <- factor(rf_data$Result)

data_split <- initial_split(rf_data)
data_train <- training(data_split)
data_test <- testing(data_split)

#Building the spec ----
rf_spec <- rand_forest(trees = 500, mtry = 3) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

#Model fit ----
rf_fit <- rf_spec %>%
  fit(Result ~ Home + Elo_diff + Round + Last3Wins + AvgLast3Margin,
      data = data_train)

#Predictions on the training set ----
data_train$.pred <- predict(rf_fit, data_train)

# Training (accuracy) ----
results_acc_train <- rf_fit %>% 
  augment(new_data = data_train) %>% 
  accuracy(Result, .pred_class)

results_acc_train
#94% for now

#Predictions on the testing set -----
data_test$.pred <- predict(rf_fit, data_test)

results_acc_test <- rf_fit %>% 
  augment(new_data = data_test) %>% 
  accuracy(Result, .pred_class)

results_acc_test
#53.8% for now

#VIP ----
rf_fit %>%
  vip()

#Additional
var_importance <- importance(rf_fit$fit, type = "impurity")

# Plot variable importance with custom labels
barplot(var_importance, names.arg = colnames(data_train)[1:(ncol(data_train)-2)],
        main = "Variable Importance - Impurity",
        xlab = "Variable Names")



#XGB ----
xgb_data <- afl_test
str(xgb_data)
xgb_data <- xgb_data %>%
  select(Home, Elo_diff, ELO, Opp_ELO,Date, Team, Opponent, Result)
xgb_data$Home <- factor(xgb_data$Home)
xgb_data$Elo_diff <- factor(xgb_data$Elo_diff)
xgb_data$Result <- factor(xgb_data$Result)
xgb_data$ELO <- factor(xgb_data$ELO)
xgb_data$Opp_ELO <- factor(xgb_data$Opp_ELO)
xgb_data$Team <- factor(xgb_data$Team)
xgb_data$Opponent <- factor(xgb_data$Opponent)

data_split <- initial_split(xgb_data)
xgb_train <- training(data_split)
xgb_test <- testing(data_split)

#Building the spec ----
xgb_spec <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

#Fitting the model ----
xgb_fit <- xgb_spec %>% 
  fit(Result ~ Home + Elo_diff, data = xgb_train)
class(xgb_fit)

# Make predictions on the training set ----
xgb_train$.pred <- predict(xgb_fit, xgb_train)

#Creating a confusion matrix ----
# Train data
xgb_fit %>% 
  augment(new_data = xgb_train) %>% 
  conf_mat(truth = Result, estimate = .pred_class)

# Test data 
xgb_fit %>% 
  augment(new_data = xgb_test) %>% 
  conf_mat(truth = Result, estimate = .pred_class)

#Testing accuracy ----
# Train data
xgb_fit %>% 
  augment(new_data = xgb_train) %>%
  accuracy(truth = Result, estimate = .pred_class)
#57.8 (Isnt this quite low for a training dataset)
# Test data
xgb_fit %>% 
  augment(new_data = xgb_test) %>%
  accuracy(truth = Result, estimate = .pred_class)
#57.7
#VIP ----
xgb_fit %>%
  vip()

#Additional
var_importance <- xgb.importance(model = xgb_fit$fit)




## Predict and filter for 2025




# Ladder creation - my dataset is called results_2025 
# ACTUAL ladder
ladder_actual <- results_2025 %>%
  group_by(Team) %>%
  summarise(
    Games = n(),
    Wins_Actual = sum(Result == 1),
    Losses_Actual = sum(Result == 0),
    Draws_Actual = sum(Result == 0.5),
    WinPct_Actual = round(mean(Result) * 100, 1),
    Points_Actual = Wins_Actual * 4 + Draws_Actual * 2,
    Points_For = sum(Points_For),
    Points_Against = sum(Points_Against),
    Percentage_Actual = round((Points_For / Points_Against) * 100, 1)
  )

# PREDICTED ladder
ladder_predicted <- results_2025 %>%
  group_by(Team) %>%
  summarise(
    Wins_Pred = sum(GLM_Forecast == 1),
    Losses_Pred = sum(GLM_Forecast == 0),
    Draws_Pred = sum(Win_Prob_Pred > 0.45 & Win_Prob_Pred < 0.55),  # optional
    WinPct_Pred = round(mean(GLM_Forecast) * 100, 1),
    Points_Pred = Wins_Pred * 4 + Draws_Pred * 2,
    Percentage_Pred = round((sum(Points_For) / sum(Points_Against)) * 100, 1)
  )

# Merge both ladders
ladder_comparison <- ladder_actual %>%
  left_join(ladder_predicted, by = "Team") %>%
  arrange(desc(Points_Actual))

ladder_comparison <- ladder_comparison %>%
  mutate(Point_Diff = Points_Pred - Points_Actual,
         Rank_Actual = rank(-Points_Actual),
         Rank_Pred = rank(-Points_Pred),
         Rank_Diff = abs(Rank_Pred - Rank_Actual))

mean(ladder_comparison$Rank_Diff)
ladder_comparison












