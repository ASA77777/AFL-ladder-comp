#Loading libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(elo)
library(rsample)
library(caret)
library(fitzRoy) # For AFL data


# Fetch data using FitzRoy package functions
# See FitzRoy documentation 
afl_data <- fitzRoy::fetch_player_stats_afltables(season = 2000:2024)
afl <- fetch_results_afltables(season = 1897:2024)
colSums(is.na(afl))

afl$Result <- ifelse(afl$Margin > 0, 1, ifelse(afl$Margin < 0, 0, 0.5))

afl <- afl %>%
  select(Round.Number, Home.Team, Away.Team, Result)

elo_basic <- elo::elo.run(formula = Result ~ Home.Team + Away.Team,
                          data = afl,
                          initial.elos = 1500,
                          k = 20,
                          history = T) %>%
  as.data.frame()


head(elo_basic)

#Hyperparatmeter tuning
#Splitting data into training and testing sets
split <- initial_split(afl)
train <- training(split)
test <- testing(split)

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

# Create a grid 
params <- expand.grid(init = seq(1000, 3000, by = 50),
                      kfac = seq(10, 50, by = 5))


# Apply the function 
params$accuracy <- apply(X = params,
                         MARGIN = 1,
                         FUN = function(x)
                           elo_score(x[1], x[2], train)[[1]]$overall["Accuracy"])

#ALL accuracies are the same - some issue here. Will have to check
# Optimal Parameters
best <- subset(params, accuracy == max(params$accuracy))
best$accuracy1 <- NULL
head(best)



elo_final <- elo::elo.run(formula = Result ~ Home.Team + Away.Team,
                          data = afl,
                          initial.elos = 1000,
                          k = 10,
                          history = T) 
elo_final_df <- elo_final %>% 
  as.data.frame()

#Making predictions
test$predictions <- predict(elo_final, test, type = "prob")

# Predicted win or loss based on probability
test$pred.Result <- ifelse(test$predictions > 0.5, 1, ifelse(test$predictions < 0.5, 0, 0.5))

test <- test %>%
  mutate(pred.result1 = ifelse(predictions > 0.5, "Win", ifelse(predictions < 0.5, "Loss", "Draw"))) %>%
  mutate(actual.result = ifelse(Result > 0.5, "Win", ifelse(Result < 0.5, "Loss", "Draw")))

cm <- caret::confusionMatrix(data =  factor(test$actual.result, levels = c("Win", "Loss")),
                             reference = factor(test$pred.result1, levels = c("Win", "Loss")))
# Accuracy ----
cm$overall["Accuracy"]



#GLM
colnames(afl)
model1 <- glm(Result ~ Home.Team + Away.Team, data = afl, family = binomial(link = "logit"))
install.packages("sjPlot")
library(sjPlot)
tab_model(model1)

# Data Manipulation 







# Creating a teams data frame 








# The creation of ELO
library(elo)
# Creating the ELO logic
# My two datasets I have used here are results and teams_elo, results has all the data and teams_elo is the teams and their elo
for(i in 1:nrow(afl)){
  if(i %% 2 != 0){ 
    print(i)
    
    Team_A <- afl$Team[i]
    Team_B <- afl$Team[i+1]
    
    Result_A <- afl$Result[i]
    Result_B <- afl$Result[i+1]
    
    ## Get Current ELO ##
    
    ELO_A <- as.numeric(teams_elo[teams_elo$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teams_elo[teams_elo$Team == Team_B, "ELO"])
    
    ## Load current ELO into the main data set 
    
    results$ELO[i] <- ELO_A
    results$Opp_ELO[i] <- ELO_B
    
    results$ELO[i+1] <- ELO_B
    results$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELOs
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 20 * (Result_A - E_A) # Our K value here is 20, the K value controls how much ELO moves after each game, higher more dramatically and lower less. 
    Elo_Updated_B <- ELO_B + 20 * (Result_B - E_B)
    
    ## Update Team ELOs
    
    teams_elo[teams_elo$Team == Team_A, "ELO"] <- Elo_Updated_A
    teams_elo[teams_elo$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}


# Add ELO Difference to the data set


# Run Logistic Regression Model 
# We use logistic regression as it is a classification model, there are two outcomes, win or lose therefore it will output a probability between 0-1. 






## Test Model





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












