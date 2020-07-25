library(dplyr)
library(formattable)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
# Arrange Fixture
rating_pred <- fixture %>% 
  filter(Round == 3) %>% # choose round you want to predict
  select(Date, Home.Team,Away.Team)
#library(tidyverse)
rating_pred$Home.Team<-stringr::str_replace(rating_pred$Home.Team, "Footscray", "Western Bulldogs")
rating_pred$Home.Team<-stringr::str_replace(rating_pred$Home.Team, "Brisbane Lions", "Brisbane")
rating_pred$Away.Team<-stringr::str_replace(rating_pred$Away.Team, "Footscray", "Western Bulldogs")
rating_pred$Away.Team<-stringr::str_replace(rating_pred$Away.Team, "Brisbane Lions", "Brisbane")
#detach("package:tidyverse", unload = TRUE)

# Training and testing split
glicko_train <- ratings[ratings$match <= 1500,]
glicko_test <- ratings[ratings$match > 1500,]
elo_ratings <- elo(glicko_train)
glicko2_train_set<-glicko2(glicko_train, history = T)
pvals <- predict(glicko2_train_set, glicko_test)
glicko_model<-cbind(glicko_test, pvals)
glicko_model <- glicko_model %>% 
  mutate(model_pred = ifelse(pvals >0.5, 1, 0)) %>% 
  mutate(model_res = ifelse(model_pred == results, "correct", "incorrect"))

plotly_build(ggplot(glicko_model, aes(x = model_res, fill = model_res))+
               geom_bar(aes(y = (..count..)/sum(..count..)))+
               ggtitle('proportion of correct guesses from Glicko')+
               ylab("Percent predictions")+
               xlab('Prediction result'))

p<-ggplot(glicko_model, aes(x = model_res, fill = model_res))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses from Glicko2 on testing dataset')+
  ylab("Percent predictions")+
  xlab('Prediction result')


#run future prediction
home_team_pred<-predict(glicko_rate, rating_pred)
glicko_table<-cbind(rating_pred, home_team_pred)
glicko_table <- glicko_table %>% 
  mutate(predicted_winner = ifelse(home_team_pred > 0.5, Home.Team, Away.Team)) %>%
  mutate_at(vars(home_team_pred), funs(round(., 2)))

t<-formattable(glicko_table, align = c("l", rep("r", NCOL(glicko_table) - 1)))

grid.arrange(
  tableGrob(t),
  p,
  #ncol = 1,
  nrow = 2,
 # widths = c(1.5, 1),
  clip = FALSE
)
