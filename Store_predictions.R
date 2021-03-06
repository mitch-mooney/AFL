library(ggpubr)
library(reactable)
library(htmltools)
library(sparkline)

source('functions/reactable_function.R')

#bind guess with fixture
prob_pred_df <- prob_pred_df%>%
  rename(Loss_prob = V1,
         Win_Prob = V2,
         #Draw_Prob = V3,
         Tips = V3)

season_predictions <-read.csv('csv_files/fixture_res.csv')

new_predictions<-score_data_lean %>% 
  filter(Margin == 999) %>% 
  mutate(Tips = ifelse(pred_win_prob > 0.5, 1, 0)) %>% 
  select(Tips,	pred_loss_prob,	pred_win_prob,margin_est_linear,margin_est_rand)

table<-cbind(round, new_predictions)
table$Margin <- NULL

table %<>% 
  mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition))
  
table %<>%
  select(Date,	Match_id,	Season,	Team,	Opposition,	Status,	Venue,	Round,	results,	Odds,	line_Odds,	Opp_Odds,	Opp_lineOdds,	Tips,	pred_loss_prob,	pred_win_prob, Team_predicted,	margin_est_linear,	margin_est_rand) %>% 
  rename(
    Loss_prob = pred_loss_prob,
    Win_Prob = pred_win_prob,
    margin_estimate_1 = margin_est_linear,
    margin_estimate_2 = margin_est_rand
  )

#bind new with previous predictions
new_season_pred<-plyr::rbind.fill(season_predictions, table)
#rewrite csv with up to date predictions to keep tally
write.csv(new_season_pred,'csv_files/fixture_res.csv')

table_final <- table %>% 
  filter(Status == "Home")

table_final %<>% 
  select(Date, Season, Team, Opposition, Venue, Round, Loss_prob, Win_Prob, Team_predicted, margin_estimate_1) %>% 
  mutate(Loss_prob = round(Loss_prob, digits = 2))%>% 
  mutate(Win_Prob = round(Win_Prob, digits = 2))

# generate table to merge with simulation plot
t <- table_final %>% 
  select(Team, Opposition, Round, Loss_prob, Win_Prob, margin_estimate_1, Team_predicted) %>% 
  rename(Pred_Winner=Team_predicted,Pred_Margin = margin_estimate_1)

# use reactable to use team logos

reactable(t, columns = list(
  Team = colDef(maxWidth = 150, align = "center", cell = function(value) {
    img_src <- knitr::image_uri(sprintf("images/%s.png", value))
    image <- img(src = img_src, height = "60px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "80px"), image)
    )
    
  }),
  Opposition = colDef(maxWidth = 150, align = "center", cell = function(value) {
    img_src <- knitr::image_uri(sprintf("images/%s.png", value))
    image <- img(src = img_src, height = "60px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "80px"), image)
    )
  }),
  Pred_Winner = colDef(name = "Predicted Winner", maxWidth = 150, align = "center", cell = function(value) {
    img_src <- knitr::image_uri(sprintf("images/%s.png", value))
    image <- img(src = img_src, height = "60px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "80px"), image)
    )
  }),
  Round = colDef(align = "center", maxWidth = 120),
  Loss_prob = colDef(name = "Loss Probability", align = "center", maxWidth = 120),
  Win_Prob = colDef(name = "Win Probability", align = "center", maxWidth = 120),
  Pred_Margin = colDef(name = "Predicted Margin", align = "center", maxWidth = 120)
))


matches<-results %>% 
  filter(Season >= 2010) %>% 
  group_by(Season, Round.Number) %>% 
  summarise_each(funs(n_distinct(Date))) %>% 
  select(Season, Round.Number, Date)

lag <-tail(matches$Date, 1)
round_num <- tail(matches$Round.Number, 1)

#glicko ratings table
rating_history <- glicko %>% 
  filter(var == "Rating") %>% 
  group_by(match) %>% 
  mutate(rank = rank(-value))%>% 
  group_by(Team) %>% 
  mutate(lag = lag(rank, n =lag)) %>% 
  ungroup()%>% 
  mutate(change = ifelse(rank < lag, "Up", ifelse(rank > lag, "Down", "Unchanged")),
         match = as.numeric(match)) %>% 
  filter(match == max(match)) %>% 
  select(Team, change) %>%
  rename(Player = Team)

team_rate <- glicko_rate$ratings
team_rate %<>% 
  select(!c(Lag, Deviation, Volatility)) %>% 
  mutate(Rating = round(Rating, 0), Rank = rank(-Rating)) %>% 
  select(Rank, Player, Rating)

team_rate<-left_join(rating_history, team_rate, by = c("Player"))

spark_table <- glicko_clean %>%
  group_by(Team) %>%
  summarise(Rating = round(tail(value, n = 1), 0),sparkline = list(tail(value, n = round_num))) %>% 
  rename(Player = Team)

spark_table <- merge(team_rate,spark_table, by=c("Rating", "Player"), all.x=TRUE, all.y=TRUE)

reactable_function(data = spark_table)


#bits and brier scores following https://rpubs.com/DamienG/613310
season_predictions$predicted_prob = pmax(season_predictions$Loss_prob, season_predictions$Win_Prob)
season_predictions$brier = (season_predictions$predicted_prob - season_predictions$Tip_Outcome)^2
season_predictions$bits = ifelse(season_predictions$Tip_Outcome == 1, 1 + log(season_predictions$predicted_prob, base = 2), 
                                 ifelse(season_predictions$Tip_Outcome == 0, 1 + log(1 - season_predictions$predicted_prob, base = 2),
                                        1 + 0.5*log(season_predictions$predicted_prob*(1-season_predictions$predicted_prob), base = 2)))

#create dataframe comparing home and away prediction scores
accuracy <- season_predictions %>% 
  group_by(Status) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), 
            Bits = round(sum(bits), 3), 
            MAE = round(mean(margin_error),2),
            Accuracy = round(100*mean(Tip_Outcome), 1), 
            Tips = sum(correct_tips))

formattable::formattable(accuracy, align = c("l", rep("c", NCOL(accuracy) - 1)))

model_accuracy <- season_predictions %>%
  mutate(Round = sapply(strsplit(Round," "), `[`, 2),
         Round = as.numeric(Round)) %>% 
  group_by(Status, Round) %>%
  filter(Status == "Home") %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), 
            Bits = round(mean(bits), 3), 
            MAE = round(mean(margin_error),2),
            Tips = sum(correct_tips), 
            Matches = n(), 
            `Round Accuracy` = round(Tips/n(), 2)) %>%
  mutate(Cumulative = cumsum(Tips), 
         `Cumulative Matches` = cumsum(Matches), 
         `Cumulative Accuracy` = round((Cumulative/`Cumulative Matches`),2)) %>% 
  select(Round, Matches, `Cumulative Matches`,Bits, MAE, Tips, Cumulative, `Round Accuracy`, `Cumulative Accuracy`)

formattable::formattable(model_accuracy, align = c("l", rep("c", NCOL(accuracy))))
# see if how well predicting teams
team_accuracy <- season_predictions %>% 
  group_by(Status, Team) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         margin_error = margin_estimate_1 - Actual.Margin) %>% 
  summarise(Brier = round(mean(brier), 3), 
            Bits = round(mean(bits), 3), 
            MAE = round(mean(margin_error)),
            Accuracy = round(100*mean(Tip_Outcome), 1))
ormattable::formattable(team_accuracy, align = c("l", rep("c", NCOL(team_accuracy) - 1)))

# Betting return
ROI<-season_predictions %>%
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         paid = ifelse(Actual == 1, Odds, Opp_Odds),
         return = Tip_Outcome * paid,
         ROI = return *10)

ROI %>% 
  filter(predicted_prob > 0.50, Round != "Round 2") %>% 
  group_by(Status, Round) %>% 
  summarise(ROI = sum(ROI), 
            Investment = n()*10)

ROI %>% 
  filter(predicted_prob > 0.50,
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>%
  group_by(Status) %>% 
  summarise(ROI = sum(ROI), 
            Investment = n()*10)

favorites <- season_predictions %>% 
  mutate(favorite = ifelse(Odds < Opp_Odds, "Favorite", "Outside"),
         correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         paid = ifelse(Actual == 1, Odds, Opp_Odds),
         return = Tip_Outcome * paid,
         ROI = return *10)

favorites %<>%
  mutate(pred_cat = ifelse(predicted_prob < 0.1, 1, 
                           ifelse(predicted_prob > 0.1 & predicted_prob < 0.2, 2,
                                  ifelse(predicted_prob > 0.2 &predicted_prob < 0.3, 3,
                                         ifelse(predicted_prob > 0.3 & predicted_prob < 0.4, 4,
                                                ifelse(predicted_prob > 0.4 & predicted_prob < 0.5, 5,
                                                       ifelse(predicted_prob > 0.5 & predicted_prob < 0.6, 6,
                                                              ifelse(predicted_prob > 0.6 & predicted_prob< 0.7, 7,
                                                                     ifelse(predicted_prob > 0.7 & predicted_prob < 0.8, 8,
                                                                            ifelse(predicted_prob > 0.8 & predicted_prob < 0.9, 9, 10))))))))))

favorites %>% 
  filter(predicted_prob > 0.5,
         Status == "Away",
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(favorite, Round) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)%>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100)

favorites %>% 
  filter(predicted_prob > 0.5,
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(favorite, Status) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10) %>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100)

favorites %>% 
  filter(predicted_prob > 0.50, 
         Status == "Away",
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(pred_cat, favorite) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)%>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100) %>% 
  ggplot(aes(x = pred_cat, y = per_return, color = favorite))+
  geom_line()

favorites %>% 
  filter(predicted_prob > 0.50,
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(pred_cat, Status) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)%>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100) %>% 
ggplot(aes(x = pred_cat, y = per_return, color = Status))+
  geom_line()
