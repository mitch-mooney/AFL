library(formattable)
library(ggpubr)
#bind guess with fixture
prob_pred_df <- prob_pred_df%>%
  rename(Loss_prob = V1,
         Win_Prob = V2,
         #Draw_Prob = V3,
         Tips = V3)

season_predictions <-read.csv('fixture_res.csv')

new_predictions<-score_data_lean %>% 
  filter(Margin == 999) %>% 
  mutate(Tips = ifelse(pred_win_prob > 0.5, 1, 0)) %>% 
  select(Tips,	pred_loss_prob,	pred_win_prob,margin_est_linear,margin_est_rand)

table<-cbind(round, new_predictions)
table$Margin <- NULL

table<- table %>% 
  mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition))
  
table<-table %>%
  select(Date,	Match_id,	Season,	Team,	Opposition,	Status,	Venue,	Round,	results,	Odds,	line_Odds,	Opp_Odds,	Opp_lineOdds,	Tips,	pred_loss_prob,	pred_win_prob, Team_predicted,	margin_est_linear,	margin_est_rand) %>% 
  rename(
    Loss_prob = pred_loss_prob,
    Win_Prob = pred_win_prob,
    margin_estimate_1 = margin_est_linear,
    margin_estimate_2 = margin_est_rand
  )
formattable(table, align = c("l", rep("c", NCOL(table) - 1)))

#bind new with previous predictions
new_season_pred<-plyr::rbind.fill(season_predictions, table)
#rewrite csv with up to date predictions to keep tally
write.csv(new_season_pred,'fixture_res.csv')

table_final <- table[1:9,]
table_final <- table_final %>% 
  select(Date, Season, Team, Opposition, Venue, Round, Loss_prob, Win_Prob, Team_predicted, margin_estimate_1) %>% 
  mutate(Loss_prob = round(Loss_prob, digits = 2))%>% 
  mutate(Win_Prob = round(Win_Prob, digits = 2))
#display tips in table with probabilities
formattable(table_final, align = c("l", rep("c", NCOL(table_final) - 1)))

# generate table to merge with simulation plot
t <- table_final %>% 
  select(Team, Opposition, Round, Loss_prob, Win_Prob, margin_estimate_1, Team_predicted) %>% 
  rename(Pred_Winner=Team_predicted,Pred_Margin = margin_estimate_1)
tbl<-ggtexttable(t, rows = NULL, theme = ttheme("light", tbody.style = tbody_style(fill ="white", size = 10)))
# Plot chart and table into one object
ggarrange(plt, tbl, nrow = 2, ncol = 1, heights=c(2, 1))

#bits and brier scores following https://rpubs.com/DamienG/613310
season_predictions$predicted_prob = pmax(season_predictions$Loss_prob, season_predictions$Win_Prob)
season_predictions$brier = (season_predictions$predicted_prob - season_predictions$Tip_Outcome)^2
season_predictions$bits = ifelse(season_predictions$Tip_Outcome == 1, 1 + log(season_predictions$predicted_prob, base = 2), 
                                 ifelse(season_predictions$Tip_Outcome == 0, 1 + log(1 - season_predictions$predicted_prob, base = 2),
                                        1 + 0.5*log(season_predictions$predicted_prob*(1-season_predictions$predicted_prob), base = 2)))

#create dataframe comparing home and away prediction scores
accuracy = season_predictions %>% 
  group_by(Status, Round) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>% 
  mutate(margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), Bits = round(mean(bits), 3), MAE = round(mean(margin_error),2),
            Accuracy = round(100*mean(Tip_Outcome), 1), Tips = sum(correct_tips))

formattable(accuracy, align = c("l", rep("c", NCOL(accuracy) - 1)))

model_accuracy = season_predictions %>% 
  group_by(Status) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>% 
  mutate(margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), Bits = round(mean(bits), 3), MAE = round(mean(margin_error),2),
            Accuracy = round(100*mean(Tip_Outcome), 1), Tips = sum(correct_tips), Correct = round(Tips/n(), 2))

formattable(model_accuracy, align = c("l", rep("c", NCOL(accuracy) - 1)))
# see if how well predicting teams
team_accuracy = season_predictions %>% 
  group_by(Status, Team) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>% 
  mutate(margin_error = margin_estimate_1 - Actual.Margin) %>% 
  summarise(Brier = round(mean(brier), 3), Bits = round(mean(bits), 3), MAE = round(mean(margin_error)),
            Accuracy = round(100*mean(Tip_Outcome), 1))
formattable(team_accuracy, align = c("l", rep("c", NCOL(team_accuracy) - 1)))

# Betting return
ROI<-season_predictions %>%
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>%
  mutate(paid = ifelse(Actual == 1, Odds, Opp_Odds)) %>% 
  mutate(return = Tip_Outcome * paid) %>% 
  mutate(ROI = return *10)

ROI %>% 
  filter(predicted_prob > 0.70) %>% 
  filter(Round != "Round 2") %>% 
  group_by(Status, Round) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)

ROI %>% 
  filter(predicted_prob > 0.70) %>% 
  filter(Round != "Round 2") %>% 
  group_by(Status) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)


