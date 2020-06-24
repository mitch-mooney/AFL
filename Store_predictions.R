library(formattable)
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
  select(Tips, pred_win_prob, pred_loss_prob,margin_est_linear, margin_est_rand)

table<-cbind(round, new_predictions)
table$Margin <- NULL

table<- table %>% 
  mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition))
  
table<-table %>%
  select(Date, Season, Team, Opposition, Status, Venue, Round, pred_loss_prob, pred_win_prob, Team_predicted, margin_est_linear, margin_est_rand) %>% 
  rename(
    Loss_prob = pred_loss_prob,
    Win_Prob = pred_win_prob,
    margin_estimate_1 = margin_est_linear,
    margin_estimate_2 = margin_est_rand
  )

library(plyr)
#bind new with previous predictions
new_season_pred<-rbind.fill(season_predictions, table)
detach("package:plyr", unload = TRUE)
#rewrite csv with up to date predictions to keep tally
write.csv(new_season_pred,'fixture_res.csv')

table <- table[1:9,]
#display tips in table with probabilities
formattable(table, align = c("l", rep("r", NCOL(table) - 1)))

