library(formattable)
#bind guess with fixture
prob_pred_df <- prob_pred_df%>%
  rename(Loss_prob = V1,
         Win_Prob = V2,
         #Draw_Prob = V3,
         Tips = V3)

season_predictions <-read.csv('fixture_res.csv')
new_predictions<-cbind(round, prob_pred_df)
table<-new_predictions %>%
  mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition)) %>% 
  select(Date, Season, Team, Opposition, Status, Venue, Round, Loss_prob, Win_Prob, Team_predicted)
table <- table[1:9,]
#display tips in table with probabilities
formattable(table, align = c("l", rep("r", NCOL(table) - 1)))

library(plyr)
#bind new with previous predictions
new_season_pred<-rbind.fill(season_predictions, new_predictions)
detach("package:plyr", unload = TRUE)
#rewrite csv with up to date predictions to keep tally
write.csv(new_season_pred,'fixture_res.csv')

