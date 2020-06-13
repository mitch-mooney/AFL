library(formattable)
#bind guess with fixture
prob_pred_df <- prob_pred_df%>%
  rename(Loss_prob = V1,
         Win_Prob = V2,
         Draw_Prob = V3,
         Tips = V4)

season_predictions <-read.csv('fixture_res.csv')
new_predictions<-cbind(round2, prob_pred_df)
#display tips in table with probabilities
formattable(new_predictions, align = c("l", rep("r", NCOL(new_predictions) - 1)))

library(plyr)
#bind new with previous predictions
new_season_pred<-rbind.fill(season_predictions, new_predictions)
#rewrite csv with up to date predictions to keep tally
write.csv(new_season_pred,'fixture_res.csv')

