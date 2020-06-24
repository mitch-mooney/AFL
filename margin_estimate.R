# run simulation to determine margin based on category
runs = 1000
resampling<-matrix(0,runs,10)

for(i in 1:nrow(Sum_pred_cat)){
  mean = as.numeric(Sum_pred_cat[i,2])
  sd = as.numeric(Sum_pred_cat[i,3])
  sim<- rnorm(runs, mean = mean, sd = sd)
  # Create a summary vector
  resampling[,i] <- sim
}

# for a value in pred_cat select and average 100 random rows in resample
score_data_lean$margin_est_rand<-NA
prediction = as.data.frame(as.numeric(score_data_lean$pred_cat_factor))
for (j in 1:nrow(score_data_lean)){
 num<-as.numeric(score_data_lean[j,24])
 sample<- sample(resampling[,num], 100, replace=FALSE)
 mean_margin <- round(mean(sample),0)
 score_data_lean[j,25]<- mean_margin
}
     
score_data_lean<-score_data_lean %>% 
  mutate(margin_est_linear = round(-63.5+129*pred_win_prob,0))

margin_data <- score_data_lean %>%
  select(Margin,margin_est_linear, margin_est_rand, pred_win_prob, line_Odds, Odds, status, last_encounter_SC, pred_cat, pred_cat_factor)

margin_data %>%
  filter(Margin != 999) %>% 
ggplot(aes(x = margin_est_rand, y = Margin, color = pred_cat)) +
  geom_point()+
  geom_smooth(method = "lm", se=TRUE, color="blue", formula = my.formula) +
  labs(color="Win Probability") +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

margin_data %>%
  filter(Margin != 999) %>% 
  ggplot(aes(x = margin_est_linear, y = Margin, color = pred_cat)) +
  geom_point()+
  geom_smooth(method = "lm", se=TRUE, color="blue", formula = my.formula)+
  labs(color="Win Probability") +
  ggtitle("Predicted margin vs actual margin by wining prediction probability")+
  xlab("predicted margin")+
  ylab("actual margin")
  #stat_poly_eq(formula = my.formula, 
  #             aes(label = paste(..rr.label.., sep = "~~~")),label.y.npc = "bottom", 
  #             parse = TRUE)  
