#AFL
# run simulation to determine margin based on category
runs = 100000
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
 sample<- sample(resampling[,num], 10000, replace=FALSE)
 mean_margin <- round(mean(sample),0)
 score_data_lean[j,25]<- mean_margin
}

# estimate margin using linear model     
score_data_lean<-score_data_lean %>% 
  mutate(margin_est_linear = round(-63.5+129*pred_win_prob,0))
#create simulation of margin estimate
margin_sim <- score_data_lean %>% 
  filter(Margin == 999) %>% 
  select(pred_loss_prob, pred_win_prob, margin_est_linear, pred_cat_factor)

margin_sim<- cbind(round, margin_sim)
games = nrow(round)
#fill matrix with columns representing the simulated game margins
  # Look at including home or away match into the loop.
score_sim <- matrix(0, nrow = 10000, ncol = games)
for (k in 1:nrow(margin_sim)){
  num<-as.numeric(margin_sim[k,18])
  sample<- sample(resampling[,num], 10000, replace=FALSE)
  score_sim[,k] <- sample
}
score_sim <- as.data.frame(score_sim)
score_sim<-reshape2::melt(score_sim)
#get data frame of match data to merge with simulation
names <- data.frame(team=margin_sim$Team, opp=margin_sim$Opposition, game=1:games, round = margin_sim$Round)
names<-as.data.frame(lapply(names, rep, 10000))
names <- arrange(names, game)
#merge names with score simulation
score_sim <- cbind(score_sim, names)
score_sim$match <- paste(score_sim$team, score_sim$opp, sep = " v ")

mean_score = score_sim %>%
  group_by(game, team, opp) %>% 
  summarise(rating.mean=mean(value), rating.sd = sd(value), rating.median = median(value))

mean_score = mean_score %>% 
  mutate(result = ifelse(rating.mean < 0, opp, team)) %>% 
  mutate(match = paste(team, opp, sep = " v "))

#create custom colors for teams
cols <- (c("Gold Coast" = 'gold',"GWS" = 'orange',"Collingwood" = 'black', "North Melbourne" = 'mediumblue',"Sydney"= "firebrick1", "Fremantle" ="purple","Port Adelaide" = "lightseagreen", "Adelaide" = "gold",
           "West Coast" = "blue", "Melbourne" = "darkblue", "Western Bulldogs" = "ivory1", "Richmond" = "yellow","Carlton" = "navy","Hawthorn" = "chocolate4", "St Kilda" = "grey", "Essendon" =  "red3",
           "Brisbane" = "maroon", "Geelong" = "dodgerblue"))

# create plot for simulation
plt<-score_sim %>% 
  mutate(result = ifelse(value < 0, opp, team)) %>% 
  filter(game < (games/2 +1)) %>% #change this to suit how many matches there are that round
  ggplot(aes(x = value, color = result))+
  geom_histogram(binwidth = 1,  alpha = 0.8)+
  geom_vline(data=mean_score[1:(games/2),], aes(xintercept=rating.mean,  colour=result), #change mean_score[1:games/2]
             linetype="dashed", size=1)+
  scale_colour_manual(values = cols)+
  labs(title = "Match simulation of AFL",
       subtitle = score_sim$round,
       color = "Team",
       x = "simulated margin")+
  scale_x_continuous(breaks = seq(-100, 100, 20))+
  theme(plot.title = element_text(hjust = 0.5), # Centered title
        plot.background = element_rect(fill="black"), # Black background
        panel.background = element_rect(fill="gray20"), # Dark grey panel background
        panel.grid.minor = element_line(color="black"), # Hide grid lines
        panel.grid.major = element_line(color="black"), # Hide grid lines
        axis.text = element_text(color="white"), # Make axis text white
        title = element_text(color="white", face="bold"), # Make title white and bold.
        legend.background = element_rect(fill="black"), # Make legend background black
        legend.text = element_text(color="white"), # Make legend text white
        legend.key = element_rect(fill="black", color="black"), #Squares/borders of legend black
        legend.position = c(.9,.4)) + # Coordinates. Top right = (1,1) # Dark grey panel back+
  facet_grid(game+match~.,labeller = label_wrap_gen(width = 0.5, multi_line = TRUE))

#summary table to simulation results
tapply(score_sim$value, score_sim$match, summary)
# mean and 95% confidence of simulation
#computation of the standard error of the mean
score_sim %>% 
  group_by(match) %>% 
  summarise(mean = mean(value), sd = sd(value), cv = sd/mean, sem = sd(value)/sqrt(length(value)), lower_CI = mean(value)-2*sem, upper_CI = mean(value)+2*sem)


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

score_data_lean_error<-score_data_lean %>%
  filter(Margin != 999) %>% 
  mutate(margin_linear_error = Margin - margin_est_linear) %>% 
  mutate(margin_rand_error = Margin - margin_est_rand)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

rmse(score_data_lean_error$margin_linear_error)
mae(score_data_lean_error$margin_linear_error)

rmse(score_data_lean_error$margin_rand_error)
mae(score_data_lean_error$margin_rand_error) 

