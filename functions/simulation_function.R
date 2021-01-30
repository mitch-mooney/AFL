simulation <- function(runs){

  # run simulation to determine margin based on category
  runs = runs
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
  next_col <- which(colnames(score_data_lean)=="margin_est_rand")
  for (j in 1:nrow(score_data_lean)){
    num<-as.numeric(score_data_lean[j,24])
    sample<- sample(resampling[,num], 10000, replace=FALSE)
    mean_margin <- round(mean(sample),0)
    score_data_lean[j,26]<- mean_margin
  }
  
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
  sim_list <- list(score_sim = score_sim, score_data_lean = score_data_lean)
  
  return(sim_list)
}
