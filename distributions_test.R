library(fitdistrplus)


game1<-score_sim %>% 
  filter(game == 1)
descdist(game1$value, boot = 1000)

game2<-score_sim %>% 
  filter(game == 2)
descdist(game2$value, boot = 1000)

game3<-score_sim %>% 
  filter(game == 3)
descdist(game3$value, boot = 1000)

game4<-score_sim %>% 
  filter(game == 4)
descdist(game4$value, boot = 1000)

game5<-score_sim %>% 
  filter(game == 5)
descdist(game5$value, boot = 1000)

game6<-score_sim %>% 
  filter(game == 6)
descdist(game6$value, boot = 1000)

game7<-score_sim %>% 
  filter(game == 7)
descdist(game7$value, boot = 1000)

match <- c("Port Adelaide v Western Bulldog","Richmond v Brisbane", "Geelong v North Melbourne", "Adelaide v Melbourne","Collingwood v Sydney", "Gold Coast v St Kilda", "Essendon v GWS")
skew <- c(-0.03869118, 0.05635402, -0.005389775, 0.04229985, -0.02887582, -0.02560078, -0.04924172) 
kurt <- c(2.996879, 3.059271, 3.046733, 2.963345, 3.041496, 3.122269, 3.08137)
sim_dist<-data.frame(match, skew, kurt)

formattable(sim_dist, align = c("l", rep("c", NCOL(table) - 1)))

descdist(fit_dist1$Margin, boot = 1000)
descdist(fit_dist2$Margin, boot = 1000)
descdist(fit_dist3$Margin, boot = 1000)
descdist(fit_dist4$Margin, boot = 1000)
descdist(fit_dist5$Margin, boot = 1000)
descdist(fit_dist6$Margin, boot = 1000)
descdist(fit_dist7$Margin, boot = 1000)
descdist(fit_dist8$Margin, boot = 1000)
descdist(fit_dist9$Margin, boot = 1000)
descdist(fit_dist10$Margin, boot = 1000)

prediction_category <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
skew <- c(-0.2677452, 0.1234518, -0.00913922,-0.04564814, 0.008667671, 0.0623132, 0.01953887, -0.09471042,-0.1092447, 0.1621393)
kurt <- c(3.212489, 3.067672, 2.805335, 3.07853, 2.884964,2.989746, 3.09254, 2.869305, 2.943778,3.268719)
category_dist <- data.frame(prediction_category,skew, kurt)

formattable(category_dist, align = c("l", rep("c", NCOL(table) - 1)))
