library(keras)

data <- future_data_lean
col_num<-as.numeric(ncol(data))
data[1:col_num] <- lapply(data[1:col_num], as.numeric) #make sure all variables are numeric

# Check the data matrix
#table(future_data$results)
#barplot(prop.table(table(data$results)),
#        col = rainbow(3),
#        ylim = c(0, 1),
#        ylab = 'proportion',
#        xlab = 'score events',
#        cex.names = 1.5)

#include all future data with previous data
data <- as.matrix(data)
dimnames(data) <- NULL        
data[,2:col_num] <- normalize(data[,2:col_num])
# identify where the new data is 999 is always the largest number of the matrix
first <- which(data>=sort(data, decreasing = T)[1], arr.ind = T)
firstRow <- head(first[,1], n=1)
lastRow <- tail(first[,1], n=1)
# separate future data
future_matrix <- data[firstRow:lastRow,2:col_num] #have to adjust the row for new data
full_future_matrix<-data[firstRow:lastRow,1:col_num]
#remove future data from training set
data<- data[-firstRow:-lastRow,] #have to adjust the row for new data
# organize training set
set.seed(321)
ind<-sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
training <- data[ind==1, 2:col_num]
test <- data[ind==2 , 2:col_num]
trainingtarget <- data[ind==1, 1]
testtarget <- data[ind==2, 1]
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
test_var<-as.numeric(nrow(testLabels))
test_dim<-as.numeric(ncol(testLabels))

### --- Winning Model --- ### 71.3% accuracy on test data on 1500 EPOCHS
#configure model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(col_num-1)) %>% 
  layer_dropout(rate = 0.8) %>% 
  layer_dense(units = 128, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 32, activation = 'relu') %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 2, activation = 'softmax')
summary(model)
#compile model choose an optimizer
model %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_adam(lr=0.002),
          #optimizer = optimizer_sgd(lr = 0.002),
          #optimizer = optimizer_rmsprop(lr = 0.003),
          metrics = 'accuracy')

history <- model %>% 
  fit(training,
      trainLabels,
      epochs = 1500,
      batch_size = 256,
      validation_split = 0.3)

#plot(history)

#evaluate model from test dataset
model %>% 
  evaluate(test, testLabels)
# look at the model prediction probabilities
prob<- model %>% 
  predict_proba(test)
#predict test data targets
pred <- model %>% 
  predict_classes(test)
#create a confusion matrix using absolute values
table(Predicted = pred, Actual = testtarget)

#bind guesses with probabilities and actual target values identify correct and incorrect guesses
prob_pred<-cbind(round(prob[1:test_var,1:test_dim], 3),
      pred[1:test_var],
      testtarget[1:test_var])
prob_pred_df <- as.data.frame(prob_pred)
prob_pred_df <- prob_pred_df %>%
  mutate(pred_result = ifelse(V3 == V4, "correct", "incorrect"))


# See how the degree of predicition works
#plotly_build(prob_pred_df%>%
#  filter(V2 < .40)%>%
#  ggplot(aes(x = pred_result, fill = pred_result))+
#  geom_bar(aes(y = (..count..)/sum(..count..)))+
#  ggtitle('proportion of correct guesses when probabilities above 0.6')+
#  ylab("Percent predictions")+
#  xlab('Prediction result'))

#plotly_build(prob_pred_df%>%
#  filter(V2 > 0.40 & V2 <0.6)%>%
#ggplot(aes(x = pred_result, fill = pred_result))+
#  geom_bar(aes(y = (..count..)/sum(..count..)))+
#  ggtitle('proportion of correct guesses when probabilities between 0.4 & 0.6')+
#  ylab("Percent predictions")+
#  xlab('Prediction result'))
#
#plotly_build(prob_pred_df%>%
#  filter(V2 > .60)%>%
#  ggplot(aes(x = pred_result, fill = pred_result))+
#  geom_bar(aes(y = (..count..)/sum(..count..)))+
#  ggtitle('proportion of correct guesses when probabilities above 0.6')+
#  ylab("Percent predictions")+
#  xlab('Prediction result'))

#predict future events targets
pred_new <- model %>% 
  predict_classes(future_matrix)
#predict target probabilities
prob_future<-model %>% 
  predict_proba(future_matrix)
future_rows<-as.numeric(nrow(prob_future))
prob_pred_future<-cbind(round(prob_future[1:future_rows,1:test_dim], 3),
                 pred_new[1:future_rows])
prob_pred_df <- as.data.frame(prob_pred_future)

#put down predictions for all matches to add to score_margin dataframe
data_mat<-rbind(data, full_future_matrix)
x<-data_mat[,2:col_num]
all_match_pred<-model %>% predict_proba(x)

score_data_lean<-cbind(score_data_lean, all_match_pred)
names(score_data_lean)[21] <- "pred_loss_prob"
names(score_data_lean)[22] <- "pred_win_prob"

#plot correlation between win probability and Margin
my.formula <- y ~ x
score_data_lean%>%
  filter(Margin != 999) %>% 
  filter(pred_win_prob <0.5)%>%
  ggplot(aes(x = pred_win_prob, y = Margin, color = team))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE, color="blue", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)   

score_data_lean<-score_data_lean %>%
  mutate(pred_cat = ifelse(pred_win_prob < 0.1, 1, 
                           ifelse(pred_win_prob > 0.1 & pred_win_prob < 0.2, 2,
                                  ifelse(pred_win_prob > 0.2 &pred_win_prob < 0.3, 3,
                                         ifelse(pred_win_prob > 0.3 & pred_win_prob < 0.4, 4,
                                                ifelse(pred_win_prob > 0.4 & pred_win_prob < 0.5, 5,
                                                       ifelse(pred_win_prob > 0.5 & pred_win_prob < 0.6, 6,
                                                              ifelse(pred_win_prob > 0.6 & pred_win_prob< 0.7, 7,
                                                                     ifelse(pred_win_prob > 0.7 & pred_win_prob < 0.8, 8,
                                                                            ifelse(pred_win_prob > 0.8 & pred_win_prob < 0.9, 9, 10))))))))))
#make numerical value categories
score_data_lean$pred_cat_factor <- as.factor(score_data_lean$pred_cat)

# New facet label names for supp variable
score_data_lean$pred_cat <- factor(score_data_lean$pred_cat_factor, levels = c(1,2,3,4,5,6,7,8,9,10), 
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))

# Find the mean of each group removing the unknown margins
Sum_pred_cat<-score_data_lean %>%
  group_by(pred_cat) %>% 
  filter(Margin != 999) %>% 
  summarise(rating.mean=mean(Margin), rating.sd = sd(Margin))

# Interleaved histograms
score_data_lean %>%
  filter(Margin != 999) %>% 
ggplot(aes(x=Margin, color=pred_cat)) +
  geom_histogram(aes(y = ..density..),fill="white", position="dodge")+
  geom_density(alpha = 0.3)+
  geom_vline(data=Sum_pred_cat, aes(xintercept=rating.mean,  colour=pred_cat),
             linetype="dashed", size=1)+
  labs(title = "Histogram of margins within prediction probability categories",
       subtitle = "AFL",
       color="Win Probability") +
  theme(legend.position="top")+
  facet_grid(pred_cat ~.)

# plot to see if home or away predictions are more effective
plotly_build(score_data_lean%>%
               filter(status == 2)%>%
               mutate(pred_result = ifelse(pred_win_prob > 0.5, 1, 0)) %>% 
               mutate(result = ifelse(Margin > 0, 1, ifelse(Margin < 0, 0, 0.5))) %>% 
               mutate(outcome = ifelse(pred_result == result, "correct", "incorrect")) %>% 
               ggplot(aes(x = outcome, fill = pred_result))+
               geom_bar(aes(y = (..count..)/sum(..count..)))+
               ggtitle('proportion of correct guesses when probabilities below 0.4')+
               ylab("Percent predictions")+
               xlab('Prediction result'))


