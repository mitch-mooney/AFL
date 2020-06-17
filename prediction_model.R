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


### --- Testing Model --- ###
 
#configure model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(col_num-1)) %>% 
  layer_dropout(rate = 0.8) %>% 
  layer_dense(units = 128, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.3) %>% 
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
          #optimizer = optimizer_rmsprop(lr = 0.002),
          metrics = 'accuracy')

history <- model %>% 
  fit(training,
      trainLabels,
      epochs = 1500,
      batch_size = 256,
      validation_split = 0.2)

plot(history)

#evalute model from test dataset
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
# plot findings separating into probability categories
plotly_build(prob_pred_df%>%
  filter(V2 < 0.40)%>%
  ggplot(aes(x = pred_result, fill = pred_result))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses when probabilities below 0.4')+
  ylab("Percent predictions")+
  xlab('Prediction result'))

plotly_build(prob_pred_df%>%
  filter(V2 > 0.40 & V2 <0.6)%>%
ggplot(aes(x = pred_result, fill = pred_result))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses when probabilities between 0.4 & 0.6')+
  ylab("Percent predictions")+
  xlab('Prediction result'))

plotly_build(prob_pred_df%>%
  filter(V2 > .60)%>%
  ggplot(aes(x = pred_result, fill = pred_result))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses when probabilities above 0.6')+
  ylab("Percent predictions")+
  xlab('Prediction result'))

#predict future events targets
pred_new <- model %>% 
  predict_classes(future_matrix)
#predict target probabilities
prob_future<-model %>% 
  predict_proba(future_matrix)
prob_pred_future<-cbind(round(prob_future[1:18,1:test_dim], 4),
                 pred_new[1:18])
prob_pred_df <- as.data.frame(prob_pred_future)
