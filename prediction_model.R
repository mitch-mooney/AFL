library(keras)

data <- future_data

table(future_data$results)
barplot(prop.table(table(data$results)),
        col = rainbow(3),
        ylim = c(0, 1),
        ylab = 'proportion',
        xlab = 'score events',
        cex.names = 1.5)

#include all future data with previous data
data <- as.matrix(data)
dimnames(data) <- NULL        
data[,2:18] <- normalize(data[,2:18])
# identify where the new data is 999 is always the largest number of the matrix
first <- which(data>=sort(data, decreasing = T)[1], arr.ind = T)
firstRow <- head(first[,1], n=1)
lastRow <- tail(first[,1], n=1)
# seperate future data
future_matrix <- data[firstRow:lastRow,2:18] #have to adjust the row for new data
#remove future data from training set
data<- data[-firstRow:-lastRow,] #have to adjust the row for new data
# organize training set
set.seed(1234)
ind<-sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
training <- data[ind==1, 2:18]
test <- data[ind==2 , 2:18]
trainingtarget <- data[ind==1, 1]
testtarget <- data[ind==2, 1]
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)

#configure model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 64, activation = 'sigmoid', input_shape = c(17)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 128, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.6) %>% 
  layer_dense(units = 32, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 3, activation = 'softmax')
summary(model)

model %>% 
  compile(loss = 'categorical_crossentropy',
          #optimizer = 'adam',
          #optimizer = optimizer_sgd(lr = 0.05, decay = 0.005, momentum = 0.9, nesterov = TRUE),
          optimizer = 'rmsprop',
          metrics = 'accuracy')

history <- model %>% 
  fit(training,
      trainLabels,
      epochs = 2000,
      batch_size = 128,
      validation_split = 0.3)
#class_weight = list("0" = 1, "1" = 0.001, "2" = 1))
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
#predict future events targets
pred_new <- model %>% 
  predict_classes(future_matrix)
#predict target probabilities
model %>% 
  predict_proba(future_matrix)

#bind guesses with probabilities and actual target values identify correct and incorrect guesses
prob_pred<-cbind(1-prob[1:807,1:3],
      pred[1:807],
      testtarget[1:807])
prob_pred_df <- as.data.frame(prob_pred)
prob_pred_df <- prob_pred_df %>%
  mutate(pred_result = ifelse(V4 == V5, "correct", "incorrect"))
# plot findings seperating into probability categories
prob_pred_df%>%
  filter(V2 < 0.40)%>%
  ggplot(aes(x = pred_result, fill = pred_result))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses when probabilities below 0.4')

prob_pred_df%>%
  filter(V2 > 0.40 & V2 <0.6)%>%
ggplot(aes(x = pred_result, fill = pred_result))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses when probabilities between 0.4 & 0.6')

prob_pred_df%>%
  filter(V2 > .60)%>%
  ggplot(aes(x = pred_result, fill = pred_result))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle('proportion of correct guesses when probabilities above 0.6')

