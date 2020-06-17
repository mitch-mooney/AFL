library(keras)
library(ggpmisc)
library(corrplot)

#correlation plots
M <- cor(score_data_lean)
corrplot(M, method = "circle")
#png(height=1200, width=1500, pointsize=15, file="overlap.png")
corrplot(cor(M), method = "color", addCoef.col="grey", number.cex=0.5)

#data preparations
data_score <- score_data_lean
colNum<-as.numeric(ncol(data_score))
data_score[1:col_num] <- lapply(data_score[1:col_num], as.numeric) 
# separate future data
future_score <- data_score%>%
  filter(Margin == 999)%>%
  select(-Margin)#have to adjust the row for new data
#remove future data from training set
data_score<- data_score%>%
  filter(Margin != 999) #have to adjust the row for new data
# organize training set
set.seed(123)
ind_score<-sample(2, nrow(data_score), replace = T, prob = c(0.8, 0.2))
training_score <- data_score[ind_score==1, 2:colNum]
test_score <- data_score[ind_score==2 , 2:colNum]
trainingtarget_score <- data_score[ind_score==1, 1]
testtarget_score <- data_score[ind_score==2, 1]
trainingtarget_score<-as.vector(trainingtarget_score$Margin)
testtarget_score <- as.vector(testtarget_score$Margin)
# Test data is *not* used when calculating the mean and std.
# Normalize training data
train_data <- scale(training_score) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_score, center = col_means_train, scale = col_stddevs_train)
future_data_score <- scale(future_score, center = col_means_train, scale = col_stddevs_train)

# Build model as a function
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dropout(rate = 0.6) %>% 
    layer_dense(units = 32, activation = "relu") %>%
    #layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 16, activation = "relu") %>%
    #layer_dropout(rate = 0.2) %>% 
    layer_dense(units = 8, activation = "relu") %>%
    layer_dropout(rate = 0.6) %>% 
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 750

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  trainingtarget_score,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(print_dot_callback)
)

#Make predicitions from testing data
test_predictions <- model %>% 
  predict(test_data)
# bind predictions and actual values
pred_score<-cbind(test_predictions,
                  testtarget_score)
pred_score <- as.data.frame(pred_score)
pred_score <-pred_score %>% 
              rename(
              test_predictions = V1,
              target = testtarget_score)
# plot results
my.formula <- y ~ x
ggplot(pred_score, aes(x = test_predictions, y=target))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..rr.label.., sep = "~~~")), 
               parse = TRUE)   

futrure_margin_predictions <- model %>% 
  predict(future_data_score)
