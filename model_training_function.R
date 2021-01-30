model_training <- function(inputs, target){
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
    fit(inputs,
        target,
        epochs = 1500,
        batch_size = 256,
        validation_split = 0.3)
  #list(model = model, histroy = history, summary = summary(model))
  return(model)
}
