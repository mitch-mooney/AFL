model_data <- function(data){

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

dataframe_list <- list(training = training, trainLabels = trainLabels,test = test, trainLabels = trainLabels, test_var = test_var, testtarget = testtarget, test_dim = test_dim, future_matrix = future_matrix, full_future_matrix = full_future_matrix, data = data)

}

