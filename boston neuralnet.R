# libraries ####
install.packages('MASS', 'neuralnet')

library(MASS)
library(neuralnet)

# set randomization factor ####
set.seed(500)

# set data ####
help(Boston)
data <- Boston
View(data)

# removing any rows with missing information ####
  apply(data, 2, function(x) sum(is.na(x)))
    # zeros indicate that there are no missing values in any column

# constructing the training and testing data set for glm ####
  index <- sample(1:nrow(data), round(0.75*nrow(data)))
  train <- data[index, ]
  test <- data[-index, ]
  
# fitting a generalized linear regression to the data ####
  lm.fit <- glm(medv~., data = train)
  plot(lm.fit)
  summary(lm.fit)
  
  pr.lm <- predict(lm.fit, test)
  MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
  
# fitting the nerual network ####
  
  # rescaling data between the interval of [0,1]
    maxs <- apply(data, 2, max)
    mins <- apply(data, 2, min)
    
    scaled <- as.data.frame(scale(data, 
                                  center = mins,
                                  scale = maxs - mins))
  
  # setting the training and testing datasets
    neural_train <- scaled[index, ]
    neural_test <- scaled[-index, ]
  
# constucting the neural network ####
  
  # assigning the names of each columns to n
  n <- names(neural_train)  
    
  # formulating the regression formula 
  f <- as.formula(paste('medv ~', paste(n[!n %in% 'medv'], collapse = ' + ')))
  
  # constructing a 13, 7, 5, 3, 1 neural network tomodel the gression previously defined
  nn <- neuralnet(f, data = neural_train, hidden = c(5,3), linear.output = TRUE)  
  
  # visualizing the potential neural network
  plot(nn)
  
# predicting medv value using the neural network ####
  
  # predicting the neuron threshold value
  predict.nn <- compute(nn, neural_test[ , 1:13])
  names(predict.nn)
  View(predict.nn)  
  
  predict.nn.1 <- predict.nn$net.result*(max(data$medv) - min(data$medv)) + min(data$medv)
  
  # definning the MSE for the neural network to minimize 
  test.r <- (neural_test$medv)*(max(data$medv) - min(data$medv)) + min(data$medv)
  
  MSE.nn <- sum((test.r - predict.nn.1)^2)/nrow(neural_test)

# comparing MSE of glm vs neuralnet ####
  print(paste(MSE.lm, MSE.nn))
  print(paste('Reduction in MSE cause by using a Neural Network: ', (1 - MSE.nn/MSE.lm)*100, '%'))