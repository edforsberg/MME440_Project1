cat("\014")
options(warn=1)
library(tidyverse)
library(latex2exp) # Latex in ggplot2 labels
cbPalette <- c(
  "#da9033",
  "#6d6bd1",
  "#85b937",
  "#b959c8",
  "#52c257",
  "#d13f80",
  "#d04d36",
  "#41c0c7",
  "#bb5663",
  "#4a8a36",
  "#925391",
  "#bcae37",
  "#6b8ecd",
  "#717229",
  "#63c086",
  "#dc88c1",
  "#388661",
  "#e09571",
  "#a9af63",
  "#9b5f2c")
#datas<-read.csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.data", header=TRUE, sep=",")
#------------------------------# create test data
CreateCircleData <- function(nrClass, nrOfDataPts, std, seed){
  set.seed(seed)
  n = nrOfDataPts/nrClass
  myData <- data.frame(x1=double(),x2=double(),Y=integer())
  for(i in 1:nrClass){
    x1 = rnorm(n, mean = 10*cos(2*pi*i/nrClass), sd = std)
    x2 = rnorm(n, mean = 10*sin(2*pi*i/nrClass), sd = std)
    Y = rep(i,n)
    myData <- data.frame(x1=c(myData$x1,x1),
                         x2=c(myData$x2,x2),
                         Y=c(myData$Y,Y))
  }
  myData$Y<- as.factor(myData$Y)
  colnames(myData)<-c("x1","x2","Y")
  myData <- myData[sample(nrow(myData)),]
  return(myData)
}

CreateData <- function(nrOfClasses, nrOfDataPts, std, seed){
    set.seed(seed)
    n = nrOfDataPts
    nrClass = nrOfClasses
    myData <- data.frame(x1=double(),x2=double(),Y=integer())
    for(i in 1:floor(nrClass/2)){
      k = i*2
      x11 = rnorm(n, mean = 20+10*(i-1), sd = std)
      x21 = rnorm(n, mean = 20, sd = std)
      x12 = rnorm(n, mean = 20+10*(i-1), sd = std)
      x22 = rnorm(n, mean = 30, sd = std)
      Y1 = rep((k-1),n)
      Y2 = rep(k,n)
      myData <- data.frame(x1=c(myData$x1,x11,x12),
                           x2=c(myData$x2,x21,x22),
                           Y=c(myData$Y,Y1,Y2))
    }
    myData$Y<- as.factor(myData$Y)
    colnames(myData)<-c("x1","x2","Y")
    myData <- myData[sample(nrow(myData)),]
    return(myData)
  }
#----------Plot points#
n=480
nrClass=16
std = 4
myData = CreateCircleData(nrClass, n, std, 102)

ggplot(myData, aes(x = x1, y = x2, colour = Y)) +
  geom_point(size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_fixed()

h <- 0.2 
x1_arr <- seq(-25, 25, by = h)
x2_arr <- seq(-25, 25, by = h)
myGrid <- expand.grid(x1_arr, x2_arr)

pred <- lapply(5, function(k) {
  class::knn(
    as.matrix(myData[,1:2]),   # training data (variables)
    myGrid,                     # test data (variables)
    as.integer(myData$Y),  # training data (classes)
    k = k)                         # k
})

myGrid <- tibble(
  x1 = rep.int(myGrid[,1], 1),
  x2 = rep.int(myGrid[,2], 1),
  Y = as.factor(do.call(c, pred)))

ggplot() +
  geom_tile(
    aes(x = x1, y = x2, fill = Y),
    data = myGrid, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myData, size = 1.5) +
  facet_wrap(~ 5, ncol = 1) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()

fit_lda <- MASS::lda(Y ~ x1 + x2, myData)
myGrid <- expand.grid(x1_arr, x2_arr)
colnames(myGrid) <- c("x1", "x2")
myGrid <- tibble(
  x1 = rep.int(myGrid[,1], 1),
  x2 = rep.int(myGrid[,2], 1),
  Y = predict(fit_lda, myGrid)$class)

ggplot() +
  geom_tile(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    colour = "transparent",
    data = myGrid,
    width = h, height = h,
    alpha = 0.5) +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myData, size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()

#ggtitle("LDA")  
#----------Errors#
  
errorFuncKNN <- function(data,n,k){
  nrcl = nlevels(data$Y)
  error = 0
  len = length(data[,1])
  for(i in 1:n){
    ii = ((i-1)*len/n+1):(i*len/n)
    test <- data[ii,]
    test$Y <- factor(test$Y, levels = 1:nrcl)
    train <- data[-ii,]
    train$Y<-factor(train$Y, levels = 1:nrcl)
    pred_test <- lapply(k, function(k) {
      class::knn(
        as.matrix(train[,1:2]),   # training data (variables)
        as.matrix(test[,1:2]),  # test data (variables)
        as.integer(train$Y),  # training data (classes)
        k = k)# k
    })
    pred_Y <- as.factor(do.call(c, pred_test))
    pred_Y<-factor(pred_Y, levels = 1:nrcl)
    error = error + length(which(test$Y != pred_Y))
  }
  error/len
}

errorFuncLDA <- function(data,n){
  nrcl = nlevels(data$Y)
  error = 0 
  len = length(data[,1])
  for(i in 1:n){
    ii = ((i-1)*len/n+1):(i*len/n)
    test <- data[ii,]
    test$Y <- factor(test$Y, levels = 1:nrcl)
    train <- data[-ii,]
    train$Y<-factor(train$Y, levels = 1:nrcl)
    fit_lda <- MASS::lda(Y ~ x1 + x2, train)
    pred_Y = predict(fit_lda, test)$class
    pred_Y<-factor(pred_Y, levels = 1:nrcl)
    error = error+length(which(test$Y != pred_Y))
  }
  error/len
}

myTest1 <- CreateData(8, 20, 3, 103)
print(errorFuncKNN(myTest1,5,5))
print(errorFuncLDA(myTest1,5))

#-------------------------------#