cat("\014")
options(warn=1)
#ggsave("filename.eps", device=cairo_ps) save image
library(tidyverse)
library(latex2exp)
library(FNN)
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
  "#9b5f2c") #Randomly generated color values 
#datas<-read.csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.data", header=TRUE, sep=",")
#------------------------------# create test data
CreateCircleData <- function(nrClass, nrOfDataPts, std, seed){ # Create data points were classes form a circle
  set.seed(seed)
  n = nrOfDataPts/nrClass
  myData <- data.frame(x1=double(),x2=double(),Y=integer())
  for(i in 1:nrClass){
    x1 = rnorm(n, mean = 9*cos(2*pi*i/nrClass), sd = std)
    x2 = rnorm(n, mean = 9*sin(2*pi*i/nrClass), sd = std)
    Y = rep(i,n)
    myData <- data.frame(x1=c(myData$x1,x1),
                         x2=c(myData$x2,x2),
                         Y=c(myData$Y,Y))
  }
  myData$Y<- as.factor(myData$Y)
  colnames(myData)<-c("x1","x2","Y")
  myData <- myData[sample(nrow(myData)),] #randomly samlpe order to data to use in evaluation
  return(myData)
}

CreateQuadData <- function(sides, nrOfDataPts, std, seed){ # Create dataset were classes form a square 
  set.seed(seed)
  n = nrOfDataPts/(sides*sides)
  atclass=0
  myData <- data.frame(x1=double(),x2=double(),Y=integer())
  for(i in 1:sides){
    for(j in 1:sides){
    atclass= atclass +1
    x1 = rnorm(n, mean = (10*(i - (1/2 + sides/2))), sd = std)
    x2 = rnorm(n, mean = (10*(j - (1/2 + sides/2))), sd = std)
    Y = rep(atclass,n)
    myData <- data.frame(x1=c(myData$x1,x1),
                         x2=c(myData$x2,x2),
                         Y=c(myData$Y,Y))
  }
  }
  myData$Y<- as.factor(myData$Y)
  colnames(myData)<-c("x1","x2","Y")
  myData <- myData[sample(nrow(myData)),]  #randomly samlpe order to data to use in evaluation
  return(myData)
}
#----------Plot points#
n=480 #Create values for dataset
nrClass=2
std = 4

myData = CreateCircleData(nrClass, n, std, 100) #Circle data used for plots
ggplot(myData, aes(x = x1, y = x2, colour = Y)) + #plot of circle dataset
  geom_point(size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_fixed()

myQuadData = CreateQuadData(3,n, std, 59) #Square shaped data set used for plots
ggplot(myQuadData, aes(x = x1, y = x2, colour = Y)) + #plot of square dataset
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

pred <- lapply(5, function(k) {#knn r-function
  FNN::knn(
    as.matrix(myData[,1:2]),   
    myGrid,                     
    as.integer(myData$Y),  
    k = k)                         
})

myGrid <- tibble(
  x1 = rep.int(myGrid[,1], 1),
  x2 = rep.int(myGrid[,2], 1),
  Y = as.factor(do.call(c, pred)))

ggplot() + #plot of circle dataset with knn decision boundarys
  geom_tile(
    aes(x = x1, y = x2, fill = Y),
    data = myGrid, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myData, size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()

fit_lda <- MASS::lda(Y ~ x1 + x2, myData) #LDA fit function
myGrid <- expand.grid(x1_arr, x2_arr)
colnames(myGrid) <- c("x1", "x2")
myGrid <- tibble(
  x1 = rep.int(myGrid[,1], 1),
  x2 = rep.int(myGrid[,2], 1),
  Y = predict(fit_lda, myGrid)$class)

ggplot() + #plot of circle dataset with LDA decision boundarys
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

#----------New Geometri#
#---quad#
myGrid <- expand.grid(x1_arr, x2_arr)
pred <- lapply(5, function(k) {
  FNN::knn(
    as.matrix(myQuadData[,1:2]),   
    myGrid,                     
    as.integer(myQuadData$Y),  
    k = k)                         
})

myGrid <- tibble(
  x1 = rep.int(myGrid[,1], 1),
  x2 = rep.int(myGrid[,2], 1),
  Y = as.factor(do.call(c, pred)))

ggplot() + #plot of square dataset with knn decision boundarys
  geom_tile(
    aes(x = x1, y = x2, fill = Y),
    data = myGrid, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myQuadData, size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()

fit_lda <- MASS::lda(Y ~ x1 + x2, myQuadData)
myGrid <- expand.grid(x1_arr, x2_arr)
colnames(myGrid) <- c("x1", "x2")
myGrid <- tibble(
  x1 = rep.int(myGrid[,1], 1),
  x2 = rep.int(myGrid[,2], 1),
  Y = predict(fit_lda, myGrid)$class)

ggplot() + #plot of square dataset with LDA decision boundarys
  geom_tile(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    colour = "transparent",
    data = myGrid,
    width = h, height = h,
    alpha = 0.5) +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myQuadData, size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(-25, 25), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(-25, 25), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()

#----------Errors#
  
errorFuncKNN <- function(data,n,k){ #function to retorn arror rate for knn
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
        as.matrix(train[,1:2]),   
        as.matrix(test[,1:2]),  
        as.integer(train$Y),  
        k = k)
    })
    pred_Y <- as.factor(do.call(c, pred_test))
    pred_Y<-factor(pred_Y, levels = 1:nrcl)
    error = error + length(which(test$Y != pred_Y))
  }
  error/len
}

errorFuncLDA <- function(data,n){ #function to retorn arror rate for LDA
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

#-------------------------------#

classVector = 2:20 #Values for error computations
nrDataPts = 480
std = 4
seed = 100
nrFolds = 10
k1 = 5
k2 = 20


calcError <- function(classVector,nrDataPts,std,seed, nrFolds, k1, k2){ #Calculate error to a data.frame
  knnErrors1 <- rep(0)
  knnErrors2 <- rep(0)
  ldaErrors <- rep(0)
  counter = 0
  for (i in classVector){
    counter = counter+1
    data = CreateCircleData(i,nrDataPts,std,seed)
    knnErrors1[counter] = errorFuncKNN(data, nrFolds,k1) #errors for knn (=5)
    knnErrors2[counter] = errorFuncKNN(data, nrFolds,k2) #errors for knn (=20)
    ldaErrors[counter] = errorFuncLDA(data, nrFolds) #errors LDA
  }
  errorData = data.frame(knnErrors1,knnErrors2,ldaErrors)
  errorData
}
print(calcError(classVector, nrDataPts, std, seed, nrFolds, k1,k2))


Create.plot<-function(classVector,k1,k2){#create plot of errors when varying nr classes
  # Generate some data
  x<-classVector; 
  df=calcError(classVector, nrDataPts, std, seed, nrFolds, k1,k2)
  y1<-df[, 1]; 
  y2<-df[, 2];
  y3<-df[, 3];
  p.plot<-plot(x, y1, type="b", pch=19, col="red", main="Error plot(n=480)", xlab="Number of classes", ylab="Percentage of errors")#, xaxt='n', ticks=TRUE)
  
  # Add a line
  lines(x, y2, pch=18, col="green", type="b", lty=2)
  
  # Add a line
  lines(x, y3, pch=18, col="blue", type="b", lty=2)
  
  # Add a legend
  legend("topleft", legend=c("kNN(k=5)", "kNN(k=20)", "LDA"),
         col=c("red", "green", "blue"), lty=1:2, cex=0.8)
  return(p.plot)  
}

plot1=Create.plot(2:20, 5, 20)
