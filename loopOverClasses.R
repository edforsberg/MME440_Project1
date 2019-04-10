CreateClasses <- function(nrOfClasses, nrOfDataPts, var, seed) {
  {
    
    set.seed(seed)
    n = nrOfDataPts
    nrClass = nrOfClasses
    myData <- data.frame(x1=double(),x2=double(),Y=integer())
    for(i in 1:floor(nrClass/2)){
      k = i*2
      x11 = rnorm(n, mean = 20+10*(i-1), sd = 3)
      x21 = rnorm(n, mean = 20, sd = 3)
      x12 = rnorm(n, mean = 20+10*(i-1), sd = 3)
      x22 = rnorm(n, mean = 30, sd = 3)
      Y1 = rep((k-1),n)
      Y2 = rep(k,n)
      myData <- data.frame(x1=c(myData$x1,x11,x12),
                           x2=c(myData$x2,x21,x22),
                           Y=c(myData$Y,Y1,Y2))
    }
    myData$Y<- as.factor(myData$Y)
    colnames(myData)<-c("x1","x2","Y")
    
    
    return(mydata)
    
  }


nrClasses = c(2,4,6,8,10)
for (i in nrClasses) {
  
  trainData = CreateClasses(i, 200, 3, 1)
  predKNN <- lapply(5, function(k) {
    class::knn(
      as.matrix(trainData[,1:2]),   # training data (variables)
      myTest,                     # test data (variables)
      as.integer(trainData$Y),  # training data (classes)
      k = k)                         # k
  })
  
  
  
}