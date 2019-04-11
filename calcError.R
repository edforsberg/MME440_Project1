classVec = c(2,4,8,12,16,20)
nrDataPts = 480



calcError <- function(classVector,nrDataPts,std,seed, nrFolds){
  knnError = 
  for (i in classVector)
    
    data = CreateData(i,nrDataPts,std,seed)
  errorKNN = errorfuncKNN(data, nrFolds)
  
  
  
  
}