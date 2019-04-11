classVector = 1:6*3
nrDataPts = 480
std = 4
seed = 1
nrFolds = 10
k1 = 4
k2 = 20


calcError <- function(classVector,nrDataPts,std,seed, nrFolds, k1, k2){
  knnErrors1 <- vector(mode="numeric", length=6)
  knnErrors2 <- vector(mode="numeric", length=6)
  index = 0
  for (i in classVector){
    index = index+1
    data = CreateData(i,nrDataPts,std,seed)
    knnErrors1[[index]] = errorfuncKNN(data, nrFolds,k1)
    knnErrors2[[index]] = errorfuncKNN(data, nrFolds,k2)
    ldaErrors[[index]] = errorFuncLDA(data, nrDataPts)
  }
  errorData = data.frame(knnErrors1,knnErrors2,ldaErrors)
  return(errorData)
  
}
print(calcError(classVector, nrDataPts, std, seed, nrFolds, k1,k2))

