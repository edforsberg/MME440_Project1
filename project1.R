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
  "#63c086",
  "#d04d36",
  "#41c0c7",
  "#bb5663",
  "#4a8a36",
  "#925391",
  "#bcae37",
  "#6b8ecd",
  "#717229",
  "#dc88c1",
  "#388661",
  "#e09571",
  "#a9af63",
  "#9b5f2c")
#datas<-read.csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.data", header=TRUE, sep=",")
set.seed(102)
n = 20
nrClass = 8
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

ggplot(myData, aes(x = x1, y = x2, colour = Y)) +
  geom_point(size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_x_continuous(TeX("x_1"), lim = c(10, 20+5*nrClass), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(10, 40), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_fixed()

h <- 0.2 
x1_arr <- seq(10, 20+5*nrClass, by = h)
x2_arr <- seq(10, 40, by = h)
myTest <- expand.grid(x1_arr, x2_arr)

pred <- lapply(5, function(k) {
  class::knn(
    as.matrix(myData[,1:2]),   # training data (variables)
    myTest,                     # test data (variables)
    as.integer(myData$Y),  # training data (classes)
    k = k)                         # k
})

myTest <- tibble(
  x1 = rep.int(myTest[,1], 1),
  x2 = rep.int(myTest[,2], 1),
  Y = as.factor(do.call(c, pred)))

ggplot() +
  geom_tile(
    aes(x = x1, y = x2, fill = Y),
    data = myTest, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myData, size = 1.5) +
  facet_wrap(~ 5, ncol = 1) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(10, 20+5*nrClass), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(10, 40), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()

fit_lda <- MASS::lda(Y ~ x1 + x2, myTest)
myTest <- expand.grid(x1_arr, x2_arr)
colnames(myTest) <- c("x1", "x2")
myTest <- tibble(
  x1 = rep.int(myTest[,1], 1),
  x2 = rep.int(myTest[,2], 1),
  Y = predict(fit_lda, myTest)$class)

ggplot() +
  geom_tile(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    colour = "transparent",
    data = myTest,
    width = h,
    height = h,
    alpha = 0.5) +
  geom_point(
    aes(x = x1, y = x2, colour = Y, fill = Y),
    data = myData, size = 1.5) +
  scale_colour_manual("Class", values = cbPalette) +
  scale_fill_manual("Class", values = cbPalette, guide = FALSE) +
  scale_x_continuous(TeX("x_1"), lim = c(10, 20+5*nrClass), expand = c(0, 0)) +
  scale_y_continuous(TeX("x_2"), lim = c(10, 40), expand = c(0, 0)) +
  theme(legend.position = "right") +
  coord_fixed()
  ggtitle("LDA")
