# Generate some data
x<-seq(2, 20, 1); 
y1<-c(2.71, 8.54, 16.25, 22.29, 31.04, 40.76, 45.42, 50.52, 56.88, 60.47, 
      67.29, 69.02, 71.85, 74.79, 75.21, 75.63, 76.07, 80.00, 83.33); 
y2<-c(2.08, 6.67, 12.08, 19.58, 28.12, 35.29, 42.08, 46.54, 52.71, 57.72,
      63.96, 68.80, 69.33, 69.17, 73.75, 73.53, 71.79, 76.42, 81.46);
y3<-c(2.08, 6.25, 12.08, 18.75, 26.46, 34.87, 40.62, 44.44, 52.29, 55.81,
      60.62, 64.96, 64.71, 70.21, 71.25, 71.64, 71.58, 76.21, 78.75);
plot(x, y1, type="b", pch=19, col="red", main="Error plot", xlab="Number of classes", ylab="Percentage of errors")#, xaxt='n', ticks=TRUE)

# Add a line
lines(x, y2, pch=18, col="green", type="b", lty=2)

# Add a line
lines(x, y3, pch=18, col="blue", type="b", lty=2)

#xtick<-c(2, 4, 8, 12, 16, 20)
#axis(side=1, at=xtick, labels = FALSE)
#text(x=xtick,  par("usr")[3], 
#     labels = xtick, las=1, srt = 0, pos = 1, xpd = TRUE)

# Add a legend
legend("topleft", legend=c("kNN(k=)", "kNN(k=)", "LDA"),
       col=c("red", "green", "blue"), lty=1:2, cex=0.8)
