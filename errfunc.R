# Generate some data
x<-c(2, 4, 8, 12, 16, 20); 
y1<-x*x; 
y2<-2*y1;
y3<-x;
plot(x, y1, type="b", pch=19, col="red", main="", xlab="Number of classes", ylab="Percentage of errors", xaxt='n', ticks=TRUE)

# Add a line
lines(x, y2, pch=18, col="green", type="b", lty=2)

# Add a line
lines(x, y3, pch=18, col="blue", type="b", lty=2)

xtick<-c(2, 4, 8, 12, 16, 20)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, las=1, srt = 0, pos = 1, xpd = TRUE)

# Add a legend
legend("topleft", legend=c("kNN(k=)", "kNN(k=)", "LDA"),
       col=c("red", "green", "blue"), lty=1:2, cex=0.8)
