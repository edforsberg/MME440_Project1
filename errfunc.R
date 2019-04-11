# Data generation
x  <- c(2, 4, 6, 10, 20)
y1 <- x.^2
y2 <- x
y3 <- 1/x
df <- data.frame(x,y1,y2,y3)


require(ggplot2)

ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer
  geom_line(aes(y=y2), colour="green")+  # second layer
  geom_line(aes(y=y3), colour="blue")  # third layer


g <- ggplot(df, aes(x))
g <- g + geom_line(aes(y=y1), colour="red")
g <- g + geom_line(aes(y=y2), colour="green")
g <- g + geom_line(aes(y=y2), colour="blue")
g