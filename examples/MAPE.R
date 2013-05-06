x <- rnorm(10, 4)
y <- x + rnorm(10, 0, 0.3)
MAPE(x,y)
sMAPE(x,y)
