#par(mfrow=c(2,1))

x<-seq(from=100, to=400, length=101)
y<-dpois(x, 250)
plot(x, y, col="blue", xlab="k", ylab="P(X=k)", pch=15)

y <- dnbinom(x, size=10e10, mu=250)
points(x, y, col="red", pch=16)

y <- dnbinom(x, size=1/0.005, mu=250)
points(x, y, col="green", pch=16)
