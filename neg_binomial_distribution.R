par(mfrow=c(3,1))

x<-seq(from=0, to=25)
y <-dnbinom(x, size=1, p=1-0.5)
plot(x, y, col="blue", xlab="k", ylab="P(X=k)", pch=16)

x<-seq(from=0, to=25)
y <-dnbinom(x, size=2, p=1-0.35)
plot(x, y, col="blue", xlab="k", ylab="P(X=k)", pch=16)

x<-seq(from=0, to=25)
y <-dnbinom(x, size=2, p=1-0.8)
plot(x, y, col="blue", xlab="k", ylab="P(X=k)", pch=16)