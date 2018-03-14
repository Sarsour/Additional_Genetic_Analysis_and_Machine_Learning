
x <-seq(from=0, to=200, length = 51)

y <- dnbinom(x, size=1/0.3, mu=80)
plot(x, y, col="blue", xlab="Count", ylab="Probability", pch=16)

a <- seq(from=0, to=149)
b <- dnbinom(a, size=1/0.3, mu=80) 
pval1tail <- 1-sum(b)
print(pval1tail)