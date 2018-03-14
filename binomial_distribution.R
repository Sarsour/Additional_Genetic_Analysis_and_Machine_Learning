# Binomial Distribution

# n = number of trials
# x = sequence up to number of trials
# y = binomial distribution

# Plot both together
par(mfrow=c(2,1))

# P = 0.5 results in a symmetric plot
n <- 30
x <- seq(0, n)
y <- dbinom(x, n, 0.5)
plot(x, y, col = "red", main = "P = 0.5", xlab = "Number of Sucesses (Heads)", ylab = "Probability")

# P = 0.1
n <- 30
x <- seq(0, n)
y <- dbinom(x, n, 0.1)
plot(x, y, col = "red", main = "P = 0.1", xlab = "Number of Sucesses (Heads)", ylab = "Probability")