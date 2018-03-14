# Poisson Distribution

# Binomial distribution assumes that each trial is either a success or failure
dbinom_value_1 <- dbinom(3, 7, 0.1)
print(dbinom_value_1)

dbinom_value_2 <- dbinom(3, 7*60, 0.1/60)
print(dbinom_value_2)

dbinom_value_3 <- dbinom(3, 7*600, 0.1/600)
print(dbinom_value_3)

dbinom_value_4 <- dbinom(3, 7*6000, 0.1/6000)
print(dbinom_value_4)

# Poisson Distribution doesn't need this assumption (Density)
dpois_value <- dpois(3, 7)
print(dpois_value)

# Random Generation - Poisson Distribution (running 3 times)
replicate(3, print(rpois(1,14)))
