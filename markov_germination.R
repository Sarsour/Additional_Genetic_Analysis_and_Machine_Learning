library(markovchain)

states = c("aa", "Aa", "AA")

transitionMatrix <- matrix(data=c(0.2, 0.8, 0, 0.25, 0.5, 0.25, 0, 0, 1), 
	byrow=TRUE, nrow=3, dimnames=list(states, states))

print(transitionMatrix)
