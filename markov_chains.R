library("markovchain")

excerpt <- "We don't need no education.
We don't need no thought control.
No dark sarcasm in the classroom.
Teacher leave them kids alone
Hey! Teacher! Leave them kids alone!
All in all it's just another brick in the wall.
All in all you're just another brick in the wall."

excerpt <- tolower(excerpt)
characters <- unlist(strsplit(excerpt, ""))

# Number of states
states <- unique(characters)
num_states <- length(states)
print(num_states)

# Sort in data frame
state_table <- table(characters)
state_df <- as.data.frame(state_table)

# Probability of getting a '!'
char_excl_point <- 3
total_chars <- sum(state_df$Freq)
excl_point_freq <- (char_excl_point / total_chars)
print(excl_point_freq)

# First-order markov chain
states=c("we", "dont", "need", "no", "education", "thought", "control", "dark", "sarcasm", "in",
"the", "classroom")
transitionMatrix<-matrix(data=c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/3, 1/3, 0, 1/3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 1), byrow=TRUE, nrow=12, dimnames=list(states, states))
mchainObject<-new("markovchain", transitionMatrix = transitionMatrix)

# Generate new lyrics
t0="we"
chain <- paste(t0, paste(rmarkovchain(n=8, object=mchainObject, t0=t0), collapse=" "), collapse=" ")
print(chain)

# Absorbing state
absorbingState <- absorbingStates(mchainObject)
print(absorbingState)
irreducibility <- is.irreducible(mchainObject)
print(irreducibility)

transitionMatrix[12, 1] <- 1
transitionMatrix[12, 12] <- 0
mchainObject<-new("markovchain", transitionMatrix = transitionMatrix)

new_irreducibility <- is.irreducible(mchainObject)
print(new_irreducibility)

# Stationary Distributions
transitionMatrix2<- transitionMatrix %*% transitionMatrix
print(transitionMatrix2)

library(matrixcalc)
matrix_times_100 <- matrix.power(transitionMatrix, 100)
print(matrix_times_100["need", "thought"])
print(matrix_times_100["we", "thought"])

# Periodic Chain
print(period(mchainObject))

# Steady States
print(steadyStates(mchainObject))

print(eigen(t(transitionMatrix)))
print(eigen(t(transitionMatrix))$vectors[,1])
print(sum(eigen(t(transitionMatrix))$vectors[,1]))
print(sum(eigen(t(transitionMatrix))$vectors[,4]))

