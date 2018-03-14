# Initial Example
alpha <- 0.4
beta <- 0.3
nucleotides <- c("A", "G", "C", "T")
rateMatrix <- matrix(data=c(-alpha-2*beta, alpha, beta, beta, alpha, -alpha-2*beta, beta, beta, beta, beta, -alpha-2*beta, alpha, beta, beta, alpha, -alpha-2*beta), byrow=TRUE,  nrow=4, dimnames=list(nucleotides, nucleotides))
print(eigen(t(rateMatrix)))
print(eigen(t(rateMatrix))$vectors[,1])
print(sum(eigen(t(rateMatrix))$vectors[,1]))
print(eigen(t(rateMatrix))$vectors[,1]/sum(eigen(t(rateMatrix))$vectors[,1]))

# Hidden Example
rateMatrix2 <- matrix(data=c(2/11, 9/11, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0.5, 0, 0, 0, 0.5, 0.25, 0, 0, 1/6, 7/12), byrow=TRUE,  nrow=5)
print(eigen(t(rateMatrix2)))
print(eigen(t(rateMatrix2))$vectors[,1]/sum(eigen(t(rateMatrix2))$vectors[,1]))

# Probability of output sequence "love love is all"
install.packages("HMM")
library("HMM")
library("markovchain")
states = c(1, 2, 3, 4, 5)
symbols = c("all", "you", "need", "is", "love")
transitionMatrix <- matrix(data=c(2/11, 9/11, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0.5, 0, 0, 0, 0.5, 0.25, 0, 0, 1/6, 7/12), byrow=TRUE, nrow=5, dimnames=list(states, states))
emissionMatrix <- matrix(data=c(0.72727, 0, 0, 0, 0.27273, 0, 0.88889, 0, 0, 0.11111, 0, 0, 0.88889, 0, 0.11111, 0, 0, 0, 0.8, 0.2, 0, 0, 0, 0, 1), byrow=TRUE, nrow=5, dimnames=list(states, symbols))
mchainObject <- new("markovchain", transitionMatrix = transitionMatrix)
stationaryDist <- steadyStates(mchainObject)
observations <- c("love", "love", "is", "all")
hmm = initHMM(states, symbols, startProbs=stationaryDist, transProbs=transitionMatrix, emissionProbs = emissionMatrix)
logForwardProbabilities <- forward(hmm,observations)
print(exp(logForwardProbabilities))

# Most likely path
viterbi <- viterbi(hmm,observations)
print(viterbi)

# Nucleotide isochore sequence
states <- c("AT-rich", "GC-rich")
alphabet <- c("A", "T", "C", "G")
hmm <- initHMM(states, alphabet, emissionProbs = matrix(c(0.45, 0.45, 0.05, 0.05, 0.05, 0.05, 0.45, 0.45), byrow=TRUE, nrow=2))
data <- unlist(strsplit("ATTATTAATACGGGCCAATTAGGGCCCAGGCCCTGCGTCCCCCCCCTTATTTTTCGTA", ''))
result <- baumWelch(hmm, data)
print(result)
viterbi <- viterbi(hmm, alphabet)
print(viterbi)
emission <- result$hmm$emissionProbs
print(emission[,emission[1,]>emission[2,] &emission[1,] > 0.01])
print(emission[,emission[2,]>emission[1,] &emission[2,] > 0.01])
print("trained model:")
print(result$hmm)
