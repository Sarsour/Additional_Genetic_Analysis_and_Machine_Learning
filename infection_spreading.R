# Transition probability from state St to St+1

prob1 <- dbinom(0, 3, 1-(1-0.01))
print(prob1)

prob2 <- dbinom(0, 3, 1-(1-0.01))
print(prob2)

prob3 <- dbinom(0, 2, 1-(1-0.01)^2)
print(prob3)


# Tranisition Matrix
p <- 0.01
N <- 4
states <- seq(from=0, to=N)
fromState <- matrix(rep(states, each = N+1), byrow=TRUE, nrow=N+1)
toState <- matrix(rep(states, times = N+1), byrow=TRUE, nrow=N+1)

transitionMatrix <- dbinom(fromState-toState, fromState, 1-(1-p)^(N-fromState))
rownames(transitionMatrix) <- states
colnames(transitionMatrix) <- states

print(transitionMatrix)


transitionMatrix[2:N, 2:N]
diag(N-1) -transitionMatrix[2:N, 2:N]
fundamentalMatrix <- solve(diag(N-1) -transitionMatrix[2:N, 2:N])
print(fundamentalMatrix)


# Infection paths
growpaths <- function(paths) {
	for(n in 1:length(paths)) {
	
		path<-paths[[n]]
		lastS = path$S[length(path$S)]
		lastI = path$I[length(path$I)]
		if(lastS>0 && lastI>0) {
			paths[[n]]<-rbind(path, data.frame(S=lastS, I=0))
			for(i in 1:lastS) {
				paths[[length(paths)+1]]<-rbind(path, data.frame(S=lastS-i, I=i))
				paths<-growpaths(paths)
			}
		 }
	}
	return (paths)
}

paths <- list()
paths[[length(paths)+1]] <- data.frame(S=c(3),I=c(1))
paths <- growpaths(paths)
print(paths)



# Duration
duration <- max(which(paths[[5]]$I!=0))
print(duration)


value <- dbinom(1, 3, 1-(1-0.2))*dbinom(0, 2, 1-(1-0.2))
print(value)


# Probability transition within each path
p <- 0.2
for(n in 1:length(paths)) {
	paths[[n]]$P[1] <- 1
	for(i in 1:(length(paths[[n]]$S)-1)) {
		paths[[n]]$P[i+1] <- dbinom(paths[[n]]$S[i]-paths[[n]]$S[i+1], paths[[n]]$S[i], 1-(1-p)^paths[[n]]$I[i])
	}
}
print("start")
duration <- 0
for(n in 1:length(paths)) {
	path<-paths[[n]]
	pathDuration <- sum(paths[[n]]$I)-1
	print(pathDuration)
	pathProbability <- prod(paths[[n]]$P)
	duration <- duration + pathDuration* pathProbability
	#print(pathProbability*pathDuration)
}
print("finish")
print(duration)



# Mutation Markov Chains
a <-0.04
b <-0.03
nucleotides <- c("A", "G", "C", "T")
transitionMatrix <- matrix(data=c(-a-2*b, a, b, b, a, -a-2*b, b, b, b, b, -a-2*b, a, b, b, a, -a-2*b), byrow=TRUE, nrow=4, dimnames=list(nucleotides, nucleotides))

print(transitionMatrix)

library(matrixcalc)
sixstep <- matrix.power(transitionMatrix, 6)
print(sixstep)

# Continuous-Time
print(eigen(transitionMatrix))

new <- eigen(transitionMatrix)$vectors[,4]/sum(eigen(transitionMatrix)$vectors[,4])
print(new)
