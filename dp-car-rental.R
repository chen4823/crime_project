## implementation of value iteration for the car rental example
## on page 81 of the RL book (Sutton and Barto).
library(tidyverse)

## state space is of the form (i,j), that is, i cars at loc 1
## and j cars at loc 2 at the end of the day.
S <- 0:20

## the action space is how many cars to move from loc 1
## to loc 2. a negative number means loc 2 to loc 1
A <- -5:5

## compute probability of going from f1 to t1 at loc 1
## demand for rental cars and returns are independent
dist1 <- function(f1,t1,a) {
    p <- 0
    ## cover all reasonable values for demand and returns
    for (d in 0:20) {
        for (r in 0:20) {
            if (t1 == f1 - d + r - a) {
                # mean demand is 3, mean num returns is 3
                p <- p + dpois(d, 3)*dpois(r, 3)
            }
        }
    }
    p
}

## compute probability of going from f2 to t2 at loc 2
## demand for rental cars and returns are independent
dist2 <- function(f2,t2,a) {
    p <- 0
    ## cover all reasonable values for demand and returns
    for (d in 0:20) {
        for (r in 0:20) {
            if (t2 == f2 - d + r + a) {
                # mean demand is 4, mean num returns is 2
                p <- p + dpois(d, 4)*dpois(r, 2)
            }
        }
    }
    p
}

## transition probabilities, that is to say, the
## probability of going from state (f1,f2) to state (t1,t2)
P <- array(0, dim=c(21,21,21,21,11),
           dimnames=list(from1=S,from2=S,to1=S,to2=S,action=A))
for (f1 in S) {
    for (f2 in S) {
        for (t1 in S) {
            for (t2 in S) {
                for (a in A) {
                    p1 <- dist1(f1,t1,a) # prob of going from f1 to t1 at loc 1
                    p2 <- dist2(f2,t2,a)
                    P[f1+1,f2+1,t1+1,t2+1,a+6] <- p1*p2 # the action a connects loc 1 and loc 2
                }
            }
        }
    }
}

## immediate (expected) reward of being in state (i,j) and taking action a
g <- function(i,j,a) {
    ## cover all reasonable values for demand
    ## assume demands at loc1 and loc2 are independent
    rev <- 0
    inv1 <- min(max(0, i-a), 20)
    inv2 <- min(max(0, j+a), 20)
    for (d1 in S) {
        for (d2 in S) {
            rev <- rev +
                dpois(d1, 3)*ifelse(d1 > inv1, inv1, d1)*10 +
                dpois(d2, 4)*ifelse(d2 > inv2, inv2, d2)*10
        }
    }
    rev - 2*abs(a)
}

alpha <- 0.9   # discount factor

## applies the DP iteration for state (i,j)
FJ <- function(i,j) {
    value <- rep(-1e6, length(A)) # holds the immediate value for each possible action
    for (a in A) {
        if (a > i || a < -j) {
            next
        }
        value.to.go <- 0   # holds expected value-to-go for action a
        for (t1 in S) {
            for (t2 in S) {
                value.to.go <- value.to.go + P[i+1,j+1,t1+1,t2+1,a+6]*V[t1+1,t2+1]
            }
        }
        value[a+6] <- g(i,j,a) + alpha*value.to.go
    }
    value.star <- max(value)
    idx <- which(near(value.star, value))
    ## return the max value and the action that acheived it
    list(value.star, A[idx])
}

## apply the value iteration algorithm.
## note that this is the gauss-seidel version of value iteration
## because we iterate one state at a time and use the interim results,
## hence the notation FJ(i) as in bertsekas.
eps <- .0001           # tolerance for convergence
k <- 0                 # iteration number
V <- matrix(0, nrow=length(S), ncol=length(S),     # value function
            dimnames=list(loc1=S, loc2=S))
V.prev <- V + 1      # to check convergence
policy <- V          # will hold the optimal policy

while (!all(near(V - V.prev, 0, tol=eps))) {
    k <- k + 1
    V.prev <- V
    for (i in S) {
        for (j in S) {
            lst <- FJ(i,j)
            V[i+1,j+1] <- lst[[1]][1]
            policy[i+1,j+1] <- lst[[2]][1]
        }
    }
    message("iteration k = ", k, ", convergence : ", sum(V - V.prev))
}
