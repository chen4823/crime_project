## implementation of value iteration for assignment of police patrols
library(tidyverse)

source("Action_transition.r")

## state space is of the form HLL, that is, the crime state
## in each of the three regions WB,EB,SP. H = high and L = low.
S

## the action space is the valid assignment of patrols to regions
## A = able (1 officer), B = baker (2 officers), 0 = empty (no officer)
A <- c("AAA","BA0","AB0","B0A","0BA")

## the transition probabilities are computed in Action_transition.r
## here we collect them into one data structure
P <- array(data=c(as.vector(P_AAA),
                  as.vector(P_BA0),
                  as.vector(P_AB0),
                  as.vector(P_B0A),
                  as.vector(P_0BA)),
                  dim=c(8,8,10,5),
                  dimnames=list(from=S, to=S, cost=0:ub, action=A))

gamma <- 0.9   # discount factor

## applies the DP iteration for state s. this is the update rule
## shown in equation (4.10) in the RL book. note that the immediate
## cost r and the cost-to-go V[s.prime] are taken in expectation.
FV <- function(s) {
    cost <- rep(0, length(A)) # holds the expected cost for each control
    names(cost) <- A
    for (a in A) {
        for (s.prime in S) {
            for (r in 0:ub) {
                cost[a] <- cost[a] + P[s,s.prime,r+1,a]*(r + gamma*V[s.prime])
            }
        }
    }
    cost.star <- min(cost)
    a.star <- which(near(cost.star, cost))[1]   # ties are possible
    ## return the min cost and the control that achieved it
    list(cost.star, A[a.star])
}

## apply the value iteration algorithm. note that this is the gauss-seidel
## version of value iteration because we iterate one state at a time and
## use the interim results.

## initialization
theta <- .0001         # tolerance for convergence
k <- 0                 # iteration number
V <- rep(0, length(S)) # initially, start the value iteration with zero cost
V.prev <- rep(theta + 1, length(S)) # to check convergence
delta <- V.prev - V    # to check convergence
policy <- rep("AAA", length(S))

names(V) <- S
names(policy) <- S

while (!all(near(delta, 0, tol=theta))) {
    k <- k + 1
    V.prev <- V
    for (s in S) {
        lst <- FV(s)
        V[s] <- lst[[1]]
        policy[s] <- lst[[2]]
    }
    delta <- V.prev - V
    message("iteration = ", k, ", policy = ", policy, ", convergence = ", sum(delta))
}
