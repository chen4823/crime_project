## compute transition probabilities by
## 1. counting crimes by year, day of year, shift, region
## 2. apply threshold to determine state in each region (L or H)
## 3. count the number of transitions from state to next state
## 4. turn counts into transition probabilities

source("data.R")    # clean up data

## we need to have crime counts for each region by
## year, doy, shift.
cc2 <- with(X, tapply(shift, list(year, doy, shift, region), length))
cc2[ is.na(cc2) ] <- 0

## to store the crime counts so that we can apply the threshold for state
CC2 <- data.frame(year=rep(2017:2018, each=365*3),
                  doy=rep(1:365, each=3, times=2),
                  shift=levels(X$shift), times=365*2,
                  WB = integer(365*2*3),   # initialize counts to zero
                  EB = integer(365*2*3),
                  SP = integer(365*2*3)
                  )

## loop through the data and place the crime count for
## a particular year, day of year, shift, and region into
## the CC2 data frame.
for (i in 1:nrow(X)) {
    y <- as.numeric(X$year[i])  # the integer level of the factor (1 or 2)
    d <- X$doy[i]
    s <- as.numeric(X$shift[i]) # integer (1, 2, or 3)

    idx <- (d*3 - 3 + s) + 1095*(y-1)  # magic incantation
    
    r <- switch(as.character(X$region[i]),
           "West Bank" = 1,
           "East Bank North" = 2,
           "St.Paul" = 3,
           NA    # other
           )
    if (is.na(r)) next
    r2 <- switch(r, 4, 1, 3)  # index into cc2
    CC2[idx, 4+r] <- cc2[y, d, s, r2]
}

## given the counts by region, return the state vector
determine.state <- function(wb, eb, sp) {
    wbs <- ifelse(wb > 1.5, "H", "L")
    ebs <- ifelse(eb > 1.5, "H", "L")
    sps <- ifelse(sp > 1.5, "H", "L")
    paste(wbs, ebs, sps, sep="")
}

CC2$state <- with(CC2, mapply(determine.state, WB, EB, SP)) 

## set up the state space
PS <- expand.grid(c("L","H"), c("L","H"), c("L","H"))
S <- apply(PS, 1, paste, collapse="")

state1 <- lag(CC2$state) # state during a shift
state2 <- CC2$state      # state during the following shift

## make the transition probabilities of the form
## P[s,s.prime,r]
## probability of going from state s to state s.prime and
## paying cost r

ub <- max(apply(CC2[, c("WB","EB","SP")], 1, sum))  # max crime count during any shift

## the transition matrix initalized to zero
P0 <- array(0, dim=c(8,8,ub+1),
            dimnames=list(from=S, to=S, cost=0:ub))

## fill in the counts
for (i in 2:length(state1)) {
    r <- sum(CC2[i, c("WB","EB","SP")])
    P0[ state1[i], state2[i], r+1] <- P0[ state1[i], state2[i], r+1 ] + 1
}

## convert counts to transition probabilities
counts2probs <- function(X) {
    for (s in 1:length(S)) {
        denom <- sum(X[s,,])
        for (s.prime in 1:length(S)) {
            for (r in 1:(ub+1)) {
                X[s,s.prime,r] <- X[s,s.prime,r]/denom
            }
        }
    }
    X  
}
P_AAA <- counts2probs(P0)

## check that all planes from state s sum to one
all( near(1, sapply(1:length(S), function(s) sum(P_AAA[s,,]))) )


