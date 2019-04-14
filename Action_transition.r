source("transition.R")

### Natural transition counts correspond to action AAA
P0

### we don't have data for the other possible actions BA0,
### AB0, B0A, 0BA, so we rely on studies of police presence
### effect on crime from the literature.

### BA0
P1 <- P0
P1[,2,] <- P1[,2,]*0.8
P1[,4,] <- P1[,4,]*0.8
P1[,5,] <- P1[,5,]*1.2
P1[,7,] <- P1[,8,]*1.2
P_BA0 <- counts2probs(P1)
## check that all planes from state s sum to one
all( near(1, sapply(1:length(S), function(s) sum(P_BA0[s,,]))) )

### AB0
P2 <- P0
P2[,3,] <- P2[,3,]*0.8
P2[,4,] <- P2[,4,]*0.8
P2[,5,] <- P2[,5,]*1.2
P2[,6,] <- P2[,6,]*1.2
P_AB0 <- counts2probs(P2)
all( near(1, sapply(1:length(S), function(s) sum(P_AB0[s,,]))) )

### B0A
P3 <- P0
P3[,2,] <- P3[,2,]*0.8
P3[,3,] <- P3[,3,]*1.2
P3[,6,] <- P3[,6,]*0.8
P3[,7,] <- P3[,7,]*1.2
P_B0A <- counts2probs(P3)
all( near(1, sapply(1:length(S), function(s) sum(P_B0A[s,,]))) )

### 0BA
P4 <- P0
P4[,2,] <- P4[,2,]*1.2
P4[,3,] <- P4[,3,]*0.8
P4[,6,] <- P4[,6,]*1.2
P4[,7,] <- P4[,7,]*0.8
P_0BA <- counts2probs(P4)
all( near(1, sapply(1:length(S), function(s) sum(P_0BA[s,,]))) )
