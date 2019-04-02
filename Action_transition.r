source("transition.R")

### Natural transition counts
P0

### Actions: AAA, BA0, AB0, B0A, 0BA

### BA0
P1 = P0
P1[,2] = P1[,2]*0.8
P1[,4] = P1[,4]*0.8
P1[,5] = P1[,5]*1.2
P1[,7] = P1[,8]*1.2
P_BA0 <- together(P1)
all(near(1, apply(P_BA0, 1, sum)))  # check that rows sum to one


### AB0
P2 = P0
P2[,3] = P2[,3]*0.8
P2[,4] = P2[,4]*0.8
P2[,5] = P2[,5]*1.2
P2[,6] = P2[,6]*1.2
P_AB0 <- together(P2)
all(near(1, apply(P_AB0, 1, sum)))  # check that rows sum to one


### B0A
P3 = P0
P3[,2] = P3[,2]*0.8
P3[,3] = P3[,3]*1.2
P3[,6] = P3[,6]*0.8
P3[,7] = P3[,7]*1.2
P_B0A <- together(P3)
all(near(1, apply(P_B0A, 1, sum)))  # check that rows sum to one


### 0BA
P4 = P0
P4[,2] = P4[,2]*1.2
P4[,3] = P4[,3]*0.8
P4[,6] = P4[,6]*1.2
P4[,7] = P4[,7]*0.8
P_0BA <- together(P4)
all(near(1, apply(P_0BA, 1, sum)))  # check that rows sum to one
