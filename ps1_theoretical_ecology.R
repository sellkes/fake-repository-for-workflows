#B.1
lambda <- 2
TimeSteps <- 10
t <- c(1:TimeSteps)
N <- numeric()
N[1] = 1

#B.2
for (x in t)
{
  N[t+1] = lambda*N[t]
} 

t <- append(0,t) #adding timestep 0 to t for plotting

plot(t, N, log ="y")

#do this again with new values to create new vectors
lambda_2 <- 1.8
t_2 <- c(1:TimeSteps)
N_2 <- numeric()
N_2[1] = 1

for (x in t_2)
{
  N_2[t_2+1] = lambda_2*N_2[t_2]
} 

t_2 <- append(0,t_2) #adding timestep 0 to t for plotting


points(t_2, N_2, col="blue")

#B.3

