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

plot(t, N, log ="y" )

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
#The plot of logN vs time is a straight line, where the plot of N vs time is an exponential curve - 
#plotting the logN as a function of time shows us that the growth of the population is happening at a constant rate.

?length
#Sorry I can't figure out how to do one line of code so I am doing things a longer and probably unnecessarily 
#complicated way

# I rearranged the equation to instead find lambda (which I'm calling r for rate) from known N values, adding it to 
# a new vector so I can then use it to plot things. I am not confident I did this right
length(N)
r <- numeric() #create a vector for growth rates

for(i in 1:length(N))
{
r[i] <- N[i+1]/N[i]
}

plot (N, r, log = "y")
points (N, r, col = "purple")
abline(h=lambda, lty=2, col = 'red')

#B.4
lambda_4 <- 0.8
TimeSteps_4 <- 100000
t_4 <- c(1:TimeSteps_4)
N_4 <- numeric()
N_4[1] = 1000


for (x in t_4) {
  while (x > 0) {
  N_4[t_4+1] = lambda_4*N_4[t_4]
  }
} 

length(N_4) #length of dataframe where the values are positive, or the population isn't extinct
extinction <- length(N_4 [N_4 > N_4[1]/10]) #length of dataframe where values are above 10% of starting values

#a - the population reaches 0 after the 10001th time step

#b - The population dips below 10 (which is 10% of 1000) at the 12th timestep

t_4 <- append(0,t_4) #add 0 timestep to t_4 for plotting

plot(t_4, N_4)

abline(v=extinction, lty=3, col = "mediumpurple1")
