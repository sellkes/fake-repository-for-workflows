#A.1
lambda <- 1.01
TimeSteps <- 2500
t <- c(1:TimeSteps)
N <- numeric()
N[1] = 1
sigma <- 0
mu <- 0

for (x in t)
{
  N[t+1] = lambda*exp(rnorm(1,mu, sigma))*N[t]
} 

t <- append(0,t) #adding timestep 0 to t for plotting

plot(t, N, log ="y" )

#when I change sigma there is even more variation in my N values - the line more wiggly 
#its way up. When sigma is 0 the line becoems straight again 

lambda <- 0.9
TimeSteps <- 2500
t <- c(1:TimeSteps)
N <- numeric()
N[1] = 100
sigma <- 0.2
mu <- 0
rlambda <- numeric()

for (x in t)
{
  rlambda[x] = lambda*exp(rnorm(1,mu, sigma))*N[t]
  N[t+1] = rlambda*N[t]
} 

t <- append(0,t) #adding timestep 0 to t for plotting

plot(t, N, log ="y" )

mean(rlambda)

#the mean rlambda is 1.03, which is slightly larger than the lambda value I set of 1.01.
# when lambda is <1 the realized lambda is still larger than the one I set

#plot and save multiple realizations 
lambda <- 1.01
TimeSteps <- 2500
t <- c(1:TimeSteps)
N <- numeric()
N[1] = 0
sigma <- 0.2
mu <- 0
rlambda <- numeric()
reps <- 150
Col<-as.vector(col2rgb('black')/255) # to convert a named colour into its code
Col<-rgb(Col[1],Col[2],Col[3], alpha=0.2) 

plot(N,type='n',log='y',ylim=c(1,10^20)) 
for (r in 1:reps)
{for (x in t)
{
  rlambda[x] = lambda*exp(rnorm(1,mu, sigma))*N[t]
  N[t+1] = rlambda*N[t]
} 
  lines(N, col= Col)
}

#I am getting a ton of errors that say "In rlambda[x] <- lambda * exp(rnorm(1, mu, sigma)) * N[t] :
#number of items to replace is not a multiple of replacement length" and i tried to use the key but it still won't work so I 
#am giving up for now
  
#A.2 
#I copied the key from the first part to try to see if I can modify that code to get 
#code from this part to work

# PART A. STOCHASTIC DENSITY-INDEPENDENT GROWTH
###############################################
lambda <- 1.01  # this line sets the variable "lambda" equal to 2
TimeSteps <- 250 # sets the variable "TimeSteps" equal to 10
t <- c(1:TimeSteps) # creates a vector from 1 to the number of timesteps
N <- numeric() # creates an empty numeric vector
N[1] <- 1

Col<-as.vector(col2rgb('black')/255) # to convert a named colour into its code
Col<-rgb(Col[1],Col[2],Col[3], alpha=0.2) # to convert the color into a semi-transparent colour; alpha sets the level of transparency

# set stochastic parameters
mu<-0
sigma<-0.2

# Run individual realization
rlambda<-numeric() # to store stochasticlambda values
for (Time in t){
  rlambda[Time]<-lambda*exp(rnorm(1,mu,sigma))
  N[Time+1] <- N[Time]*rlambda[Time]
}
plot(N,log='y',ylim=c(1,10^20))

mean.rlambda<-mean(rlambda);mean.rlambda
# ANSWER: arithmetic mean is > given lambda

#################
# Run and plot all realizations
rlambda<-array(NA, dim=c(reps, length(t)) # to store stochastic lambda values in an array
N<-array(NA,dim=c(reps,(length(t)+1))) # to store N in an array
N[,1]<-1 #adding N(0) in the colummn
reps<-150 # specify number of replicate realizations
#plot(N,type='n',log='y',ylim=c(1,10^20)) #  to initialize plot
for (r in 1:reps){
  for (Time in t){
    rlambda[Time]<-lambda*exp(rnorm(1,mu,sigma))
    N[Time+1] <- N[Time]*rlambda[Time]
  }
}
head(N)

#plot array of N values from all reps

plot(N[1,],log='y',ylim=c(1,10^20),type='n')
for (r in 1:nrow(N)){
  lines(N[r,], col=Col)
}

?lines

A.3
gmean <- function(x){ exp ( mean (log ( x ) ) ) }

gmean(rlambda)
#mean of lambdas from all reps is 1.0103 - it is very close to my specified lambda of 1.01

#Add the following to your plot of stochastic realizations using lines(): (a) the population size
#over time expected if the deterministic value of λ were equal to the mean observed arithmetic λ, and
#(b) the population size over time expected if the deterministic value of λ were equal to the mean
#observed geometric λ. (Set lwd=3 and col=‘red’ or ‘green’ to make these lines more visible).

plot(N[1,],log='y',ylim=c(1,10^20),type='n')
for (r in 1:nrow(N)){
  lines(N[r,], col=Col)
}