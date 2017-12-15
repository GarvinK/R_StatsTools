#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
# Modeling an AR(1) process using Monte Carlo Markov Chain 
#           with  Gibbs Sampling            
#             
#             by Garvin Kruthof

#         code is not optimized yet !!! 
#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------

#install.packages("truncnorm")
#install.packages("invgamma")

require(truncnorm) #RV of truncated normal distribution
require(invgamma) #RV of inverse gamma distribution

#Function takes as an input the number of simulations (iters), the time series y, and priors of c, tau (= 1 / sigma^2), and a)
# Outputs data frame with three columns.
# 1. column = a; 2. column = 1/sigma^2, 3. column = c

gibbsAR = function(iters,y,c=0,tau=0.001,a=0.4){
  x <-array(0,c(iters+1,3))
  x[1,1] = a
  x[1,2] = 1/tau
  x[1,3] = c
  
  n = length(y)
  
  for(t in 2:(iters+1)){
    #calculating y0
    y_0 = rnorm(1,x[t-1,3]+x[t-1,1]*(y[1]-x[t-1,3]),sd=sqrt(x[t-1,2]))
    
    #calculating a 
    
    #calculating sums 
    a_mean_nom = NULL
    a_mean_denom = NULL
    #manually for the first entries, that depend on y0
    a_mean_nom[1]  = (y_0-x[t-1,3]) * (y[1]-x[t-1,3])
    a_mean_denom[1] = (y_0-x[t-1,3])^2
    for (i in 2:(length(y))){
      
      a_mean_nom[i]  = (y[i-1]-x[t-1,3]) * (y[i]-x[t-1,3])
      a_mean_denom[i] = (y[i-1]-x[t-1,3])^2
    }
    
    a_mean_nom = sum(a_mean_nom)
    a_mean_denom = sum(a_mean_denom)
    
    #store result of truncated random normal for a
    x[t,1]  = rtruncnorm(1,a=-1, b=1, mean=a_mean_nom/a_mean_denom,sd = sqrt(x[t-1,2]/a_mean_denom))

    
    #calculating sigma
    sigma_mean = NULL
    sigma_mean[1] = ((y[1]- x[t-1,3]) - x[t,1]*(y_0-x[t-1,3]))^2
    
    for (i in 2:(length(y))){
      
      sigma_mean[i] = ((y[i] - x[t-1,3]) - x[t,1]*(y[i-1]-x[t-1,3]))^2
    }
    sigma_mean = sum(sigma_mean)
    
    x[t,2] = rinvgamma(1, n/2 +1, 0.5*sigma_mean)

    #calculating c 
    c_mean = NULL
    c_mean[1] = y[1] - x[t,1]*y_0
    
    for (i in 2:(length(y))){
      
      c_mean[i] = y[i] - x[t,1]*y[i-1]
      
    }
    c_mean = sum(c_mean)
    x[t,3] = rnorm(1, (1/(n*(1-x[t,1])))*c_mean,sd= sqrt(x[t,2] / (n*(1-x[t,1])^2))) 

  }
    #converting sigma^2 back to tau
    x[,2] = 1/x[,2]
  return(x)
}


#-----------------------------------------------------------
#Example
#-----------------------------------------------------------

#comparing results with estimations using Arima()
require(forecast)
Testset = arima.sim(list(order=c(1,0,0),ar = 0.6),n=1000)
Arima(Testset, order=c(1,0,0),include.mean = TRUE) 
apply((gibbsAR(1000,Testset,a=0.6)),2,mean)
