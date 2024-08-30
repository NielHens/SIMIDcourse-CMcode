##############################################
##  DETERMINISTIC MODEL                      #
##  Author: SIMID TEAM                       #
##  Last update: 01/08/2024                  #
##############################################


# PART 3 - DETERMINISTIC MSEIR MODEL
# ===========================================

## Starting with a basic SIR model (with/without demography)

# Loading packages
library(deSolve)    #packages containing functions to solve the model
library(ggplot2)    #for plotting purpose
library(reshape2)   #for data transforming
library(tidyverse)  #for data manipulation

#---------------------------------------------------------
# Specify the parameters
L <- 80                               # life expectancy
mu <- 1/L                             # mortality rate 
sigma <- 1/(3/12)                     # transition rate from m --> s
kappa <- 1/(2/365)                    # transition rate from e --> i
nu <- 1/(7/365)                       # recovery rate (i.e., from i --> r)
R0 <- 2                               # basic reproduction number
beta <- R0*(kappa+mu)/kappa*(mu+nu)   # transmission rate

parameters <- c(mu = mu, sigma = sigma, beta = beta, kappa = kappa, nu = nu)

# Set the initial states
N <- 10^6                                  # Total population

##  Work with numbers
initial_states <- c(M = 1,                 # number of new-born individuals at initial t0
                    S = N-2,               # number of susceptibles at initial t0
                    E = 0,                 # number of people in the latent period
                    I = 1,                 # number of infected persons at initial t0
                    R = 0                  # number of people of recovered
)


## Work with proportions
initial_states <- c(m = mu/sigma,          # proportion of new-born individuals
                    s = 1-mu/sigma - 1/N,  # proportion of susceptibles
                    e = 0,                 # proportion of people in the latent period
                    i = 1/N,               # proportion of infected persons
                    r = 0                  # proportion of recovered
)

# Define the time horizon
times <- seq(0, 1000, by = 0.01)           # length of 1000 years


#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameters, and times
# We can start specifying our model function:

# With numbers
mseir_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         N = M+S+E+I+R
         dM = mu*N - (mu+sigma)*M
         dS = sigma*M - beta*I*S/N - mu*S
         dE = beta*I*S/N - (kappa+mu)*E
         dI = kappa*E - nu*I - mu*I
         dR = nu*I - mu*R
         
         return(list(c(dM, dS, dE, dI, dR)))     # our output
       })
}


# With proportions
mseir_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         dm <- mu - sigma*m - mu*m
         ds <- sigma*m - beta*s*i - mu*s
         de <- beta*s*i - kappa*e - mu*e
         di <- kappa*e - nu*i - mu*i
         dr <- nu*i - mu*r
         
         return(list(c(dm, ds, de, di, dr)))     # our output
       })
}


#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
mseir_out <- as.data.frame(ode(y = initial_states,
                             times = times, 
                             func = mseir_model,
                             parms = parameters))

# View the results (dataframe)
View(mseir_out)

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Based on the output: Plot the results 
# Switch the names of axes corresponding to the numbers of infected when appropriate

# Phase plots
ggplot(data = mseir_out) + 
  geom_point(aes(x = s, y = i)) + 
  xlab("proportion susceptible") + 
  ylab("proportion infected") +
  theme_minimal()

# Incidence over time
melt(mseir_out, id = "time") %>%
  rename("compartment" = "variable",
         "proportion" = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = proportion, color = compartment)) + 
  xlab("time (years)") + 
  ylab("proportion") +
  theme_minimal()



#---------------------------------------------------------
# (Optional) 
# What would happen if one switches to a resolution of days for the time scale?
# i.e., times <- seq(0, 1000, by = 1)          # length of 1000 days


# DLSODA-  At current T (=R1), MXSTEP (=I1) steps   
# taken on this call before reaching TOUT     
# In above message, I1 = 5000
# 
# In above message, R1 = 47.9149
# 
# Warning messages:
#   1: In lsoda(y, times, func, parms, ...) :
#   an excessive amount of work (> maxsteps ) was done, but integration was not successful - increase maxsteps
#   2: In lsoda(y, times, func, parms, ...) :
#   Returning early. Results are accurate, as far as they go

