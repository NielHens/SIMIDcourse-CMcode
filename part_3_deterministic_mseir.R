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
L <-                               # life expectancy
mu <-                              # mortality rate 
sigma <-                           # transition rate from m --> s
kappa <-                           # transition rate from e --> i
nu <-                              # recovery rate (i.e., from i --> r)
R0 <-                              # basic reproduction number
beta <-                            # transmission rate

parameters <- c(mu = mu, sigma = sigma, beta = beta, kappa = kappa, nu = nu)

# Set the initial states
N <- 10^6                                  # Total population

##  Work with numbers
initial_states <- c(M = ,                 # number of new-born individuals at initial t0
                    S = ,               # number of susceptibles at initial t0
                    E = ,                 # number of people in the latent period
                    I = ,                 # number of infected persons at initial t0
                    R =                   # number of people of recovered
)


## Work with proportions
initial_states <- c(m = ,          # proportion of new-born individuals
                    s = ,  # proportion of susceptibles
                    e = ,                 # proportion of people in the latent period
                    i = ,               # proportion of infected persons
                    r =                   # proportion of recovered
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
         
         
         
         
         # our output
            
       })
}


# With proportions
mseir_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         
         
         
         # our output
         
         
             
       })
}


#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
mseir_out <- 
  
  
  

# View the results (dataframe)
# View(mseir_out)

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Based on the output: Plot the results 
# Switch the names of axes corresponding to the numbers of infected when appropriate

# Phase plots





# Incidence over time







