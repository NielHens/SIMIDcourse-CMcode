##############################################
##  DETERMINISTIC MODEL                      #
##  Author: SIMID TEAM                       #
##  Last update: 01/08/2024                  #
##############################################


# PART 2 - SIR extensions: SIRS & SIS
# ===========================================
# Loading packages
library(deSolve)    #packages containing functions to solve the model
library(ggplot2)    #for plotting purpose
library(reshape2)   #for data transforming
library(tidyverse)  #for data manipulation

#---------------------------------------------------------
# Specify the parameters
nu <-                  # recovery rate 
mu <-                  # mortality rate 
R0 <-                  # basic reproduction number
beta <-                # transmission rate
sigma <-               # the transition rate from R compartment to S compartment

parameters_sirs <- 
parameters_sis <- 


# Set the initial states
N <- 10^6                             # Total population

# Initial states for SIRS model
# Write your codes here




# Initial states for SIS model
# Write your codes here





# Define the time horizon
# Write your codes here




#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameters, and times
# We can start specifying our model function:

# Model function for SIRS model
# Write your codes here






# Model function for SIS model
# Write your codes here





#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
sirs_out <- 
  

sis_out <- 
  
  

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Based on the output: Plot the results
# Write your codes here



