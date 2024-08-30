##############################################
##  DETERMINISTIC MODEL                      #
##  Author: SIMID TEAM                       #
##  Last update: 01/08/2024                  #
##############################################


# PART 4 - DETERMINISTIC SIR MODEL WITH VACCINATION
# ===========================================
# Loading packages
library(deSolve)    #packages containing functions to solve the model
library(ggplot2)    #for plotting purpose
library(reshape2)   #for data transforming
library(tidyverse)  #for data manipulation

#---------------------------------------------------------
# Specify the parameters
nu <-              # recovery rate 
mu <-              # mortality rate 
R0 <-              # basic reproduction number
beta <-            # transmission rate
p_vac <-           # the vaccination coverage 

parameters <- 

# Set the initial states
N <- 10^6                             # Total population

# Work with numbers
# Write your codes here





# Work with proportions
# Write your codes here




# Define the time horizon





#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameter, and times
# We can start specifying our model function:

# Work with numbers
# Write your codes here






# Work with proportions
# Write your codes here







#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
model_out <- 
  
  
  
  
  

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Plot the results








