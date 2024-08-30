##############################################
##  DETERMINISTIC MODEL                      #
##  Author: SIMID TEAM                       #
##  Last update: 01/08/2024                  #
##############################################


# PART 1 - DETERMINISTIC SIR MODEL
# ===========================================
# Loading packages
library(deSolve)    #packages containing functions to solve the model
library(ggplot2)    #for plotting purpose
library(reshape2)   #for data transforming
library(tidyverse)  #for data manipulation

#---------------------------------------------------------
# Specify the parameters
nu <-                   # recovery rate 
mu <-                   # mortality rate (here we specify it is zero, which means without demography)
R0 <-                   # basic reproduction number
beta <-                 # transmission rate

parameters <- 

#---------------------------------------------------------
# Set the initial states
N <- 10^6                             # Total population

# Model by number
# write your codes here





# Model by proportions
# write your codes here





#---------------------------------------------------------
# Define the time horizon
# write your codes here



#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameter, and times
# We can start specifying our model function:

# Model by numbers
# write your codes here








# Model by proportions
# write your codes here








#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
# write your codes here
model_out <- 

  
  
  
  
  
  
# View the results (dataframe)
# View(model_out)


#---------------------------------------------------------
# Plot the results by number
# write your codes here







# Plot the results by proportions
# write your codes here







# Calculate the force of infection









#---------------------------------------------------------
# Answer the questions
# What is the proportion of the population have recovered at the end of the observed period?


# At what time point that we observe the peak of the epidemic?


# At that specific time point, what is the maximum proportion of the population have infected?



#---------------------------------------------------------
# EXTRA (OPTIONAL)
#---------------------------------------------------------
# What would happen if:
# We keep the recovery rate (i.e., parameter nu) as the same as specified previously (i.e., nu = 1/7)
# but vary the R0 from 1 to 6?

