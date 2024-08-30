##############################################
##  DETERMINISTIC MODEL                      #
##  Author: SIMID TEAM                       #
##  Last update: 01/08/2024                  #
##############################################



# PART 5 - THE BASIC SIR MODEL WITH MULTIPLE SUB-POPULATION
# ===========================================
# Loading packages
library(deSolve)    #packages containing functions to solve the model
library(ggplot2)    #for plotting purpose
library(reshape2)   #for data transforming
library(tidyverse)  #for data manipulation

#---------------------------------------------------------
# Specify the parameters
nu <-            # recovery rate 
mu <-            # mortality rate 
b <-             # probability of being infected per contact made

# Contact matrix (Belgium 2006, SOCRATES tool)

  
  
  
# List of parameters
parameters <- 
  
  

# Set the initial states
N <- 10^6                             # Total population






# Define the time horizon
       


#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameters, and times
# We can start specifying our model function:

age_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
        
         # Population per group
         N1 <- S1+I1+R1
         N2 <- S2+I2+R2 
         
         # Calculate the transmission rate
         beta_11 <- b*c_11
         beta_12 <- b*c_12
         beta_21 <- b*c_21
         beta_22 <- b*c_22
         
         # Differential equations
         dS1 <- -(beta_11*I1/N1 + beta_12*I2/N2)*S1 + mu*N1 - mu*S1
         dI1 <-  (beta_11*I1/N1 + beta_12*I2/N2)*S1 - nu*I1 - mu*I1
         dR1 <-  nu*I1 - mu*R1
         
         dS2 <- -(beta_21*I1/N1 + beta_22*I2/N2)*S2 + mu*N2 - mu*S2
         dI2 <-  (beta_21*I1/N2 + beta_22*I2/N2)*S2 - nu*I2 - mu*I2
         dR2 <-  nu*I2 - mu*R2
         
         return(list(c(dS1, dI1, dR1, dS2, dI2, dR2)))     #output
       })
}


#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
model_out <- 
  
  
  
  
  


#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Plot the results







# Based on the output and the plot results:
# What is the proportion of the individuals in group 1 have infected?



# What is the proportion of the individuals in group 2 have infected?





