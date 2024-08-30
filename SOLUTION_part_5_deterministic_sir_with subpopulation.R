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
nu <- 1/7          # recovery rate 
mu <- 1/80         # mortality rate 
b <- 0.05          # probability of being infected per contact made

# Contact matrix (Belgium 2006, SOCRATES tool)
c_11 <- 5.43      # mean number of contacts per day that individuals in group 1 make within its own group
c_12 <- 6         # mean number of contacts per day that individuals in group 1 make with group 2
c_21 <- 1.57      # mean number of contacts per day that individuals in group 2 make with group 1
c_22 <- 10.05     # mean number of contacts per day that individuals in group 2 make within its own group


parameters <- c(mu = mu, nu = nu, b = b, 
                c_11 = c_11, c_12 = c_12, c_21 = c_21, c_22 = c_22)

# Set the initial states
N <- 10^6                             # Total population

initial_states <- c(S1 = N*0.2 - 1,     # number of susceptibles in group 1 (represented for 20% of the total population)
                    I1 = 1,             # number of infected individuals in group 1
                    R1 = 0,             # number of recovered in group 1
                    S2 = N*0.8,         # number of susceptibles in group 2
                    I2 = 0,             # number of infected individuals in group 2
                    R2 = 0              # number of recovered in group 2
)


# Define the time horizon
times <- seq(0, 100, by = 1)         


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
model_out <- as.data.frame(ode(y = initial_states,
                               times = times, 
                               func = age_model,
                               parms = parameters))

# View the results (dataframe)
# View(model_out)

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Based on the output: Plot the results
# Phase plots
ggplot(data = model_out) + 
  geom_point(aes(x = S1, y = I1)) + 
  xlab("number of susceptible") + 
  ylab("number of infected") +
  theme_minimal()

# Incidence over time
melt(model_out, id = "time") %>%
  rename("compartment" = "variable",
         "no.cases" = "value") %>%
  # filter(compartment == c("S1", "I1", "R1")) %>%
  ggplot() +
  geom_line(aes(x = time, y = no.cases, color = compartment)) + 
  xlab("time") + 
  ylab("no.cases") +
  theme_minimal()



# Based on the output and the plot results:
# What is the proportion of the individuals in group 1 have infected?
(model_out$S1[1] - model_out$S1[nrow(model_out)])/(N*0.2)        #Answer: 67.3%

# What is the proportion of the individuals in group 2 have infected?
(model_out$S2[1] - model_out$S2[nrow(model_out)])/(N*0.8)        #Answer: 71.2%



#---------------------------------------------------------
# EXTEND TO THREE (OR MORE) GROUPS
#---------------------------------------------------------
# Contact matrix
# Import the Belgian contact matrix 2006 from the SOCRATES tool
# Three age groups: 0, 18, 60

contact_matrix <- as.matrix(read.csv("social_contact_matrix_belgium2006.csv"))

# Initial states
N <- 10^6                               # Total population
initial_states <- c(S1 = N*0.21 - 1,    # 21% of the population belong to the age group <18 years
                    S2 = N*0.57,        # 57% of the population are in the 18-60 years age group
                    S3 = N*0.22,        # 22% of the population are elderly
                    I1 = 1,              
                    I2 = 0,
                    I3 = 0,
                    R1 = 0,
                    R2 = 0,   
                    R3 = 0)


# Specify the list of parameters
nu <- 1/7          # recovery rate 
mu <- 1/80         # mortality rate 
b <- 0.05          # probability of being infected per contact made

parameters <- c(mu = mu, nu = nu, b = b,
                contact_matrix = contact_matrix)

# Define the time horizon
times <- seq(from = 0, to = 100, by = 1) 

# Model function
age_model <- function(times, state, parameters){
  with(as.list(parameters),
       {
         
         # Set the compartments by age group
         groups <- 3
         S <- state[1:groups]
         I <- state[(groups+1):(groups*2)]
         R <- state[(groups*2+1):(groups*3)]
         
         N <- S+I+R
         
         # Calculate the transmission rate
         beta <- b*contact_matrix 
         
         # Differential equations
         dS <- -(beta %*% as.matrix(I/N))*S + mu*N - mu*S
         dI <-  (beta %*% as.matrix(I/N))*S - nu*I - mu*I
         dR <-  nu*I - mu*R
         
         return(list(c(dS, dI, dR)))     #output
       })
}



#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
model_out <- as.data.frame(ode(y = initial_states,
                               times = times, 
                               func = age_model,
                               parms = parameters))

# View the results (dataframe)
# View(model_out)

#---------------------------------------------------------
# Explore the model output:
# Incidence over time
melt(model_out, id = "time") %>%
  rename("compartment" = "variable",
         "no.cases" = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = no.cases, color = compartment)) + 
  xlab("time") + 
  ylab("no.cases") +
  theme_minimal()





