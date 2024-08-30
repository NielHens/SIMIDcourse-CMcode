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
nu <- 1/7             # recovery rate 
mu <- 1/80            # mortality rate 
R0 <- 3               # basic reproduction number
beta <- R0*(nu+mu)    # transmission rate
p_vac <- 0.4          # the vaccination coverage (assuming that this vaccine has 100% efficacy)

parameters <- c(mu = mu, beta = beta, nu = nu, p_vac = p_vac)

# Set the initial states
N <- 10^6                             # Total population

# Work with numbers
initial_states <- c(S = N-1,          # Number of susceptibles
                    I = 1,            # Number of infected persons
                    R = 0             # Number of recovered
)


# Work with proportions
initial_states <- c(s = (N-1)/N,      # proportion of susceptibles
                    i = 1/N,          # proportion of infected persons
                    r = 0             # proportion of recovered
)


# Define the time horizon
times <- seq(0, 365*2, by = 1)          # length of 2 years, daily


#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameters, and times
# We can start specifying our model function:

# Work with numbers
vac_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         N = S+I+R
         # Differential equations
         dS = mu*(1-p_vac)*N - beta*I*S/N - mu*S
         dI = beta*I*S/N - nu*I - mu*I
         dR = nu*I - mu*R + mu*p_vac*N
         
         return(list(c(dS, dI, dR)))     # our output
       })
}


# Work with proportions
vac_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         ds <- (1-p_vac)*mu - beta*s*i - mu*s
         di <- beta*s*i - nu*i - mu*i
         dr <- p_vac*mu + nu*i - mu*r
         
         return(list(c(ds, di, dr)))     # our output
       })
}


#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
model_out <- as.data.frame(ode(y = initial_states,
                               times = times, 
                               func = vac_model,
                               parms = parameters))

# View the results (dataframe)
# View(model_out)

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Based on the output: Plot the results
# Switch the names of axes corresponding to the numbers of infected when appropriate

# Phase plots
ggplot(data = model_out) + 
  geom_point(aes(x = S, y = I)) + 
  xlab("proportion susceptible") + 
  ylab("proportion infected") +
  theme_minimal()

# Incidence over time
melt(model_out, id = "time") %>%
  rename("compartment" = "variable",
         "proportion" = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = proportion, color = compartment)) + 
  xlab("time") + 
  ylab("proportion") +
  theme_minimal()


# Calculate the herd immunity threshold
herd_immunity <- 1 - 1/R0 
herd_immunity 
# p_herd = 0.667, meaning that we weed a vaccine coverage of 66.7% in the population to prevent an epidemic. 
# However, we are assuming this vaccine has 100% efficacy.

# Question: If the efficacy of this vaccine is only 70%,
# what proportion of the population would have to be vaccinated to prevent an epidemic?
vac_efficacy <- 0.7              # Vaccine efficacy
herd_immunity  <- 1 - 1/R0          # herd immunity threshold

effective_coverage <- herd_immunity / vac_efficacy    # effective coverage
effective_coverage

# The effective coverage of the population would be: 95.2%
# In other words, we need at least 95.2% coverage (of the population) with a vaccine with 70% efficacy to achieve the herd immnunity threshold, and thus, to prevent the transmission.





