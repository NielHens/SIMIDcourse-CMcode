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
nu <- 1/7              # recovery rate 
mu <- 1/80             # mortality rate 
R0 <- 2                # basic reproduction number
beta <- R0*(nu+mu)     # transmission rate
sigma <- 1/7           # the transition rate from R compartment to S compartment

parameters_sirs <- c(mu = mu, beta = beta, sigma = sigma)
parameters_sis <- c(mu = mu, beta = beta, nu = nu)


# Set the initial states
N <- 10^6                             # Total population

# For SIRS model
initial_states_sirs <- c(s = (N-1)/N,      # proportion of susceptibles
                         i = 1/N,          # proportion of infected persons
                         r = 0             # proportion of recovered
)

# For SIS model
initial_states_sis <- c(s = (N-1)/N,      # proportion of susceptibles
                        i = 1/N           # proportion of infected persons
)


# Define the time horizon
times <- seq(0, 365*2, by = 1)          # length of 2 years, daily interval


#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameters, and times
# We can start specifying our model function:

sirs_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         ds <- mu - beta*s*i + sigma*r - mu*s
         di <- beta*s*i - nu*i - mu*i
         dr <- nu*i - sigma*r - mu*r
         
         return(list(c(ds, di, dr)))     # our output
       })
}


sis_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         ds <- mu - beta*s*i + nu*i - mu*s
         di <- beta*s*i - nu*i - mu*i
         
         return(list(c(ds, di)))     # our output
       })
}



#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
sirs_out <- as.data.frame(ode(y = initial_states_sirs,
                              times = times, 
                              func = sirs_model,
                              parms = parameters_sirs))

sis_out <- as.data.frame(ode(y = initial_states_sis,
                             times = times, 
                             func = sis_model,
                             parms = parameters_sis))

#---------------------------------------------------------
# Explore the model output:
#---------------------------------------------------------
# Based on the output: Plot the results
# Phase plots
ggplot(data = sirs_out) + 
  geom_point(aes(x = s, y = i)) + 
  xlab("proportion susceptible") + 
  ylab("proportion infected") +
  theme_minimal()

ggplot(data = sis_out) + 
  geom_point(aes(x = s, y = i)) + 
  xlab("proportion susceptible") + 
  ylab("proportion infected") +
  theme_minimal()


# Incidence over time
melt(sirs_out, id = "time") %>%
  rename("compartment" = "variable",
         "proportion" = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = proportion, color = compartment)) + 
  labs(x="time", y="proportion", title = "SIRS model") + 
  theme_minimal()

# Incidence over time
melt(sis_out, id = "time") %>%
  rename("compartment" = "variable",
         "proportion" = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = proportion, color = compartment)) + 
  labs(x="time", y="proportion", title = "SIS model") + 
  theme_minimal()


# Calculate the force of infection
# sirs model
i_tot <- sirs_out$i
prev_tot <- i_tot + sirs_out$r
plot(sirs_out$time, beta*i_tot, xlab = "time", ylab = "proportion infected", 
     type = "l", ylim = c(0,1), col = "firebrick", lwd = 2)
lines(sirs_out$time[-1], (diff(prev_tot)/diff(times))/(1-prev_tot[-1]),
      lty = 2, col = "darkgreen", lwd = 2)
lines(sirs_out$time, prev_tot, xlab = "time", ylab = "proportion infected", 
      type = "l", ylim = c(0,1), col = "darkblue", lwd = 2)

# sis model
i_tot <- sis_out$i
prev_tot <- i_tot

plot(sis_out$time, beta*i_tot, xlab = "time", ylab = "proportion infected", 
     type = "l", ylim = c(0,1), col = "firebrick", lwd = 2)
lines(sis_out$time[-1], (diff(prev_tot)/diff(times))/(1-prev_tot[-1]),
      lty = 2, col = "darkgreen", lwd = 2)
lines(sis_out$time, prev_tot, xlab = "time", ylab = "proportion infected", 
      type = "l", ylim = c(0,1), col = "darkblue", lwd = 2)









