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
nu <- 1/7                  # recovery rate 
mu <- 0/80                 # mortality rate (here we specify it is zero, which means without demography)
R0 <- 2                    # basic reproduction number
beta <- R0*(nu+mu)         # transmission rate

parameters <- c(mu = mu, beta = beta, nu = nu)

#---------------------------------------------------------
# Set the initial states
N <- 10^6                             # Total population

# Model by number
initial_states <- c(S = N-1,          # number of susceptibles
                    I = 1,            # number of infected persons
                    R = 0             # number of recovered
)

# Model by proportions
initial_states <- c(s = (N-1)/N,      # proportion of susceptibles
                    i = 1/N,          # proportion of infected persons
                    r = 0             # proportion of recovered
)

#---------------------------------------------------------
# Define the time horizon
times <- seq(0, 365*2, by = 1)          # 2-year period


#---------------------------------------------------------
# Now we have the model input, including the initial_states, parameters, and times
# We can start specifying our model function:

# Model by numbers
sir_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         dS <- mu*N - beta*S*I/N - mu*S
         dI <- beta*S*I/N - nu*I - mu*I
         dR <- nu*I - mu*R
         
         return(list(c(dS, dI, dR)))     # our output
       })
}

# Model by proportions
sir_model <- function(times, state, parameters){
  with(as.list(c(state, parameters)),
       {
         # Differential equations
         ds <- mu - beta*s*i - mu*s
         di <- beta*s*i - nu*i - mu*i
         dr <- nu*i - mu*r
         
         return(list(c(ds, di, dr)))     # our output
       })
}


#---------------------------------------------------------
# Solve our model using  the "ode" function (deSolve package)
model_out <- as.data.frame(ode(y = initial_states,
                               times = times, 
                               func = sir_model,
                               parms = parameters))

# View the results (dataframe)
# View(model_out)


#---------------------------------------------------------
# Plot the results by proportions
# Phase plots
ggplot(data = model_out) + 
  geom_point(aes(x = S, y = I)) + 
  xlab("number of susceptible") + 
  ylab("number of infected") +
  theme_minimal()

# Incidence over time
melt(model_out, id = "time") %>%
  rename("compartment" = "variable",
         "no.cases" = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = no.cases, color = compartment)) + 
  xlab("time") + 
  ylab("no.cases") +
  theme_minimal()

# Plot the results by proportions
# Phase plots
ggplot(data = model_out) + 
  geom_point(aes(x = s, y = i)) + 
  xlab("proportion of susceptible") + 
  ylab("proportion of infected") +
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


# Calculate the force of infection
i_tot <- model_out$i
prev_tot <- i_tot + model_out$r

plot(model_out$time, beta*i_tot, 
     xlab = "time", ylab = "proportion infected", 
     type = "l", ylim = c(0,1), col = "firebrick", lwd = 2)
lines(model_out$time[-1], (diff(prev_tot)/diff(times))/(1-prev_tot[-1]),
      lty = 2, col = "darkgreen", lwd = 2)
lines(model_out$time, prev_tot,
      xlab = "time", ylab = "proportion infected", 
      type = "l", ylim = c(0,1), col = "darkblue", lwd = 2)



#---------------------------------------------------------
# Answer the questions
# What is the proportion of the population have recovered at the end of the observed period?
model_out[nrow(model_out),"r"]               #Answer: 79.68%

# At what time point that we observe the peak of the epidemic?
which.max(model_out$i)                     #Answer: at day 97

# At that specific time point, what is the maximum proportion of the population have infected?
model_out[which.max(model_out$i),"i"]        #Answer: 15.34%


#---------------------------------------------------------
# EXTRA (OPTIONAL)
#---------------------------------------------------------
# What would happen if:
# We keep the recovery rate (i.e., parameter nu) as the same as specified previously (i.e., nu = 1/7)
# but vary the R0 from 1 to 6?

sir_varyingparam_func(nu=1/7, mu=0, R0=1, N=10^6, I=1, duration=730)
sir_varyingparam_func(nu=1/7, mu=0, R0=2, N=10^6, I=1, duration=730)
sir_varyingparam_func(nu=1/7, mu=0, R0=3, N=10^6, I=1, duration=730)
sir_varyingparam_func(nu=1/7, mu=0, R0=4, N=10^6, I=1, duration=730)
sir_varyingparam_func(nu=1/7, mu=0, R0=5, N=10^6, I=1, duration=730)
sir_varyingparam_func(nu=1/7, mu=0, R0=6, N=10^6, I=1, duration=730)

# Answer: When we keep the recovery rate as constant, 
# We observe that the peak of epidemic occurs earlier and higher when we increase the reproduction number, meaning that more people would have been infected.
# Also, the observed epidemic starts and then declined quickly.
# When R0=1, no epidemic is observed.


# Function for varying parameters
sir_varyingparam_func <- function(nu, mu = 0, R0, N, I = 1, duration) {
  
  # Specify parameters
  beta <- R0*(nu+mu)      # transmission rate
  parameters <- c(mu = mu, beta = beta, nu = nu)
  
  # Set the initial states
  initial_states <- c(s = (N-I)/N,      # proportion of susceptibles
                      i = I/N,          # proportion of infected persons
                      r = 0             # proportion of recovered
  )
  
  # Define the time horizon
  times <- seq(0, duration, by = 1)         
  
  # Output
  model_out <- as.data.frame(ode(y = initial_states,
                                 times = times, 
                                 func = sir_model,
                                 parms = parameters))
  
  # Ploting the incidence over time
  melt(model_out, id = "time") %>%
    rename("compartment" = "variable",
           "proportion" = "value") %>%
    ggplot() +
    geom_line(aes(x = time, y = proportion, color = compartment)) + 
    xlab("time") + 
    ylab("proportion") +
    ggtitle(paste0("SIR model with R0 = ",R0, " and recovery rate nu = ",nu)) +
    theme_minimal()
  
}

