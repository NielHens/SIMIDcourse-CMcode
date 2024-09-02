##############################################
##  STOCHASTIC SIR MODEL                     #
##  Author: SIMID TEAM                       #
##  Last update: 01/08/2024                  #
##############################################


#=========================================================
#---------------------------------------------------------
# STOCHASTIC SIR MODEL
#---------------------------------------------------------
# Initial states
N0 <- 1E3		                # Total population
S <- N0-1		                # Number of Susceptibles at time 0
I <- 1			                # Number of Infectious at time 0
R <- 0			                # Number of Recovered at time 0

# Specify the parameters
nu <- 1/3 		              # Recovery rate (in days)	
R0 <- 1.5  	                # Basic Reproduction number
gamma <- 0.98               # Gamma parameter
beta <- nu*R0/N0/gamma      # Transmission rate



#---------------------------------------------------------
# Stochastic SIR function (Poisson)
stocSIR_pois <- function(S, I, R, beta){
  
  # Initial states
  Svec <- Sold <- S  	# Number of Susceptibles at time t
  Ivec <- Iold <- I		# Number of Infectious at time t
  Rvec <- Rold <- R		# Number of Recovered at time t
  
  go <- TRUE;
  
  while (go){	
    Ih <- rpois(1, beta*Iold*Sold)                     #print(Ih)
    Rh <- rpois(1, nu*Iold)
    
    if ((Iold + Ih - Rh) <= 0) break
    
    Sold <- Sold - Ih
    if (Sold <= 0){Rold = N; break}
    
    Iold <- Iold + Ih - Rh
    Rold <- Rold + Rh
    
    Svec <- c(Svec, Sold)
    Ivec <- c(Ivec, Iold)
    Rvec <- c(Rvec, Rold)
    
    if (Iold == 0){go = FALSE}
  }
  return(list(Svec = Svec, Ivec = Ivec, Rvec = Rvec))
}


#---------------------------------------------------------
# Stochastic SIR function (Binomial)
stocSIR_binom <- function(S, I, R, beta, gamma){
  
  # Initial states
  Svec <- Sold <- S  	# Number of Susceptibles at time t
  Ivec <- Iold <- I		# Number of Infectious at time t
  Rvec <- Rold <- R		# Number of Recovered at time t
  
  go <- TRUE;
  
  while (go){	
    Ih <- rbinom(1, Sold, (1 - exp(-beta*(Iold)^gamma)))     #print(Ih)
    Rh <- rbinom(1, Iold, (1 - exp(-nu)))
    
    Sold <- Sold - Ih
    Iold <- Iold + Ih - Rh
    Rold <- Rold + Rh
    
    Svec <- c(Svec, Sold)
    Ivec <- c(Ivec, Iold)
    Rvec <- c(Rvec, Rold)
    
    if (Iold == 0){go = FALSE}
  }
  return(list(Svec = Svec, Ivec = Ivec, Rvec = Rvec))
}



#---------------------------------------------------------
# SIR_runs for different values of gamma and R0
last <- function(x) { return( x[length(x)] ) }

SIR_runs <- function(gamma,R0){
  # Transmission rate
  b <- g*R0/N0/gamma  
  
  nruns <- 1000
  out <- NULL
  
  for (i in 1:nruns){
    out <- c(out,last(SIR(S = 999, I = 1, R = 0, b, gamma)$Rvec))
  }
  
  #out
  plot(cumsum(out<200)/c(1:nruns), type="l", ylab="prob", xlab="runs", ylim=c(0,1))
  title(mean(out<200))
  return(list(extinction_prob = mean(out<200), final_size = out))
}


#---------------------------------------------------------
# Evaluations
gamma_grid <- rep(seq(0.80, 1, 0.01), 61)
R0_grid <- sort(rep(seq(1.0, 4.0, 0.05), 21))
sets <- cbind(gamma.grid, R0.grid)

tmp <- function(vec){SIR_runs(gamma = vec[1], R0 = vec[2])}
tmp_out <- apply(sets, 1, tmp)

# Plot the results
plot(gamma_grid, tmp_out, col = R0_grid*10)
plot(gamma_grid[R0_grid == 1.5], tmp_out[R0_grid == 1.5])
image(seq(0.80, 1, 0.01), seq(1.0, 4.0, 0.05), (matrix(tmp_out,21)))


#=========================================================
#---------------------------------------------------------
# STOCHASTIC SIRS MODEL
#---------------------------------------------------------
# Initial parameters
N0 <- 1E2		             # Total population
S <- N0-1		             # Number of Susceptibles at time 0
I <- 1			             # Number of Infectious at time 0
R <- 0			             # Number of Recovered at time 0

# Transmission parameters
R0 <- 2		               # Basic Reproduction number
nu <- 1/(7/365)		       # Recovery rate (in years)	
beta <- nu*R0/N0		     # Infection rate 
sigma <- 1		           # Rate to be susceptible again from the recovered class (in years)


#---------------------------------------------------------
# stochastic SIRS model

Svec <- Sold <- S  	# Number of Susceptibles at time t
Ivec <- Iold <- I		# Number of Infectious at time t
Rvec <- Rold <- R		# Number of Recovered at time t

go <- TRUE; total <- 0
while (go){	
  
  Ih <- rbinom(1, Sold, (1 - exp(-beta*Iold))) ; print(Ih)
  Rh <- rbinom(1, Iold, (1 - exp(-nu)))
  Sh <- rbinom(1, Rold, (1 - exp(-sigma)))
  
  Sold <- Sold - Ih + Sh
  Iold <- Iold + Ih - Rh
  Rold <- Rold + Rh - Sh
  
  Svec <- c(Svec, Sold)
  Ivec <- c(Ivec, Iold)
  Rvec <- c(Rvec, Rold)
  
  total <- total + 1
  
  if (Iold == 0 || total > 1000){go = FALSE; print(total)}
}


#---------------------------------------------------------
# Plot the results
barplot(Ivec)



