##############################################
##  SEROLOGICAL DATA ANALYSIS                #
##  Author: SIMID TEAM                       #
##  Last update: 01/09/2024                  #
##############################################

# Serological survey data: HAV Belgium 2002
#-------------------------------------------
serodat = read.table("HAV_Belgium2002.dat", header = T)
head(serodat)

# Calculation of the age-dependent seroprevalence
#-------------------------------------------------
age_vals = sort(unique(round(serodat$age, 0)))
seroprev = tapply(serodat$HAVres, round(serodat$age, 0), mean)
size_prev = tapply(serodat$HAVres, round(serodat$age, 0), length)

# Graphical exploration of the data
#-----------------------------------
plot_dat = data.frame(age = age_vals, seroprevalence = seroprev, size = size_prev)

# Muench's model: constant force of infection 
#---------------------------------------------

# Griffiths model                             
#-----------------

# Grenfell and Anderson model                      
#-----------------------------

# Comparison of fit to the data
#-------------------------------
AIC(fit.muench)
AIC(fit.griffiths)
AIC(fit.grenfell)
    