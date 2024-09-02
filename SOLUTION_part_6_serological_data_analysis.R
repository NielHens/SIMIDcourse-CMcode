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

library(ggplot2)
ggplot(aes(x = age, y = seroprevalence), data = plot_dat) + 
  geom_point(size = 0.025*size_prev)

# Muench's model: constant force of infection 
#---------------------------------------------
serodat$HAVneg = 1 - serodat$HAVres
fit.muench <- glm(HAVneg ~ -1 + age, family = binomial(link = log), 
                  data = serodat[serodat$age != 0,])
summary(fit.muench)

par(mfrow=c(1,2))
plot(age_vals, seroprev, xlab="age", ylab = "seroprevalence")
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.muench$fit[order(serodat$age[serodat$age != 0])], lwd = 2)
plot(age_vals,rep(-fit.muench$coeff,length(age_vals)),
     type="l", xlab="age", ylab="force of Infection", lwd = 2)

# Griffiths model                             
#-----------------
serodat$age1 <- serodat$age
serodat$age2 <- serodat$age^2
serodat$age3 <- serodat$age^3

fit.griffiths<-glm(HAVneg ~ -1 + age1 + age2, family = binomial(link = log), 
                   data = serodat[serodat$age != 0, ], 
                   start = c(fit.muench$coeff, 0))
summary(fit.griffiths)

foi.linear <- (-fit.griffiths$coeff[1])+ (-fit.griffiths$coeff[2])*age_vals

par(mfrow=c(1,2))
plot(age_vals, seroprev, xlab="age", ylab = "seroprevalence")
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.muench$fit[order(serodat$age[serodat$age != 0])], lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.griffiths$fit[order(serodat$age[serodat$age != 0])], 
      col = 2, lwd = 2)
plot(age_vals,rep(-fit.muench$coeff,length(age_vals)),
     type="l", xlab="age", ylab="force of Infection", lwd = 2)
lines(age_vals, foi.linear, lwd = 2, col = 2)

# Grenfell and Anderson model                      
#-----------------------------
fit.grenfell<-glm(HAVneg ~ age1 + age2 + age3, family = binomial(link = log),
                  data = serodat[serodat$age != 0,], 
                  start = c(0, fit.muench$coeff, 0, 0))
summary(fit.grenfell)

foi.flex<- (-fit.grenfell$coeff[2]) + 2*(-fit.grenfell$coeff[3])*age_vals + 
           3*(-fit.grenfell$coeff[4])*age_vals**2

par(mfrow=c(1,2))
plot(age_vals, seroprev, xlab="age", ylab = "seroprevalence")
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.muench$fit[order(serodat$age[serodat$age != 0])], lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.griffiths$fit[order(serodat$age[serodat$age != 0])], 
      col = 2, lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.grenfell$fit[order(serodat$age[serodat$age != 0])], 
      col = "blue", lwd = 2)
plot(age_vals, rep(-fit.muench$coeff,length(age_vals)),
     type="l", xlab="age", ylab="force of Infection", 
     lwd = 2, ylim = c(-0.05,0.1))
lines(age_vals, foi.linear, lwd = 2, col = 2)
lines(age_vals, foi.flex, lwd = 2, col = "blue")
abline(h = 0, col = 1, lwd = 2, lty = 2)

# Grenfell and Anderson: alternative models
#-------------------------------------------
fit.grenfell0<-glm(HAVneg ~ -1 + age1 + age2 + age3, family = binomial(link = log),
                  data = serodat[serodat$age != 0,], 
                  start = c(fit.muench$coeff, 0, 0))
summary(fit.grenfell0)

foi.flex0<- (-fit.grenfell0$coeff[1]) + 2*(-fit.grenfell0$coeff[2])*age_vals + 
  3*(-fit.grenfell0$coeff[3])*age_vals**2

fit.grenfell1<-glm(HAVneg ~ age1 + age2 + age3 + I(age**4), 
                   family = binomial(link = log),
                   data = serodat[serodat$age != 0,], 
                   start = c(0, fit.muench$coeff, 0, 0, 0))
summary(fit.grenfell1)

foi.flex1<- (-fit.grenfell1$coeff[2]) + 2*(-fit.grenfell1$coeff[3])*age_vals + 
  3*(-fit.grenfell1$coeff[4])*age_vals**2 + 4*(-fit.grenfell1$coeff[5])*age_vals**3

par(mfrow=c(1,2))
plot(age_vals, seroprev, xlab="age", ylab = "seroprevalence")
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.muench$fit[order(serodat$age[serodat$age != 0])], lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.griffiths$fit[order(serodat$age[serodat$age != 0])], 
      col = 2, lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.grenfell$fit[order(serodat$age[serodat$age != 0])], 
      col = "blue", lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.grenfell0$fit[order(serodat$age[serodat$age != 0])], 
      col = "orange", lwd = 2)
lines(sort(serodat$age[serodat$age != 0]), 
      1-fit.grenfell1$fit[order(serodat$age[serodat$age != 0])], 
      col = "purple", lwd = 2)
plot(age_vals, rep(-fit.muench$coeff,length(age_vals)),
     type="l", xlab="age", ylab="force of Infection", 
     lwd = 2, ylim = c(-0.05,0.1))
lines(age_vals, foi.linear, lwd = 2, col = 2)
lines(age_vals, foi.flex, lwd = 2, col = "blue")
lines(age_vals, foi.flex0, lwd = 2, col = "orange")
lines(age_vals, foi.flex1, lwd = 2, col = "purple")
abline(h = 0, col = 1, lwd = 2, lty = 2)

# Comparison of fit to the data
#-------------------------------
AIC(fit.muench)
AIC(fit.griffiths)
AIC(fit.grenfell)

AIC(fit.grenfell0)
AIC(fit.grenfell1)
