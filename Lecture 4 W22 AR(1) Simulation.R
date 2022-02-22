# Econ 112 Macroeconomic Data Analysis
# Lecture 4: AR(1) Simulation
# updated by Giacomo Rondina, January 2021

# this code simulates and plots data for an AR(1) process

#set length of simulation
n.sim=500

#generate vector of zeros by n.sim
AR.1 = rep(0,n.sim) 

#set variance of innovations
sigmae = 1;

#draw white noise innovations from normal

sim.innovations = rnorm(n=n.sim,m=0,sd=sigmae)

white.noise=ts(sim.innovations)

#set AR parameter value

phi = 0.75

#generate vector of zeros by n.sim for AR
AR.1 = rep(0,n.sim) 

#loop to generate time series
for(i in 2:n.sim){
  AR.1[i]= phi*AR.1[i-1] + white.noise[i]
}

#turn simulated data into time series object

AR.1<-ts(AR.1)

#plot simulated data

plot(AR.1,ylab="",xlab="time",main="Autoregressive Process of Order 1")

#plot(AR.1,ylab="",xlab="time",main="Process A")

#plot(AR.1,ylab="",xlab="time",main="Process B")

#compute autocorrelation function of simulated data

acf(AR.1, main="Autocorrelation Function of AR(1)")

#load package for time series regressions

library(dyn)

#estimate AR(1)

lm.ar.1<-dyn$lm(AR.1~lag(AR.1,-1))
summary(lm.ar.1)

#estimate AR(2) on AR(1) data

lm.ar.2<-dyn$lm(AR.1~lag(AR.1,-1)+lag(AR.1,-2))
summary(lm.ar.2)
