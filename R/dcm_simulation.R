# This script provides an example that demonstrates how to use the EpiModel 
# package to conduct a deterministic compartmental model simulation. This script 
# is based on the tutorial found here: http://statnet.org/tut/BasicDCMs.html
# This link provides a more detailed description along with R code.

require(EpiModel)
rm(list=ls())

#### Set the initial conditions ####

# infection probability and acts (i.e. contacts) per unit of time
# note acts may vary in definition depending on the disease modeled (e.g., sexual intercourse for STI's
# or face-to-face contacts for airborne infectious diseases)
param <- param.dcm(inf.prob = 0.20,  # infection probability
                   act.rate = 1,     # transmissible acts per unit time (i.e., contacts)
                   rec.rate = 1/20)  # average rate of recovery with immunity (reciprocal of duration)

# initial conditions (i.e., number susceptible and number infected)
init <- init.dcm(s.num = 1000, # initial number susceptible
                 i.num = 1,    # initial number infected
                 r.num = 0)    # initial number recovered

# type of Model and number of steps to simulate
control <- control.dcm(type = "SIR", # type of model
                       nsteps = 500) # number of time steps to simulate

# compute R0
param$inf.prob*param$act.rate/param$rec.rate

#### RUN THE MODEL ####
mod <- dcm(param, init, control)

# view results
mod

# plot results
par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))
plot(mod, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "Compartment Sizes")
plot(mod, y = "si.flow", lwd = 4, col = "firebrick",
     main = "Disease Incidence", legend = "n")

# summarize results at a given time step
summary(mod, at = 50)



############################
##### Flatten the curve ####
############################

param <- param.dcm(inf.prob = 0.2,                # infection probability
                   act.rate = c(1,0.75,0.5),   # transmissible acts per unit time (i.e., contacts), this is what we "flatten"
                   rec.rate = 1/20)               # average rate of recovery with immunity (reciprocal of duration)


init <- init.dcm(s.num = 1000, # initial number susceptible
                 i.num = 1,    # initial number infected
                 r.num = 0)    # initial number recovered

control <- control.dcm(type = "SIR", # type of model
                       nsteps = 500, # number of steps to simulate
                       dt = 0.5)     # time unit for model solutions

# run model
mod <- dcm(param, init, control)

par(mfrow = c(1,2), mar = c(3.2,3,2.5,1))
plot(mod, alpha = 1, main = "Disease Prevalence")
plot(mod, y = "si.flow", col = "Greens", alpha = 0.8, main = "Disease Incidence")


#############################################
#### Add complexity with birth and death ####
#############################################

param <- param.dcm(inf.prob = 0.20,  # infection probability
                   act.rate = 1,    # transmissible acts per unit time (i.e., contacts)
                   rec.rate = 1/20, # average rate of recovery with immunity (reciprocal of duration)
                   a.rate = 1/95,   # arrival rate (in population birth rate)
                   ds.rate = 1/100, # susceptible departure rate
                   di.rate = 1/80,  # infected departure rate
                   dr.rate = 1/100) # recovered departure rate

init <- init.dcm(s.num = 1000, # initial number susceptible
                 i.num = 1,    # initial number infected
                 r.num = 0)    # initial number recovered

control <- control.dcm(type = "SIR", # type of model
                       nsteps = 1000, # number of steps to simulate
                       dt = 0.5)     # time unit for model solutions


mod <- dcm(param, init, control)

