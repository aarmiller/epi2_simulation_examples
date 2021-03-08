# This script demonstrates how to use the EpiModel package to conduct a 
# individual-contact model simulation. This script is based on the tutorial 
# found here: http://statnet.org/tut/BasicICMs.html
# This link provides a more detailed description along with R code

require(EpiModel)
rm(list=ls())


#### Setup parameters for simulation
param <- param.icm(inf.prob = 0.2,   # infection probability
                   act.rate = 0.8,   # contact rate
                   rec.rate = 1/50)  # recovery rate

init <- init.icm(s.num = 500,       # initial number of individuals susceptible
                 i.num = 1,         # initial number infected
                 r.num = 0)         # initial recovered

control <- control.icm(type = "SIR", 
                       nsims = 20,   # number of simulations to perform
                       nsteps = 300) # number of steps in each simulation

mod <- icm(param, init, control)


## Plot the model
par(mfrow=c(1,1))
plot(mod)
plot(mod,sim.lines = TRUE)
# Just plot the number infected
plot(mod, y = "i.num", sim.lines = TRUE, mean.smooth = FALSE, qnts.smooth = FALSE)

## combined plot


#######################################
#### Compare DCM to ICM simulation ####
#######################################

# Note this is the exact example in the EpiModel tutorial

# Estimate a deterministic compartmental model...this one has births and deaths
param <- param.dcm(inf.prob = 0.2, 
                   act.rate = 0.8, 
                   rec.rate = 1/50,
                   a.rate = 1/100, 
                   ds.rate = 1/100, 
                   di.rate = 1/90, 
                   dr.rate = 1/100)

init <- init.dcm(s.num = 900, 
                 i.num = 100, 
                 r.num = 0)

control <- control.dcm(type = "SIR", 
                       nsteps = 300)

det <- dcm(param, init, control)

## Estimate an individual-contact model 
param <- param.icm(inf.prob = 0.2, 
                   act.rate = 0.8, 
                   rec.rate = 1/50,
                   a.rate = 1/100, 
                   ds.rate = 1/100, 
                   di.rate = 1/90,
                   dr.rate = 1/100)

init <- init.icm(s.num = 900, 
                 i.num = 100, 
                 r.num = 0)

control <- control.icm(type = "SIR", 
                       nsteps = 300, 
                       nsims = 10)

sim <- icm(param, init, control)



## Plot the different models
par(mfrow=c(1,1))
plot(det, alpha = 0.75, lwd = 4, main = "DCM and ICM Comparison")
plot(sim, qnts = FALSE, sim.lines = FALSE, add = TRUE, mean.lty = 2, legend = FALSE)

## Plot with the simulation lines
plot(det, alpha = 0.75, lwd = 4, main = "DCM and ICM Comparison")
plot(sim, qnts = FALSE, sim.lines = TRUE, add = TRUE, mean.lty = 2, legend = FALSE)


###########################
#### Flatten the Curve ####
###########################

# parameters with no social distancing
param <- param.icm(inf.prob = 0.2,   # infection probability
                   act.rate = 1.25,   # contact rate
                   rec.rate = 1/50)  # recovery rate

init <- init.icm(s.num = 500,       # initial number of individuals susceptible
                 i.num = 1,         # initial number infected
                 r.num = 0)         # initial recovered

control <- control.icm(type = "SIR", 
                       nsims = 50,   # number of simulations to perform
                       nsteps = 300) # number of steps in each simulation

mod <- icm(param, init, control)

# parameters with social distancing
param_flatten <- param.icm(inf.prob = 0.2,   # infection probability
                           act.rate = 0.5,   # contact rate (Note: this is lower)
                           rec.rate = 1/50)  # recovery rate

mod_flatten <- icm(param_flatten, init, control)

# plot both models
par(mfrow = c(1,2), mar = c(3.2,3,2.5,1))
plot(mod, y = "i.num", sim.lines=TRUE, col = "Greens", alpha = 0.8, main = "No Distancing")
plot(mod_flatten, y = "i.num", sim.lines=TRUE, col = "Greens", alpha = 0.8, main = "Social Distancing")
