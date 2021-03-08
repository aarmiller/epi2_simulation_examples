rm(list = ls())

# This simulation simulates how an outbreak might play out in the state of Iowa. It
# starts by taking as an input a potential outbreak trajectory, describing the number 
# of cases expected per day. Each day it then does the following: (1) randomly assign
# cases to a particular RMCC region, (2) randomly determine which cases will be hospitalized
# (3) randomly draw a portion of cases to be attributable to a long-term care outbreak,
# (4) computes the score.

# NOTE: This is a way over-simpliefied example of the actual analysis

# start by loading necessary packages
library(tidyverse)
library(lubridate) # lubridate makes working with dates much easier

# load functions needed for analysis
source("R/functions_for_idph_matrix.R")

# load county and region level data for Iowa
load("data/ia_county_covid.RData")


## look at what data we have loaded

# county-level outbreak data
county_covid

# region-level outbreak data
region_covid

# region-level popuation data
region_pop

##########################
#### Input Parameters ####
##########################

# start by setting the input parameters that will guide the simulation

# rate at which cases are hospitalized
hosp_rate <- 0.05

# rate at which cases occur in ltc outbreaks 
# Note: we are modeling this in a very oversimplified manner
ltc_outbreak_rate <- 0.005

# start date to begin the simulation
start_date <- ymd("2020-03-15")

# end date for the simulation
end_date <- ymd("2020-07-01")

# region probabilities that define the probability that a new case
# will occur in any one of the 6 regions
region_probs <- region_covid %>% 
  filter(date=="2020-04-19") %>% 
  mutate(case_frac=cases/sum(cases)) %>% 
  .$case_frac

region_probs

######################################
#### Components of the simulation ####
######################################

#### Walk through the functions ####

# The function draw_case_regions() takes a number of cases in a given day and draws where those 
# cases are located at based on the probability values in the object "region_probs"
draw_case_regions(100)

# just a quick test that we are drawing cases in proportion to the existing case rate
draw_case_regions(1000) %>% 
  count(region) %>% 
  mutate(region_frac=n/sum(n))

# compare this to the region probabilities
region_probs

# The function draw_case_hosp() assigns each case that is drawn to being hospitalized or 
# not, based on the probability of hospitalization defined above
draw_case_regions(100) %>% 
  draw_case_hosp()

# again another check that hospitalizations are occuring according to the correct probability
draw_case_regions(100) %>% 
  draw_case_hosp() %>% 
  count(hosp_case)

# The function draw_case_regions() draw if cases is associated with a LTC outbreak 
draw_case_regions(100) %>% 
  draw_case_hosp() %>% 
  draw_ltc_outbreak()

# The function draw_cases() is a wrapped around the above function (i.e., it incorporates all 
# the steps of drawing cases, assigning hospitalizations, and assigning LTC cases). Now 
# we can use the function draw_case() to perform all these steps.
draw_cases(100) 
draw_cases(1000) 

# Why did I do the above steps in a series of functions, instead of all of them inside ?

##############################
#### Draw Cases over time ####
##############################

# We are now going to walk through the process of drawing cases for different time points.

# Lets start by defining the time periods and number of cases we would like to draw at each

# suppose we want to draw 50 per day
time_steps <- tibble(date=as_date(start_date:end_date),
       num_cases=50)

time_steps

# we can then apply the draw_cases() function to each time step
time_steps  %>% 
  mutate(data=map(num_cases,draw_cases)) %>% 
  unnest(data) 


# what we need to do now is compute the number of new cases, hospitalizations
# and ltc outbreaks at each time step, by region
time_steps %>% 
  mutate(data=map(num_cases,draw_cases)) %>% 
  unnest(data) %>% 
  group_by(date,region) %>% 
  summarise(new_cases=n(),
            hospitalizations=sum(hosp_case),
            ltc_outbreaks=sum(ltc_outbreak))


# the above steps, including drawing cases and counting by region have all been
# incorporated into the draw_outbreak() function. Using a series of time steps we can
# use this function to give us a summary of the outbreak by day and region.
time_steps %>% 
  draw_outbreak() 


# lets plot how the cases come in per day by region
time_steps %>% 
  draw_outbreak() %>% 
  group_by(region) %>% 
  arrange(region,date) %>% 
  mutate(cases=cumsum(new_cases)) %>% 
  ggplot(aes(date,new_cases)) +
  geom_line() +
  facet_wrap(~region)


#####################################
#### Load in some Epi Curve Data ####
#####################################

# what we really want to do is play out some likely outbreak scenarios that we believe may
# take place in the state of Iowa. I have created some plausible scenarios from some of my 
# other work.

# load in the scenarios
load("data/ia_outbreak_scenarios.RData")

# Plot cumulative cases for different scenarios. Note: the red dots represents observations
# that have already occurred and the black line represents a possible trajectory that may take
# place in the future.
plot_cumulative_cases(ia_outbreak_scenarios)
plot_cumulative_deaths(ia_outbreak_scenarios)


# draw an outbreak scenario using the number of daily cases from one of the scenarios
# here we draw for scenario 1
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 1") %>%  
  draw_outbreak()

# here we draw for scenario 2
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 2") %>%  
  draw_outbreak()

# Note we are re-drawing from the start of the outbreak. If we wanted to simulate the outbreak 
# from the current point we could simply filter from the current date and make a few 
# modifications to the remaining steps

##############################
#### Compute IDPH regions ####
##############################

# Now that we know how to simulate the outbreak given a hypothetical trajectory, we now
# just need to compute the RMCC region scores that would occur at each day along that 
# trajectory

# Just like before...we are going to build this up through a series of functions. I won't 
# go through all the steps, but here is one example of what needs to occur at each time point.
# the function count_14_day_incidence() counts the per-capita incidence over the previous 14 days
# given a particular date.

# compute the 14 day lag
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 1") %>%  
  draw_outbreak() %>% 
  count_14_day_incidence(count_date=Sys.Date()) 

# the function assign region score then assigns the regional scores at a given time point
# add scores to a region
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 1") %>%  
  draw_outbreak() %>% 
  assign_region_score(count_date=Sys.Date())

ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 1") %>%  
  draw_outbreak() %>% 
  assign_region_score(count_date=Sys.Date()) %>% 
  select(region,age_score:region_score)

#########################
#### Run Simulations ####
#########################

# FINALLY....

# we are now ready to use the run_single_sim() function. This function runs through all of the above
# steps and computes the region scores at each time point for a given outbreak trajectory. Note: I have
# omitted many of the steps that go into this function. You can find these steps in the function file.

# run a single simulation
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 5") %>% 
  run_single_sim()

# plot out the region scores of a single simulation
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_outbreak_region_scores()

# plot the case counts by day along with the scores
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()


####################
#### Inference #####
####################

#### QUESTION 1: What happens if we test more... we know that we will be testing the more 
# severe cases first so if we test more we will pick up less sever cases and the hospitalization 
# rate will appear to decrease and the case count will increase
plot_cumulative_cases(ia_outbreak_scenarios)

# Example 1 - Test many people and the observed hosp rate will be low but cases will be high
hosp_rate <- 0.05

ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_outbreak_region_scores()


# Example 2 - Test few people and the observed hosp rate high but cases will be low
hosp_rate <- 0.15

ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 1") %>% 
  run_single_sim() %>% 
  plot_outbreak_region_scores()

#### Question 2: When will the decision rule be triggered relative to the epidemic
hosp_rate <- 0.05

ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()

# in regions with lower cases per capita...the rule is triggered near the peak, in
# regions where there are more cases per capita it occurs at the early part of the outbreak


##### Question 3: Are we really distinguishing between regions?
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 1") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()

# It does appear to do a good job distinguishing


#### Qeustion 4: How variable are the scores...even if the outbreak is not
set.seed(1234)
p1 <- ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()

set.seed(7894)
p2 <- ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()

set.seed(453)
p3 <- ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 6") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()

p1
p2
p3

#### Question 5: How important is that LTC outbreak rate?

ltc_outbreak_rate <- 0.01
ltc_outbreak_rate <- 0.005
ltc_outbreak_rate <- 0.001

hosp_rate <- 0.10
hosp_rate <- 0.05

set.seed(1234)
ia_outbreak_scenarios %>% 
  filter(projection=="Scenario 3") %>% 
  run_single_sim() %>% 
  plot_region_scores_and_cases()

# Note how challenging it can be to reach a 10


##### NOTE: At this point if we really want to conduct a more robust analysis we should
#####       run multiple simulation trials

