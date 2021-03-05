
rm(list=ls())
library(tidyverse)

#########################################################################
#### Sample Size Simulation - Using thermometers for early detection ####
#########################################################################

# Motivation: We would like to design a study where we use a thermometer to detect the early signs of an infection.
# For example, suppose we want to actively monitor for Surgical-Site Infections (SSIs). We might send patients home
# with a thermometer and ask the to text (or remotely send) their temperature readings. We could then try to intervene
# quicker on the patients where we think a fever was detected.

#### NOTE....THIS IS NOT REAL DATA. This is based on actual termpature data...but the actual data is restricted use. However,
#### this data was randomly generated to behave like the actual temperature data.

# load in temperature taking episodes
load("data/temperature_episodes.RData")

# summarize the datasets
febrile_episodes
febrile_episodes %>% summary()      # These are normal temperatures

non_febrile_episodes
non_febrile_episodes %>% summary()  # These are illness temperatures where a fever was recorded during an episode

# Plot histogram of temperatures by episode type
bind_rows(febrile_episodes,
          non_febrile_episodes) %>% 
  ggplot(aes(tempF)) +
  geom_histogram(bins=80) +
  geom_vline(aes(xintercept=100),color="red") +
  facet_wrap(~fever_episode,ncol = 1,scales = "free_y")

# simulate a very simple study where a fraction of individuals are febrile
# and the the others are non-febrile. We want to detect a difference between these two
# groups...why might this be useful?

# sample size is the overall study size, frac_fever is the fraction of patients who
# are febrile. For simplicity let's assume we only get one temperature reading from each
# person, and we use a simple t-test to compute the difference. Note: this is an overly simplistic
# and unrealistic setup. The febrile indicator would only be known once the patient registers a fever.
# Our goal was to detect a fever once a febrile episode begins....which takes a slight modification
# of the following setup. But this is a very close approximation for demonstration purposes.

# This is a function to simulate p-values from a simple t-test given a sample size and fraction of patients
# that are febrile. This function works by taking inputs (a) total study sample size and (b) percentage of
# these study participants we expect to be febrile. It then randomly draws a corresponding number of readings
# for each group, from the observed data above, finally it performs a statistical test to determine if there is a 
# difference between the groups. This tells us if we would be able to detect a difference in the observed readings.
sample_sim <- function(sample_size,frac_fever=.1){
  
  n_fever <- round(sample_size*frac_fever,0)
  
  n_non_fever <- sample_size-n_fever
  
  sim_set <- bind_rows(febrile_episodes %>%    # draw readings from the febrile group
                         sample_n(n_fever) %>% 
                         select(tempF,fever_episode),
                       non_febrile_episodes %>%        # draw readings from the non-febrile group
                         sample_n(n_non_fever) %>%
                         select(tempF,fever_episode))
  
  # perform a simple t-test to test for a difference between the groups
  temp <- sim_set %>% 
    t.test(tempF~fever_episode,data=.)
  
  # return results with the p-value, corresponding means, number of fevers, and the number of patients who
  # actually would have had a fever.
  res <- tibble(p_val=temp$p.value,
                mean1=temp$estimate[1],
                mean2=temp$estimate[2],
                n_fever_pos=sum(sim_set$tempF>100),
                n_fever_draw=n_fever)
  return(res)
}

# Test how the function works for sample size 100
sample_sim(100,frac_fever = 0.1)


# We want to repeat this procedure multiple times...this is a function to run multiple trials,
# given inputs of the sample size, fraction of patients febrile and number of trials to run
multi_sim <- function(sample_size,frac_fever=.1,trials=100){
  set.seed(1234)
  tibble(trial=1:trials) %>% 
    mutate(data=map(trial,~sample_sim(sample_size = sample_size,
                                      frac_fever = frac_fever))) %>% 
    unnest()
}

# Test the function to run multiple simulations
results <- multi_sim(sample_size=100,trials=100) 

results %>% 
  summarise(power=sum(p_val<0.005)/n()) # compute power

# Finally, we can run this function across multiple different sample sizes to determine, the sample size
# necessary. Here we are looping over sample sizes from 20 to 300 (in increments of 15). For each sample size,
# we are then running 200 trials.
sim_res <- tibble(sample_size=seq(20,300,by=15)) %>% 
  mutate(data=map(sample_size,~multi_sim(sample_size=.,trials=200)))

# Now we can compute the power for different sample sizes and plot the results
sim_res %>%  
  unnest() %>% 
  group_by(sample_size) %>% 
  summarise(power=sum(p_val<0.05)/n()) %>% 
  ggplot(aes(sample_size,power)) +
  geom_point() +
  geom_smooth()

# We could then find the sample size corresponding to a given power...such as 80%
sim_res %>%  
  unnest() %>% 
  group_by(sample_size) %>% 
  summarise(power=sum(p_val<0.05)/n()) %>% 
  ggplot(aes(sample_size,power)) +
  geom_point() +
  geom_smooth() +
  geom_hline(aes(yintercept = 0.8),color="red")



### A slight modification...Filtering to temperatures before reaching 100F...i.e. early detection ####

# for a slightly more appropriate comparison comment the following:

# bind_rows(febrile_episodes,
#           non_febrile_episodes) %>%
#   filter(tempF<=100) %>%
#   ggplot(aes(tempF)) +
#   geom_histogram(bins=80) +
#   geom_vline(aes(xintercept=100),color="red") +
#   facet_wrap(~fever_episode,ncol = 1,scales = "free_y")
# 
# febrile_episodes <- febrile_episodes %>% filter(tempF<=100)
# 
# sim_res2 <- tibble(sample_size=seq(20,300,by=15)) %>% 
#   mutate(data=map(sample_size,~multi_sim(sample_size=.,trials=200)))

# sim_res2 %>% 
#   unnest() %>% 
#   group_by(sample_size) %>% 
#   summarise(power=sum(p_val<0.05)/n()) %>% 
#   ggplot(aes(sample_size,power)) +
#   geom_point() +
#   geom_smooth() +
#   geom_hline(aes(yintercept = 0.8),color="red")
