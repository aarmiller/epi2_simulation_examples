rm(list=ls())
library(tidyverse)

#### Short Monte-Carlo simulation - estimate the value of pi ####

# Suppose we wanted to derive the value for pi, but we did not know
# how to numerically. We know that the area of a circle is given by 
# pi*r^2 where r is the radius. Suppose we constructed a box around 
# circle where all four side touched the edge of the circle. The area 
# of the square would then be (2r)^2 or 4r^2. 

# Now suppose we randomly drapped points inside the squre we would expect
# roughly pi*r^2/4r^2 = pi/4 of the points to land in the circle (i.e., the
# ratio of the two areas). Thus, if we randomly drop a number of points, 
# and compute the proportion in the square p_square, we can find an estimate
# for pi as p_sqaure*4.

# Here we test that in a monte carlo simulation. We will set the radius of the
# circle to be 1 and it will be centered at (0,0). Thus, the circle will 
# intersect the sides of the square at (0,1), (1,0), (-1,0), and (0,-1). Recall
# the equation of a circle is x^2+y^2=r. Thus, we can identify any point that 
# lands in the circle if it's x and y coordinates are such that x^2+y^2<1.

# Number of random points to drop
trials <- 1000

# Build a tibble with the randompoints
random_points <- tibble(x=runif(n = trials,-1,1),      # draw random x coordinate
                        y=runif(n = trials,-1,1)) %>%  # draw random y coordiante
  mutate(dist=x^2+y^2) %>%                      # comute the distence from center
  mutate(in_circle=dist<=1)                     # identify points that fall in circle

# note what the trials set contains
random_points

# view where the random points fall
random_points %>% 
  ggplot(aes(x,y,color=in_circle)) +
  geom_point()

# compute estimate for pi
pi_est <- random_points %>% 
  summarise(pct_in=sum(in_circle)/n(),
            pi=pct_in*4)

pi_est$pi

# Notice how accuracy changes for different number of trials

random_points_2 <-tibble(num_trials=c(100,1000,10000,30000)) %>% 
  mutate(points=map(num_trials,~tibble(x=runif(n = .,-1,1),      
                                       y=runif(n = .,-1,1)) %>% 
                      mutate(dist=x^2+y^2) %>%
                      mutate(in_circle=dist<=1)))

random_points_2 %>% 
  unnest() %>% 
  group_by(num_trials) %>% 
  summarise(pi=4*sum(in_circle)/n())

# plot for different number of trials
random_points_2 %>% 
  unnest() %>% 
  ggplot(aes(x,y,color=in_circle)) +
  geom_point() +
  facet_wrap(~num_trials)
