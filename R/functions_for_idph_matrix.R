count_14_day_incidence <- function(data,count_date){
  data %>% 
    filter(between(date,count_date-14,count_date)) %>% 
    group_by(region) %>% 
    summarise(cases=sum(new_cases)) %>% 
    full_join(tibble(region=1:6),by="region") %>% 
    inner_join(region_pop,by="region") %>% 
    mutate(cases=ifelse(is.na(cases),0,cases)) %>% 
    mutate(cases_per_cap=100000*cases/population) %>% 
    select(-population)
}

# this function takes a number of cases and draws where they are located at based on the region probabilityies
draw_case_regions <- function(num_cases){
  tibble(case=1:num_cases) %>% 
    mutate(region=map_int(case,~sample(1:6,1,prob = region_probs)))
}

# this function takes a list of cases and draws if the are hospitalized
draw_case_hosp <- function(data){
  data %>% 
    mutate(hosp_case=runif(n())<hosp_rate)
}

# this function takes a list of cases and draws if the are hospitalized
draw_case_hosp <- function(data){
  data %>% 
    mutate(hosp_case=runif(n())<hosp_rate)
}

# this function takes a list of cases and draws if they are associated with a long-term care outbreak
draw_ltc_outbreak <- function(data){
  data %>% 
    mutate(ltc_outbreak=runif(n())<ltc_outbreak_rate)
}

# this function combines the above steps and draws cases, hospital and LTC outbreak status
draw_cases <- function(num_cases){
  draw_case_regions(num_cases=num_cases) %>% 
    draw_case_hosp() %>% 
    draw_ltc_outbreak() 
}

# this function draws cases for each date given a varaiable with num_cases identifying how many to draw
draw_outbreak <- function(daily_counts){
  daily_counts  %>% 
    mutate(data=map(num_cases,draw_cases)) %>% 
    unnest() %>% 
    group_by(date,region) %>% 
    summarise(new_cases=n(),
              hospitalizations=sum(hosp_case),
              ltc_outbreaks=sum(ltc_outbreak)) %>% 
    ungroup()
}

# function to assign a score to a region at a given point taking an outbreaks as the input
assign_region_score <- function(data,count_date){
  tmp1 <- count_14_day_incidence(data,count_date=count_date) %>% 
    rename(cases_14day=cases,
           cases_per_cap_14day=cases_per_cap)
  
  tmp2 <- data %>% 
    filter(date<=count_date) %>% 
    group_by(region) %>% 
    summarise(tot_cases=sum(new_cases),
              tot_hospitalizations=sum(hospitalizations),
              tot_ltc_outbreaks=sum(ltc_outbreaks)) %>% 
    mutate(hosp_rate=100*tot_hospitalizations/tot_cases) %>% 
    full_join(tibble(region=1:6),by="region") %>%
    mutate_at(vars(hosp_rate,tot_ltc_outbreaks),funs(ifelse(is.na(.),0,.)))
  
  full_join(tmp1,tmp2,by="region") %>% 
    select(region,cases_per_cap_14day,hosp_rate,ltc_outbreaks=tot_ltc_outbreaks) %>% 
    inner_join(tibble(region=1:6,
                      pct_over_65=c(15.0,21.9,18.7,19.4,16.9,17.6),
                      age_score=c(2,3,2,2,2,2)),
               by="region") %>% 
    mutate(hosp_score=as.integer(as.character(cut(hosp_rate,breaks = c(0, 3, 12, 15, 10000),labels = c(0,1,2,3),right = FALSE)))) %>% 
    mutate(case_score=as.integer(as.character(cut(cases_per_cap_14day,breaks = c(0,6,21,50,10000),labels = c(0,1,2,3),right = FALSE)))) %>% 
    mutate(ltc_score=as.integer(as.character(cut(ltc_outbreaks,breaks = c(0,1,2,3,10000),labels = c(0,1,2,3),right = FALSE)))) %>% 
    mutate(region_score=age_score+hosp_score+case_score+ltc_score)
  
}

# run a complete simulation and calculate the scores
run_single_sim <- function(data,from=start_date,to=end_date){
  
  # draw the outbreak
  tmp_outbreak <- draw_outbreak(data) %>% 
    arrange(region,date) %>% 
    group_by(region) %>% 
    mutate(tot_cases=cumsum(new_cases),
           tot_hosp=cumsum(hospitalizations),
           tot_ltc_outbreaks=cumsum(ltc_outbreaks)) %>% 
    ungroup() %>% 
    arrange(date,region)
  
  region_scores <- tibble(sim_date=as_date(from:to)) %>% 
    mutate(data=map(sim_date,~assign_region_score(data = tmp_outbreak,count_date=.))) %>% 
    unnest()
  
  return(list(outbreak=tmp_outbreak,
              region_scores=region_scores))
  
  
}

##########################
#### Plot Functions ######
##########################


plot_cumulative_deaths <- function(plot_data){
  plot_data %>% 
    ggplot(aes(date)) +
    geom_line(aes(y=pred_tot_deaths)) +
    geom_point(aes(y=tot_deaths),color="red") +
    facet_wrap(~projection)  +
    ylab("Cumulative Deaths")
}

plot_cumulative_cases <- function(plot_data){
  plot_data %>% 
    ggplot(aes(date)) +
    geom_line(aes(y=pred_tot_cases)) +
    geom_point(aes(y=tot_cases),color="red") +
    facet_wrap(~projection) +
    ylab("Cumulative Cases")
}

plot_outbreak_region_scores <- function(sim_data){
  sim_data$region_scores %>% 
    mutate(region_label=paste0("Region ",region)) %>% 
    ggplot(aes(x=sim_date)) +
    geom_line(aes(y=region_score)) +
    facet_wrap(~region_label) +
    ylab("Region Score") +
    xlab("Simulation Date") +
    theme_bw()
}

# plot the daily case counts in the simulation above the region score
plot_region_scores_and_cases <- function(sim_data){
  
  tmp_p1 <- sim_data$outbreak %>% 
    inner_join(region_pop, by="region") %>% 
    mutate(tot_cases_pc=100000*tot_cases/population,
           new_cases_pc=100000*new_cases/population) %>% 
    select(date,region,new_cases_pc)
  
  tmp_p2 <- sim_data$region_scores %>%
    select(date=sim_date,region,region_score) 
  
  inner_join(tmp_p1,tmp_p2,by=c("date","region")) %>% 
    mutate(region=paste0("Region ", region)) %>% 
    rename(`Daily Cases Per Capita`=new_cases_pc,
           `Region Score`=region_score) %>% 
    gather(key=key,value=value,-date,-region) %>%
    ggplot(aes(date,value)) +
    geom_line() +
    facet_grid(key~region,scales = "free_y") +
    theme_bw()
  
}
