
# build the misclassified cohort...this function implements all the steps
# described in misclass_fdr_setup.R
draw_misclass_cohort <- function(n_miss){
  
  # Draw misclassified cases from the cf population
  sim_mis <- cf_cases %>% 
    sample_n(n_miss) %>%
    select(strata,id)
  
  # Draw a control cohort (i.e., non-carriers) - based on 1:5 match
  sim_controls <- non_carriers %>% 
    mutate(sim_controls=map2(non_carriers,num_cases,
                             ~sample_n(tbl = .x,
                                       size = 5*.y,
                                       replace = TRUE))) %>% 
    select(strata,num_cases,sim_controls) %>% 
    unnest() %>% 
    select(strata,id)
  
  # Draw case cohort (i.e., carriers) - first we need to subtract the misclassified 
  # cases from the match number in each strata
  
  # First count the number of misclassified cases that were drawn in each strata
  strata_mis_count <- sim_mis %>% count(strata)
  
  # subtract the number of misclassidied cases in each strata from 
  sim_cases <- non_carriers %>%
    left_join(strata_mis_count,by="strata") %>%
    mutate(n=ifelse(is.na(n),0L,n)) %>%
    mutate(num_cases=ifelse(num_cases-n<0,0,num_cases-n)) %>%
    mutate(sim_controls=map2(non_carriers,num_cases,
                             ~sample_n(tbl = .x,
                                       size = .y,
                                       replace = TRUE))) %>% 
    select(strata,num_cases,sim_controls) %>% 
    unnest() %>% 
    select(strata,id)
  
  # add back in the misclassified cases
  sim_cases <- bind_rows(sim_cases,sim_mis)
  
  # aggregate final cohort
  sim_cohort <- bind_rows(sim_controls %>% mutate(carrier=0L),
                          sim_cases %>% mutate(carrier=1L))
  
  
  sim_inds <- sim_cohort %>% inner_join(dx_indicators,by="id")
  
  return(sim_inds)
  
}

# Compute standard odds ratios...this function manually computes the simple
# odds ratios for each condition
compute_odds_ratios <- function(data){
  data %>% 
    summarise_at(vars(-id,-carrier,-strata),funs((sum(carrier==1 & .==1)/
                                                    (case_count-sum(carrier==1 & .==1)))/
                                                   (sum(carrier==0 & .==1)/
                                                      (control_count-sum(carrier==0 & .==1)))))
}

# Get paired odds ratios using conditional logistic regression...this function 
# computes the paired odds ratio using conditional logistic regression to account
# for stratification from matching...note the details of this model/function are 
# beyond the scope of this course...but notes are provided.
get_paired_or <- function(data){
  
  # a temporary function to run the conditional logit model for a given condition
  temp_func <- function(var_name) {
    # rename the variable in the dataset to "cond_ind" for running model below
    in_data <- data %>% 
      rename(cond_ind := !!var_name)
    
    # run conditional logistic regression model...not tryCatch will retun NA in the case
    # that the model breaks (e.g., we draw no outcome cases)
    temp_mod <- tryCatch(survival::clogit(cond_ind~carrier + strata(strata), data=in_data),
                         warning = function(w) {NA},
                         error = function(e) {NA})
    
    # return model odds ratio...again tryCatch helps avoid errors
    mod_or <- tryCatch(exp(coef(temp_mod)[[1]]),
                       warning = function(w) {NA},
                       error = function(e) {NA})
    
    # return model p-value
    mod_or_p <- tryCatch(coef(summary(temp_mod))[,5],
                         warning = function(w) {NA},
                         error = function(e) {NA})
    
    # retun the paired odds ratio and the corresponding p_val
    out <- tibble(or=mod_or,
                  p_val=mod_or_p)
    
    return(out)
  }
  
  # loop over the different conditions 
  res <- original_results %>% 
    select(name) %>% 
    mutate(est=map(name,~temp_func(.))) %>% 
    unnest()
  
  return(res)
}

