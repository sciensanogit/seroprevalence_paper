##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##                      ESTIMATION OF RR
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
est_marginal <- function(pop.table, rstan_dat = rstan_dat){
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      PREPARATION
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ## Marginal weights prov
  prop_prov <- pop.table %>%
    group_by(province) %>%
    summarise(N = sum(pop)) %>%
    mutate(prop = N/sum(N)) %>%
    select(prop) %>%
    as.matrix()
  
  ## Marginal weights sex
  prop_sex <- pop.table %>%
    group_by(sex) %>%
    summarise(N = sum(pop)) %>%
    mutate(prop = N/sum(N)) %>%
    select(prop) %>%
    as.matrix()
  
  ## Marginal weights age
  prop_age <- pop.table %>%
    group_by(age.groups) %>%
    summarise(N = sum(pop)) %>%
    mutate(prop = N/sum(N)) %>%
    select(prop) %>%
    as.matrix()

  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      AGE
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## ESTIMATE marginal RR for age
  age_res <- list()
  
  for (i in 1:rstan_dat$ng_age) {

    ## generate for each option a posterior prediction of seroprevalence
    lin_comb <- beta_age_post[,i] + beta_sex_post %*% prop_sex + 
      beta_prov_post %*% prop_prov
    prob <- 1/(1 + exp(-lin_comb))
    
    ## save prob for each age category
    age_res[[as.character(i)]] <- prob
      
  }
  
  age_res <- bind_rows(age_res)
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      SEX
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## ESTIMATE marginal RR for sex
  sex_res <- list()
  
  for (i in 1:rstan_dat$ng_sex) {
    
    ## generate for each option a posterior prediction of seroprevalence
    lin_comb <- beta_age_post %*% prop_age + beta_sex_post[,i] + 
      beta_prov_post %*% prop_prov
    prob <- 1/(1 + exp(-lin_comb))
    
    ## save prob for each age category
    sex_res[[as.character(i)]] <- prob
    
  }
  
  sex_res <- bind_rows(sex_res)
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##                      PROV
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## ESTIMATE marginal RR for province
  prov_res <- list()
  
  for (i in 1:rstan_dat$ng_prov) {
    
    ## generate for each option a posterior prediction of seroprevalence
    lin_comb <- beta_age_post %*% prop_age + beta_sex_post %*% prop_sex + 
      beta_prov_post[,i]
    prob <- 1/(1 + exp(-lin_comb))
    
    ## save prob for each age category
    prov_res[[as.character(i)]] <- prob
    
  }
  
  prov_res <- bind_rows(prov_res)
  
  return(list("sex" = sex_res, "age" = age_res, "prov" = prov_res))
}
