##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  main_script.R
##  Copyright (c) Robby De Pauw
##
##  Title: Code for seroprevalence paper
##  Purpose: Code
##  Author: Robby De Pauw
##  Date: 23/04/2021
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load packages
pkgs <-c("tidyverse", "lubridate", "rstan", "ggpubr", "ggsci", "cowplot", 
         "openxlsx", "tictoc", "bayesplot", "HDInterval", "flextable")
lapply(pkgs, require, character.only = TRUE)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          LOAD THE DATA ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##---------------------------------------------------------------------
## POPULATION DATA
##---------------------------------------------------------------------

## EXAMPLE OF BELGIAN POPULATION DATA
## CREATE a population table for BELGIUM
POP <- read.xlsx("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking%20naar%20woonplaats%2C%20nationaliteit%20burgelijke%20staat%20%2C%20leeftijd%20en%20geslacht/TF_SOC_POP_STRUCT_2020.xlsx")
POP <- POP %>%
  select(CD_AGE, MS_POPULATION, CD_SEX,
  TX_RGN_DESCR_NL, TX_PROV_DESCR_NL) %>%
  rename(age = CD_AGE , population = MS_POPULATION,
         sex = CD_SEX, region = TX_RGN_DESCR_NL,
         province = TX_PROV_DESCR_NL) %>%
  mutate(province = ifelse(is.na(province) & region == "Brussels Hoofdstedelijk Gewest",
                "Brussels", province),
         age.groups = cut(age,breaks=c(18, 35, 55, 75),right=F) ## create age-groups that match data-analysis
         ) %>%
  group_by(age.groups, sex, province) %>%
  filter(!is.na(age.groups)) %>%
  summarise(pop = sum(population))

## RECODE provinces
POP$province <- fct_recode(factor(POP$province),
                                     Bru = "Brussels", A = "Provincie Antwerpen",
                                     H = "Provincie Henegouwen", L = "Provincie Limburg",
                                     Luik = "Provincie Luik", Lux = "Provincie Luxemburg",
                                     Na = "Provincie Namen", OV = "Provincie Oost-Vlaanderen",
                                     VB = "Provincie Vlaams-Brabant", BrW = "Provincie Waals-Brabant",
                                     WV = "Provincie West-Vlaanderen")

## RECODE factors to integers as an input for Stan
POP.table <- POP %>%
  mutate(province = as.character(province)) %>%
  arrange(age.groups, sex, province) 

POP.table$age.groups <- POP.table$age.groups %>% as_factor() %>% as.integer()
POP.table$sex <- POP.table$sex %>% as_factor() %>% as.integer()
POP.table$province <- POP.table$province %>% as_factor() %>% as.integer()

# CREATE an array using contents of
P <- array(data = POP.table$pop, 
           dim=head(as.numeric(lapply(POP.table, function(x) length(unique(x)))),-1), 
           dimnames=lapply(POP.table[,1:(length(POP.table)-1)], unique))

# CHECK that P was assigned correctly by looking at the value of N for a random row
dim(P)
P[1, 1, 1]

##---------------------------------------------------------------------
## SEROLOGY DATA
##---------------------------------------------------------------------
## LOAD the data
df <- read_csv("example_data/example_data.csv")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          MODEL THE DATA ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## SAVE results as a list
seroprot <- list() ## Seroprotection
seroprev <- list() ## Seroprevalence

## DEFINE cut-offs for each outcome
cutoff_prev <- 1
cutoff_prot  <- 12

## CREATE outcome variables based on specified cut-offs
df$prev <- as.integer(cut(df$y, breaks = c(-Inf, cutoff_prev, Inf), right = FALSE, labels = c("0", "1")))-1
df$prot <- as.integer(cut(df$y, breaks = c(-Inf, cutoff_prot, Inf), right = FALSE, labels = c("0", "1")))-1

## LOOP over all time series
for (i in 1:max(df$series)) {
  tic("total") ## Keep track of time
  
##---------------------------------------------------------------------
## BAYESIAN ANALYSIS
##---------------------------------------------------------------------
  
  ## filter data
  sub_df <- df %>%
    filter(series %in% i)
  
  ## adjusted stan model
  rstan_dat <- list(N = nrow(sub_df), ## Number of observations
                    y = sub_df$prev, ## Outcome (prevalence or protection)
                    age = sub_df$age.groups, ## age groups
                    prov = sub_df$province, ## provinces/regions
                    sex = sub_df$sex, ## sex
                    ng_age = max(sub_df$age.groups), ## number of age groups
                    ng_prov = max(sub_df$province), ## number of regional groups
                    ng_sex = max(sub_df$sex), ## number of groups for sex
                    P = P, ## POPULATION ARRAY
                    tp = 155, ## TRUE POSITIVES
                    tn = 772, ## TRUE NEGATIVES
                    fp = 3,   ## FALSE POSITIVES
                    fn = 0)   ## FALSE NEGATIVES
  
  ## use available cores
  options(mc.cores = parallel::detectCores())
  
  ## fit the model
  fit <- stan(file = 'stan_model.stan', 
               data = rstan_dat,
               chains = 5,
               iter = 10000, 
               control=list(adapt_delta=0.99), refresh = 100)
  
  ##---------------------------------------------------------------------
  ## CALCULATE RR
  ##---------------------------------------------------------------------
  
  ## extract posteriors
  fit_mcmc <- extract(fit)

  ## PREDICT GROUPS
  ## Extract posterior distributions
  beta_age_post <- fit_mcmc$beta_age
  beta_sex_post <- fit_mcmc$beta_sex
  beta_prov_post <- fit_mcmc$beta_prov
  
  ## SOURCE AND ESTIMATE MARGINAL
  source("RR_est.R") ## script to calculate RR based on obtained output
  est_all <- est_marginal(pop.table = POP.table, rstan_dat = rstan_dat)
  
  ## Finalize timing
  toc()
}

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                    CREATE DOCUMPENT WITH OUTPUT ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rmarkdown::render("O:/1_Automation/seroprevalence_paper/README.Rmd")
