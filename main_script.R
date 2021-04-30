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
         "openxlsx", "tictoc", "bayesplot", "HDInterval")
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
  










  
  ## save as tmp
  sex <- seroprot[["est_all"]][["sex"]]
  age <- seroprot[["est_all"]][["age"]]
  prov <- seroprot[["est_all"]][["prov"]]
  
  seroprot_res <- list("diagnostics" = diagnostics, "est" = est, 
                               "sex" = sex, "age" = age, "prov" =  prov)

## save the results
save(file = "../blooddonor_seroprot.Rdata", list = c("seroprot_res"))






























##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          ANALYSIS ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("../blooddonor_seroprev.Rdata")
load("../blooddonor_seroprot.Rdata")
load("../blooddonor_seroprev_marg.Rdata")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          TIME POINTS ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
seroprev <- list()
seroprot <- list()

for (i in 2:(length(seroprev_res)+1)) {
  
  seroprev[[as.character(i)]] <- seroprev_res[[as.character(i)]][["est"]]
  seroprot[[as.character(i)]] <- seroprot_res[[as.character(i)]][["est"]]
  
}

seroprev <- bind_rows(seroprev, .id = "time") %>%
  mutate(time = as.integer(time),
         est = "Seroprevalence") %>%
  arrange(time)
seroprot <- bind_rows(seroprot, .id = "time") %>%
  mutate(time = as.integer(time),
         est = "Seroprotection") %>%
  arrange(time)

sero <- bind_rows(seroprev, seroprot)

## add datestamp to data
sero$date <- fct_recode(factor(sero$time),"30/03/2020" = "1",
                          "14/04/2020" = "2", 
                          "27/04/2020" = "3", 
                          "11/05/2020" = "4", 
                          "25/05/2020" = "5", 
                          "08/06/2020" = "6",
                          "22/06/2020" = "7", 
                          "06/07/2020" = "8", 
                          "20/07/2020" = "9", 
                          "03/08/2020" = "10", 
                          "17/08/2020" = "11", 
                          "31/08/2020" = "12", 
                          "14/09/2020" = "13", 
                          "28/09/2020" = "14", 
                          "12/10/2020" = "15", 
                          "26/10/2020" = "16", 
                          "9/11/2020" = "17", 
                          "23/11/2020" = "18", 
                          "7/12/2020" = "19", 
                          "21/12/2020" = "20", 
                          "04/01/2021" = "21", 
                          "18/01/2021" = "22")
sero$date <- as.Date(sero$date, format = "%d/%m/%Y")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          RISK FACTORS
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Extract estimates for sex and age
sex <- list()
age <- list()
prov <- list()

for (i in 2:(length(seroprev_res)+1)) {
  ## save as tmp
  tmp_sex <- seroprev_res[[as.character(i)]][["sex"]] %>%
    mutate(est = "Seroprevalence")
  tmp_age <- seroprev_res[[as.character(i)]][["age"]] %>%
    mutate(est = "Seroprevalence")
  tmp_prov <- seroprev_res[[as.character(i)]][["prov"]] %>%
    mutate(est = "Seroprevalence")
  ## save in new list
  sex[[as.character(i)]] <- tmp_sex
  age[[as.character(i)]] <- tmp_age
  prov[[as.character(i)]] <- tmp_prov
  
  print(as.character(i))
}

## create dataframes
seroprev_sex <- sex %>%
  bind_rows(.id = "time")

seroprev_age <- age %>%
  bind_rows(.id = "time")

seroprev_prov <- prov %>%
  bind_rows(.id = "time")

## Extract estimates for sex and age
sex <- list()
age <- list()
prov <- list()

for (i in 2:(length(seroprot_res)+1)) {
  ## save as tmp
  tmp_sex <- seroprot_res[[as.character(i)]][["sex"]] %>%
    mutate(est = "Seroprotection")
  tmp_age <- seroprot_res[[as.character(i)]][["age"]] %>%
    mutate(est = "Seroprotection")
  tmp_prov <- seroprot_res[[as.character(i)]][["prov"]] %>%
    mutate(est = "Seroprotection")
  ## save in new list
  sex[[as.character(i)]] <- tmp_sex
  age[[as.character(i)]] <- tmp_age
  prov[[as.character(i)]] <- tmp_prov
  
  print(as.character(i))
}

## create dataframes
seroprot_sex <- sex %>%
  bind_rows(.id = "time")

seroprot_age <- age %>%
  bind_rows(.id = "time")

seroprot_prov <- prov %>%
  bind_rows(.id = "time")

## bind dataframes
sero_sex <- bind_rows(seroprot_sex, seroprev_sex)
sero_age <- bind_rows(seroprot_age, seroprev_age)
sero_prov <- bind_rows(seroprot_prov, seroprev_prov)

## add datestamp to data
sero_sex$date <- fct_recode(factor(sero_sex$time),"30/03/2020" = "1",
                        "14/04/2020" = "2", 
                        "27/04/2020" = "3", 
                        "11/05/2020" = "4", 
                        "25/05/2020" = "5", 
                        "08/06/2020" = "6",
                        "22/06/2020" = "7", 
                        "06/07/2020" = "8", 
                        "20/07/2020" = "9", 
                        "03/08/2020" = "10", 
                        "17/08/2020" = "11", 
                        "31/08/2020" = "12", 
                        "14/09/2020" = "13", 
                        "28/09/2020" = "14", 
                        "12/10/2020" = "15", 
                        "26/10/2020" = "16", 
                        "9/11/2020" = "17", 
                        "23/11/2020" = "18", 
                        "7/12/2020" = "19", 
                        "21/12/2020" = "20", 
                        "04/01/2021" = "21", 
                        "18/01/2021" = "22")
sero_sex$date <- as.Date(sero_sex$date, format = "%d/%m/%Y")

## add datestamp to data
sero_age$date <- fct_recode(factor(sero_age$time),"30/03/2020" = "1",
                            "14/04/2020" = "2", 
                            "27/04/2020" = "3", 
                            "11/05/2020" = "4", 
                            "25/05/2020" = "5", 
                            "08/06/2020" = "6",
                            "22/06/2020" = "7", 
                            "06/07/2020" = "8", 
                            "20/07/2020" = "9", 
                            "03/08/2020" = "10", 
                            "17/08/2020" = "11", 
                            "31/08/2020" = "12", 
                            "14/09/2020" = "13", 
                            "28/09/2020" = "14", 
                            "12/10/2020" = "15", 
                            "26/10/2020" = "16", 
                            "9/11/2020" = "17", 
                            "23/11/2020" = "18", 
                            "7/12/2020" = "19", 
                            "21/12/2020" = "20", 
                            "04/01/2021" = "21", 
                            "18/01/2021" = "22")
sero_age$date <- as.Date(sero_age$date, format = "%d/%m/%Y")

## add datestamp to data
sero_prov$date <- fct_recode(factor(sero_prov$time),"30/03/2020" = "1",
                            "14/04/2020" = "2", 
                            "27/04/2020" = "3", 
                            "11/05/2020" = "4", 
                            "25/05/2020" = "5", 
                            "08/06/2020" = "6",
                            "22/06/2020" = "7", 
                            "06/07/2020" = "8", 
                            "20/07/2020" = "9", 
                            "03/08/2020" = "10", 
                            "17/08/2020" = "11", 
                            "31/08/2020" = "12", 
                            "14/09/2020" = "13", 
                            "28/09/2020" = "14", 
                            "12/10/2020" = "15", 
                            "26/10/2020" = "16", 
                            "9/11/2020" = "17", 
                            "23/11/2020" = "18", 
                            "7/12/2020" = "19", 
                            "21/12/2020" = "20", 
                            "04/01/2021" = "21", 
                            "18/01/2021" = "22")
sero_prov$date <- as.Date(sero_prov$date, format = "%d/%m/%Y")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          ACTUAL ANALYSIS AND REPORT
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

descr <- read.xlsx("../../3_Data_analysis/Alltimepoints_newweights.xlsx")
COV.prev <- read_csv("//sas9xbi/Data2/ID/ID_DATA/covid_all_cases_nodup_hd.csv")
load("//sciensano.be/fs/1102_EPIVG_Crisis/Crise coronavirus/Surveillance/Hospitals/Surge_capacity_surveillance/02_ExtractionLimeSurvey/Final/data_for_model.RData")

dir_r <-
  paste0("//sciensano.be/FS/1102_EPIVG_Crisis/Crise coronavirus/Surveillance/Seroprevalence/Blood donors")

## mount drive
cmd <-
  sprintf('net use o: "%s"',
          gsub("/", "\\", dir_r, fixed = TRUE))
system(cmd)

## knit document
rmarkdown::render("O://1_Automation/2_Scripts/samplingblooddonor/seropaper.Rmd", output_file = "O://1_Automation/4_Reports/paper.docx", 
                  output_format = rdocx_document()
                  )


