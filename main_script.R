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
pkgs <-c("tidyverse", "lubridate", "rstan", "ggpubr", "ggsci", "cowplot", "BelgiumMaps.StatBel", "sf", "openxlsx")
lapply(pkgs, require, character.only = TRUE)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          LOAD THE DATA ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## population_table
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

## Recode provinces
POP$province <- fct_recode(factor(POP$province),
                                     Bru = "Brussels", A = "Provincie Antwerpen",
                                     H = "Provincie Henegouwen", L = "Provincie Limburg",
                                     Luik = "Provincie Luik", Lux = "Provincie Luxemburg",
                                     Na = "Provincie Namen", OV = "Provincie Oost-Vlaanderen",
                                     VB = "Provincie Vlaams-Brabant", BrW = "Provincie Waals-Brabant",
                                     WV = "Provincie West-Vlaanderen")

## recode to integers
POP.table <- POP %>%
  mutate(province = as.character(province)) %>%
  arrange(age.groups, sex, province) %>%
  mutate(across(c(age.groups, sex, province), as_factor))

# Create an array using contents of ps_reordered to pass to Stan
P <- array(data = pop2020.table$pop, 
           dim=head(as.numeric(lapply(pop2020.table, function(x) length(unique(x)))),-1), 
           dimnames=lapply(pop2020.table[,1:(length(pop2020.table)-1)], unique))

# check that P was assigned correctly by looking at the value of N for a random row
dim(P)
P[1, 1, 1]

## Load the data
alldat <- read_excel("../../3_Data_analysis/Alltimepoints_newweights.xlsx")

## Recode provinces
alldat$Province <- fct_recode(factor(alldat$Province),
                              Bru = "Brussels Capital Region", A = "Antwerp",
                              H = "Hainaut", L = "Limburg",
                              Luik = "Liège", Lux = "Luxembourg",
                              Na = "Namur", OV = "East Flanders",
                              VB = "Flemish Brabant", Brw = "Walloon Brabant",
                              WV = "West Flanders")

alldat$agecat10 <- cut(alldat$Age, breaks=c(18, 35, 55, 76),right=F)

## recode to integers
alldat <- alldat %>%
  select(age.groups = agecat10, sex = Gender, province = Province, y = Wantai, Series) %>%
  arrange(age.groups, sex, province) %>%
  mutate_at(c("age.groups", "sex", "province"), as.factor) %>%
  mutate_at(c("age.groups", "sex", "province"), as.integer)

alldat$Series <- as.integer(alldat$Series)

## take data from period 1
example <- alldat %>%
  filter(Series == "20")

## load population data
pop2020 <- read_excel("../../1_Data/5_popdata/TF_SOC_POP_STRUCT_2020.xlsx")
pop2020 <- pop2020[, c("CD_AGE", "MS_POPULATION", "CD_SEX",
                       "TX_RGN_DESCR_NL", "TX_PROV_DESCR_NL")]
pop2020 <- pop2020 %>%
  dplyr::rename(age = CD_AGE , population = MS_POPULATION,
                sex = CD_SEX, region = TX_RGN_DESCR_NL,
                province = TX_PROV_DESCR_NL)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          POPULATION STRATA
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Import Brussels into province-variable
pop2020$province <- ifelse(is.na(pop2020$province) & pop2020$region == "Brussels Hoofdstedelijk Gewest",
                           "Brussels", pop2020$province)

## Create strata based on Province, Age, and gender
pop2020$age.groups <- cut(pop2020$age,breaks=c(18, 35, 55, 75),right=F)

pop2020.table <- pop2020 %>%
  group_by(age.groups, sex, province) %>%
  filter(!is.na(age.groups)) %>%
  summarise(pop = sum(population))

## Recode provinces
pop2020.table$province <- fct_recode(factor(pop2020.table$province),
                                     Bru = "Brussels", A = "Provincie Antwerpen",
                                     H = "Provincie Henegouwen", L = "Provincie Limburg",
                                     Luik = "Provincie Luik", Lux = "Provincie Luxemburg",
                                     Na = "Provincie Namen", OV = "Provincie Oost-Vlaanderen",
                                     VB = "Provincie Vlaams-Brabant", BrW = "Provincie Waals-Brabant",
                                     WV = "Provincie West-Vlaanderen")

## recode to integers
pop2020.table <- pop2020.table %>%
  mutate(province = as.character(province)) %>%
  arrange(age.groups, sex, province) %>%
  mutate_at(c("age.groups", "sex", "province"), as.factor) %>%
  mutate_at(c("age.groups", "sex", "province"), as.integer)

# Create an array using contents of ps_reordered to pass to Stan
P <- array(data = pop2020.table$pop, 
           dim=head(as.numeric(lapply(pop2020.table, function(x) length(unique(x)))),-1), 
           dimnames=lapply(pop2020.table[,1:(length(pop2020.table)-1)], unique))

# check that P was assigned correctly by looking at the value of N for a random row
dim(P)
P[1, 1, 1]

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                          SAMPLE STRATA
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##            EASY MODEL (NOT ACCOUNTED FOR SE and SP)
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## easy stan model
rstan_dat <- list(N = nrow(example),
                  y = example$y,
                  age = example$age.groups,
                  prov = example$province,
                  sex = example$sex,
                  ng_age = max(example$age.groups),
                  ng_sex = max(example$sex),
                  ng_prov = max(example$province),
                  P = P)

## optimal use of # cores
options(mc.cores = parallel::detectCores())

## fit the model
fit <- stan(file = 'stan_1.stan', data = rstan_dat)

## check the model
traceplot(fit, pars = c("phi"))
plot(fit)

## extract posteriors
fit_mcmc <- extract(fit) 
mean(fit_mcmc$phi)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##            ADJUSTED MODEL (ACCOUNTED FOR SE and SP)
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## sensitivity and specificity
study_A <- c(25,0,2,82)
study_B <- c(78,0,2,300)
study_C <- c(127,4,27,667)

alldata <- rbind(study_B, study_B, study_C)
colnames(alldata) <- c("TP", "FP", "FN", "TN")
tot_res <- colSums(alldata)

## adjusted stan model
rstan_dat <- list(N = nrow(example),
                  y = example$y,
                  age = example$age.groups,
                  prov = example$province,
                  sex = example$sex,
                  ng_age = max(example$age.groups),
                  ng_sex = max(example$sex),
                  ng_prov = max(example$province),
                  P = P,
                  tp = tot_res["TP"],
                  tn = tot_res["TN"],
                  fp = tot_res["FP"],
                  fn = tot_res["FN"])

## optimal use of # cores
options(mc.cores = parallel::detectCores())

## fit the model
fit2 <- stan(file = 'stan_2.stan', 
             data = rstan_dat,
             chains = 5,
             iter = 10000, 
             control=list(adapt_delta=0.99))

## check the model
traceplot(fit2, pars = c("phi"))
bayesplot::rhat(fit2)
plot(fit2)
rstan::check_divergences(fit2)

## extract posteriors
fit_mcmc_2 <- extract(fit2) 
mean(fit_mcmc_2$phi)

## PREDICT GROUPS
## Extract posteriod distributions
beta_age_post <- fit_mcmc_2$beta_age
beta_sex_post <- fit_mcmc_2$beta_sex
beta_prov_post <- fit_mcmc_2$beta_prov

## Function for simulating y based on new x for each group
group_res <- list()

## loop over all groups
for (i in 1:rstan_dat$ng_age) {
  for (j in 1:rstan_dat$ng_sex) {
    for (k in 1:rstan_dat$ng_prov) {
      
      ## generate for each option a posterior prediction of seroprevalence
      lin_comb <- sample(beta_age_post[,i]) + sample(beta_sex_post[,j]) + sample(beta_prov_post[,k])
      prob <- 1/(1 + exp(-lin_comb))
      
      ## save result
      group_res[[paste0(i, "_", j, "_", k)]] <- data.frame(seroprev = prob, 
                                                           age = i,
                                                           sex = j,
                                                           prov = k)
      
    }
  }
}

group_res <- bind_rows(group_res, .id = "strata")

## ESTIMATE RR
source("RR_est.R")
est_all <- est_marginal(pop2020.table = pop2020.table, rstan_dat = rstan_dat)
















































## What to search for
ptrn_1 <- "seroprot"
ptrn_2 <- "seroprev"

## loop over all results
list.of.seroprot <- list.files(path = "../", pattern = ptrn_1)
list.of.seroprev <- list.files(path = "../", pattern = ptrn_2)

## save rate and CI
seroprot_res <- list()
seroprev_res <- list()

## SEROPROT
for (f in list.of.seroprot) {
  print(paste0("Loading ", f))
  tic("Load the data")
  ## load the file
  f_tmp <- load(file = paste0("../",f))
  toc()
  
  tic("Extract data and diagnostics")
  ## extract timepoint
  time <- str_split(str_split(f, pattern = "_", simplify = TRUE)[2], pattern = "\\.", simplify = TRUE)[1]
  
  ## Extract estimates for seroprot en seroprev
  stan_samples <- extract(seroprot$adjusted)
  
  ## Check diagnosis
  ### RHATS
  rhats <- rhat(seroprot$adjusted, pars = c("beta_age", "beta_sex", "beta_prov", 
                                            "sigma_age", "sigma_prov", "sigma_sex",
                                            "phi"))
  ### ESS (Effective sample size)
  ess <- neff_ratio(seroprot$adjusted, pars = c("beta_age", "beta_sex", "beta_prov", 
                                                "sigma_age", "sigma_prov", "sigma_sex",
                                                "phi"))
  
  ## save diagnostics
  diagnostics <- list(rhats, ess)

  ## Extract
  est <- tibble(median = median(stan_samples$phi),
            hdill = hdi(stan_samples$phi, credMass = 0.95)[1],
            hdiul = hdi(stan_samples$phi, credMass = 0.95)[2])
  
  ## save as tmp
  sex <- seroprot[["est_all"]][["sex"]]
  age <- seroprot[["est_all"]][["age"]]
  prov <- seroprot[["est_all"]][["prov"]]
  
  seroprot_res[[time]] <- list("diagnostics" = diagnostics, "est" = est, 
                               "sex" = sex, "age" = age, "prov" =  prov)
  
  toc()
  
  rm(list = f_tmp)

}

## save the results
save(file = "../blooddonor_seroprot.Rdata", list = c("seroprot_res"))

## SEROPREV
for (f in list.of.seroprev) {
  print(paste0("Loading ", f))
  tic("Load the data")
  ## load the file
  f_tmp <- load(file = paste0("../",f))
  toc()
  
  tic("Extract data and diagnostics")
  ## extract timepoint
  time <- str_split(str_split(f, pattern = "_", simplify = TRUE)[2], pattern = "\\.", simplify = TRUE)[1]
  
  ## Extract estimates for seroprot en seroprev
  stan_samples <- extract(seroprev$adjusted)
  
  ## Check diagnosis
  ### RHATS
  rhats <- rhat(seroprev$adjusted, pars = c("beta_age", "beta_sex", "beta_prov", 
                                            "sigma_age", "sigma_prov", "sigma_sex",
                                            "phi"))
  ### ESS (Effective sample size)
  ess <- neff_ratio(seroprev$adjusted, pars = c("beta_age", "beta_sex", "beta_prov", 
                                                "sigma_age", "sigma_prov", "sigma_sex",
                                                "phi"))
  
  ## save diagnostics
  diagnostics <- list(rhats, ess)
  
  ## Extract
  est <- tibble(median = median(stan_samples$phi),
                hdill = hdi(stan_samples$phi, credMass = 0.95)[1],
                hdiul = hdi(stan_samples$phi, credMass = 0.95)[2])
  
  ## save as tmp
  sex <- seroprev[["est_all"]][["sex"]]
  age <- seroprev[["est_all"]][["age"]]
  prov <- seroprev[["est_all"]][["prov"]]
  
  seroprev_res[[time]] <- list("diagnostics" = diagnostics, "est" = est, 
                               "sex" = sex, "age" = age, "prov" =  prov)
  
  toc()
  
  rm(list = f_tmp)
  
}

## save the results
save(file = "../blooddonor_seroprev.Rdata", list = c("seroprev_res"))

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


