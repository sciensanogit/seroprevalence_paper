// BAYESIAN MODEL FOR SEROPREVALENCE AND SEROPROTECTION ESTIMATES
// AUTHOR: Robby.DePauw@Sciensano.be
// VERSION 1.0

data {
  // Specification of group sizes for age, province, and sex
  int<lower = 0> ng_age;
  int<lower = 0> ng_prov;
  int<lower = 0> ng_sex;
  
  // specification of observations and data
  int<lower = 0> N; //Number of participants
  int<lower = 0> y[N]; //Outcome (positive or negative)
  int<lower = 1, upper = ng_age> age[N]; // Age group of participant
  int<lower = 1, upper = ng_prov> prov[N]; // Province of participant
  int<lower = 1, upper = ng_sex> sex[N]; // Sex of participant
  
  // Population data
  int<lower = 0> P[ng_age, ng_sex, ng_prov]; // Population data for each catagory
  
  // Test accuracy (TP, FN, TN, FP) based on own data or literature
  int<lower=0> tp;  
  int<lower=0> fn;  
  int<lower=0> tn;  
  int<lower=0> fp; 

}

parameters {
  // Modelling parameters
  real<lower = 0> sigma_age;// SD of age coeff
  real<lower = 0> sigma_prov;// SD of prov coeff
  real<lower = 0> sigma_sex;// SD of sex coeff
  
  vector<multiplier = sigma_age>[ng_age] beta_age; //betacoeff for each age group
  vector<multiplier = sigma_prov>[ng_prov] beta_prov; //betacoeff for each province
  vector<multiplier = sigma_sex>[ng_sex] beta_sex; //betacoeff for each sex group
  
  // Accuracy estimates
  real <lower=0,upper=1> se; // Sensitivity
  real <lower=0,upper=1> sp; // Specificity
}

transformed parameters {
  vector <lower=0,upper=1>[N] pt; // True proportion
  vector <lower=0,upper=1>[N] pa; // Adjusted proportion (based on Se and Sp)

  pt = inv_logit(beta_age[age] + beta_prov[prov] + beta_sex[sex]);  
  pa = se*pt+(1-sp)*(1-pt);
}

model {
  // Modelling priors
  beta_age ~ normal(0, sigma_age);
  sigma_age ~ normal(0, 3);
  beta_prov ~ normal(0, sigma_prov);
  sigma_prov ~ normal(0, 3);
  beta_sex ~ normal(0, sigma_sex);
  sigma_sex ~ normal(0, 3);

  // Model
  target += binomial_lpmf(tp | tp+fn, se);
  target += binomial_lpmf(tn | tn+fp, sp);
  target += bernoulli_lpmf(y | pa);
}

generated quantities {
// Poststratification to correct for potential selection bias
  real expect_pos = 0;
  int total = 0;
  real<lower = 0, upper = 1> phi;
  // Loop over each group
  for (b in 1:ng_age)
    for (c in 1:ng_sex)
      for (d in 1:ng_prov) {
        total += P[b,c,d];
        expect_pos += P[b, c, d] * inv_logit(beta_age[b] + beta_sex[c] + beta_prov[d]);
             }
  phi = expect_pos / total; // post-stratified seroprevalence estimate
}
