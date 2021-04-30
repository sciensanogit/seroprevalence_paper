
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APPENDIX R-CODE

<!-- badges: start -->
<!-- badges: end -->

The goal of the Appendix of R-code is to provide interested readers in
more information about the modeling code.

This document contains more information on the Bayesian estimation and
statistical analysis. The main analysis script (`main_script.R`)
contains the R-code that was used to load and restructure the data. The
Bayesian model as written in stan can be consulted in the
`stan_model.stan`-file.

## Bayesian MCMC Diagnostics

### Divergence

In a first step, the number of chains that diverged where evaluated
using the `rstan::check_divergences`-function:

### Trace plots

A first diagnostic plot is the trace plot, which is a time series plot
of the Markov chains. That is, a trace plot shows the evolution of
parameter vector over the iterations of one or many Markov chains

``` r
## check the model
posterior_fit <- as.array(fit) ## save as array to plot
pl_age <- mcmc_trace(posterior_fit, regex_pars = c("age"))
pl_sex <- mcmc_trace(posterior_fit, regex_pars = c("sex"))
pl_prov <- mcmc_trace(posterior_fit, regex_pars = c("prov"))
```

For age, this resulted in the following trace plot:

``` r
pl_age
```

![](README_files/figure-gfm/trace_age-1.png)<!-- -->

For sex, this resulted in the following trace plot:

``` r
pl_sex
```

![](README_files/figure-gfm/trace_sex-1.png)<!-- -->

For province, this resulted in the following trace plot:

``` r
pl_prov
```

![](README_files/figure-gfm/trace_prov-1.png)<!-- -->

### R-hat

Another way to evaluate chain convergence to an equilibrium distribution
is to compare its behavior to other initialized chains. This is the
motivation for the potential scale reduction statistic, split-*R̂*. The
split-*R̂* statistic measures the ratio of the average variance of draws
within each chain to the variance of the pooled draws across chains; if
all chains are at equilibrium, these will be the same and split-*R̂* will
be one. If the chains have not converged to a common distribution, the
split-*R̂* statistic will be greater than one (see Gelman et al. 2013,
Stan Development Team 2018).

``` r
## RHAT STATISTIC
rhats <- rhat(fit, pars = c("beta_age", "beta_sex", "beta_prov", 
                                            "sigma_age", "sigma_prov", "sigma_sex"))
mcmc_rhat(rhats)
```

![](README_files/figure-gfm/rhat-1.png)<!-- -->

### ESS

The effective sample size is an estimate of the number of independent
draws from the posterior distribution of the estimand of interest. The
*n*<sub>*e**f**f*</sub> metric used in Stan is based on the ability of
the draws to estimate the true mean value of the parameter. Because the
draws within a Markov chain are not independent if there is
autocorrelation, the effective sample size, *n*<sub>*e**f**f*</sub>, is
usually smaller than the total sample size, N. The larger the ratio of
*n*<sub>*e**f**f*</sub> to N the better (see Gelman et al. 2013, Stan
Development Team 2018 for more details) .

``` r
## ESS STATISTIC
ess <- neff_ratio(fit, pars = c("beta_age", "beta_sex", "beta_prov", 
                                                "sigma_age", "sigma_prov", "sigma_sex",
                                                "phi"))
mcmc_neff(ess, size = 2)  
```

![](README_files/figure-gfm/ess-1.png)<!-- -->

## Bayesian Output
