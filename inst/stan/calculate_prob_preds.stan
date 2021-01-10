//
// This stan program aims to build a Bayesian model for predicting
// probability of membership for Australian and International
// artists in each of the four quartiles in the Triple J Hottest 100
// Countdown
//
// NOTE: This program will eventually have the capability to take additional
// model inputs such as number of song plays on Triple J. This is slated for
// a future versioned release of hotteR.
//
// Values for the mean and SD of the nationality prior distribution were generated
// by the calculate_priors model. Units are in log odds but this should be converted
// outside of Stan for ease of interpretability.
//

//
// Author: Trent Henderson, 8 January 2021
//

data {
    int<lower=4> K; // Number of outcome categories (quartiles)
    int<lower=0> N; // Sample size
    int<lower=1,upper=K> y[N]; // Outcome vector of quartiles
    int<lower=1,upper=2> nationality[N]; // Binary variable for artist nationality
    real nationality_prior_mean; // Mean of the prior distribution for the nationality coefficient in log odds
    real nationality_prior_sd; // SD of the prior distribution for the nationality coefficient in log odds
}

parameters {

  // Coefficients for input variables

  real beta_nationality;

  // Number of response variable categories to ensure model doesn't fit beyond them

  ordered[K-1] c;
}

transformed parameters{

  // Variable for likelihood function

  vector[N] calc;

  for (n in 1:N){
    calc[n] = nationality[n] * beta_nationality;
  }
}

model {

  // Priors
  // Distribution of coefficient for nationality is known to be roughly normally distributed
  // (but slightly leptykurtic) so the prior used here is normal. See plot_prior.R function
  // for visualisation of this

  beta_nationality ~ normal(nationality_prior_mean,nationality_prior_sd);

  // Likelihood for Bayesian inference

  for (n in 1:N){
    y[n] ~ ordered_logistic(calc[n], c);
  }
}

generated quantities {

  vector[N] log_lik; // Pointwise estimates for log-likelihood
  int<lower = 1, upper = K> y_ppc[N]; // Instantiate posterior predictive check variable

  for (n in 1:N){

    // Generate log-likehood as Stan doesn't do this automatically

    log_lik[n] = ordered_logistic_lpmf(K | calc[n], c);

    // Generate predictions for posterior predictive checks

    y_ppc[n] = ordered_logistic_rng(beta_nationality, c);
  }

}
