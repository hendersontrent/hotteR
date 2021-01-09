//
// This stan program aims to build a Bayesian model for predicting
// probability of membership for Australian and International
// artists in each of the four quartiles in the Triple J Hottest 100
// Countdown. This is the prior generation program.
//
// NOTE: This program will eventually have the capability to take additional
// model inputs such as number of song plays on Triple J. This is slated for 
// a future versioned release of hotteR.
//
// This program uses flat priors (i.e. does not specify any) as it aims to fit
// a basic model that can then be used as an informative prior in a prediction
// based model environment.
//

//
// Author: Trent Henderson, 8 January 2021
//

data {
    int<lower=4> K; // Number of outcome categories (quartiles)
    int<lower=0> N; // Sample size
    int<lower=1,upper=K> y[N]; // Outcome vector of quartiles
    int<lower=1,upper=2> nationality[N]; // Binary variable for artist nationality
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

  // No priors as this is a flat model to extract initial simulated coefficients
  // Stan automatically knows that absence of prior specification in model block
  // indicates a flat prior model

  // Likelihood for Bayesian inference
  
  for (n in 1:N){
    y[n] ~ ordered_logistic(calc[n], c);
  }
}