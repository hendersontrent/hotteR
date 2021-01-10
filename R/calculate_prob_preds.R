#'
#' Function to produce a basic Bayesian analysis of artist nationality
#' probabilities for each quartile in the next Hottest 100 Countdown using
#' an informative prior from the calculate_priors() function
#' @import dplyr
#' @import rstan
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @param timescale a value of either ["Last Decade", "All Time"] indicating the timescale to calculate over
#' @param nationality_prior_mean a scalar value for the mean of the prior distribution of nationality in log odds
#' @param nationality_prior_sd a scalar value for the standard deviation of the prior distribution of nationality in log odds
#' @return a dataframe of tidied model outputs including values for coefficients, eta, log likelihood and posterior predictive checks
#' @author Trent Henderson
#' @export
#'

calculate_prob_preds <- function(timescale = c("Last Decade", "All Time"),
                                 nationality_prior_mean,
                                 nationality_prior_sd){

  timescale <- match.arg(timescale)

  message("Calculating probabilities. This may take a substantial period of time due to the nature of Bayesian analysis and MCMC sampling... Stan will print updates in the console as it runs.")

  if(length(timescale) != 1){
    stop("timescale should be a single entry of either 'Last Decade' or 'All Time'")
  }

  the_timescales <- c("Last Decade", "All Time")
  '%ni%' <- Negate('%in%')

  if(timescale %ni% the_timescales){
    stop("timescale should be a single entry of either 'Last Decade' or 'All Time")
  }

  if(!as.numeric(nationality_prior_mean)){
    stop("nationality_prior_mean should be a numeric scalar quantity.")
  }

  if(!as.numeric(nationality_prior_sd)){
    stop("nationality_prior_sd should be a numeric scalar quantity.")
  }

  #------------ Aggregate historical data -----------------

  tmp <- historical_countdowns %>%
    janitor::clean_names() %>%
    dplyr::mutate(indicator = case_when(
      grepl(" ", year) ~ "Remove",
      TRUE             ~ "Keep")) %>%
    dplyr::filter(indicator == "Keep") %>% # Remove specialist Countdowns (e.g. Of The Decade, All-Time)
    dplyr::select(-c(indicator)) %>%
    dplyr::mutate(quartile = case_when(
      rank <= 25             ~ 1, # Computes 4 quantiles to group rankings by
      rank > 25 & rank <= 50 ~ 2,
      rank > 50 & rank <= 75 ~ 3,
      rank > 75              ~ 4)) %>%
    dplyr::mutate(nationality = ifelse(country == "Australia", 2, 1)) %>% # Buckets countries into binary
    dplyr::mutate(nationality = as.integer(nationality)) %>%
    dplyr::mutate(year = as.numeric(year))

  if(timescale == "Last Decade"){

    # Retrieve last decade of values

    last_decade <- tmp %>%
      dplyr::select(c(year)) %>%
      dplyr::distinct() %>%
      dplyr::top_n(year, n = 10) # Makes it dynamic instead of hard coding and working backwards

    # Aggregate over quartiles

    tmp1 <- tmp %>%
      dplyr::filter(year %in% last_decade$year)
  }

  if(timescale == "All Time"){
    tmp1 <- tmp
  }

  #------------ Run the Bayesian model --------------------

  # Set up data to feed into the model

  N <- nrow(tmp1)
  K <- length(unique(tmp1$quartile))
  y <- tmp1$quartile
  nationality <- tmp1$nationality
  nationality_prior_mean <- nationality_prior_mean
  nationality_prior_sd <- nationality_prior_sd


  stan_data <- list(N = N,
                    K = K,
                    y = y,
                    nationality = nationality,
                    nationality_prior_mean = nationality_prior_mean,
                    nationality_prior_sd = nationality_prior_sd)

  # Run the model and track evaluation time

  system.time({
    mod <- rstan::stan(data = stan_data,
                       file = system.file("stan", "calculate_prob_preds.stan", package = "hotteR"), # Ships with package
                       iter = 1000,
                       chains = 3,
                       seed = 123)
  })

  #------------- Extract model outputs and samples -----------------

  # Coefficients

  betas <- as.data.frame(mod) %>%
    dplyr::select(c(beta_nationality)) %>%
    dplyr::mutate(ID = dplyr::row_number()) %>% # Enables left_join() later
    dplyr::select(c(ID, beta_nationality))

  # Eta

  calc <- as.data.frame(mod) %>%
    dplyr::select(contains("calc")) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::gather(key = calc, value = value, contains("calc")) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(calc_mean = mean(value),
              calc_median = median(value),
              calc_lower = quantile(value, probs = 0.025),
              calc_upper = quantile(value, probs = 0.975)) %>%
    dplyr::ungroup()

  # Log likelihood

  log_lik <- as.data.frame(mod) %>%
    dplyr::select(contains("log_lik")) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::gather(key = log_lik, value = value, contains("log_lik")) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(log_lik_mean = mean(value),
              log_lik_median = median(value),
              log_lik_lower = quantile(value, probs = 0.025),
              log_lik_upper = quantile(value, probs = 0.975)) %>%
    dplyr::ungroup()

  # Posterior predictive checks

  ppc <- as.data.frame(mod) %>%
    dplyr::select(contains("y_ppc")) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::gather(key = y_ppc, value = value, contains("y_ppc")) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(y_ppc_mean = mean(value),
              y_ppc_median = median(value)) %>%
    dplyr::ungroup()

  # Merge all together

  all_model_outs <- betas %>%
    dplyr::left_join(calc, by = c("ID" = "ID")) %>%
    dplyr::left_join(log_lik, by = c("ID" = "ID")) %>%
    dplyr::left_join(ppc, by = c("ID" = "ID"))

  return(all_model_outs)
}
