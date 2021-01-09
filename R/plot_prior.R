#'
#' Function to produce a density plot of the prior distribution of
#' nationality coefficients to visualise the shape before fitting
#' a probabilistic prediction model
#' @import dplyr
#' @import ggplot2
#' @import rstan
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @param timescale a value of either ["Last Decade", "All Time"] indicating the timescale to calculate over
#' @return a density plot of class `ggplot` that shows the distribution of coefficient values in odds
#' @author Trent Henderson
#' @export
#'

plot_prior <- function(timescale = c("Last Decade", "All Time")){

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

  #------------ Run a flat prior model to get priors ------

  # Set up data to feed into the model

  N <- nrow(tmp1)
  K <- length(unique(tmp1$quartile))
  y <- tmp1$quartile
  nationality <- tmp1$nationality

  stan_data <- list(N = N,
                    K = K,
                    y = y,
                    nationality = nationality)

  # Run the model and track evaluation time

  system.time({
    mod <- rstan::stan(data = stan_data,
                       file = system.file("inst", "predict_probabilities.stan", package = "hotteR"), # Ships with package
                       iter = 1000,
                       chains = 3,
                       seed = 123)
  })

  #------------ Draw a density plot -----------------------

  # Aggregations for plot subtitle

  mod_output <- as.data.frame(mod) %>%
    dplyr::summarise(median = median(exp(beta_nationality)),
                     sd = sd(exp(beta_nationality)))

  # Draw plot

  p <- as.data.frame(mod) %>%
    ggplot2::ggplot(aes(x = exp(beta_nationality))) +
    ggplot2::geom_density(fill = "#fa448c", alpha = 0.5, colour = "#331a38") +
    ggplot2::geom_vline(xintercept = 1, colour = "#43b5a0", linetype = "dashed", size = 1.25) +
    ggplot2::geom_vline(xintercept = mod_output$median, colour = "#491d88", linetype = "dashed", size = 1) +
    ggplot2::labs(title = "Posterior distribution of coefficient odds for nationality",
                  subtitle = paste0("Coefficient odds median = ", round(mod_output$median, digits = 2), ", ",
                                    "coefficient odds SD = ", round(mod_output$sd, digits = 2), "\n",
                                    "Odds < 1 indicates that Australian artists have lower odds of being in higher quartiles (i.e. ranked closer to 100) than being in lower quartiles."),
                  x = "Coefficient odds",
                  y = "Density",
                  caption = "This initial model was generated using flat priors. Log odds converted to odds for interpretability.\n Purple line indicates median.") +
    hotteR::theme_hotteR(grids = TRUE)

  return(p)
}
