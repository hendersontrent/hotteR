library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(glue)
library(ggpubr)
library(janitor)
library(rtweet)
library(RSelenium)
library(rstan)

#------------- get_plays() -------------------------

the_plays <- get_plays(year = 2020)

#------------- do_plays_analysis() -----------------

do_plays_analysis(the_plays)

#------------- get_countdowns() --------------------

get_countdowns()

#------------- do_countdown_analysis() -------------

do_countdown_analysis()

#------------- calculate_priors() ------------------

calculate_priors(timescale = c("Last Decade"))

#------------- plot_probabilities() ----------------

plot_probabilities(timescale = c("Last Decade"))

#------------- plot_prior() ------------------------

plot_prior(timescale = c("Last Decade"))

#------------- plot_quartile_area() ----------------

plot_quartile_area()
