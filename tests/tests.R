library(hotteR)

#------------- get_plays() -------------------------

#the_plays <- get_plays(year = 2020)

#------------- do_plays_analysis() -----------------

#do_plays_analysis(the_plays)

#------------- get_countdowns() --------------------

#get_countdowns(sleep = 1)

#------------- do_countdown_analysis() -------------

do_countdown_analysis(data = historical_countdowns)

#------------- plot_probabilities() ----------------

plot_probabilities(data = historical_countdowns, timescale = "Last Decade")
plot_probabilities(data = historical_countdowns, timescale = "All Time")

#------------- plot_quartile_area() ----------------

plot_quartile_area(data = historical_countdowns)
