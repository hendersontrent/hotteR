
# hotteR

Hottest 100 Countdown and Triple J Analysis in R.

## Installation

You can install `hotteR` from GitHub by running the following:

``` r
devtools::install_github("hendersontrent/hotteR")
```

## get\_plays()

You can automatically pull summed counts of plays from the @triplejplays
Twitter account using the `get_plays()` one-line function. The data is
cleaned, processed, and summed behind the scenes, leaving you more time
to focus on analysis and data visualisation.

``` r
library(hotteR)
library(dplyr)
library(magrittr)

play_data <- get_plays(year = 2020)
```

## Further work

More functions are currently under development. Please check back soon\!
