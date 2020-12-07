#'
#' Function to webscrape historical Triple J Hottest 100 Countdown results
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import rvest
#' @import XML
#' @import data.table
#' @import xml2
#' @author Trent Henderson
#' @export
#' @param year The year of interest to pull hottest 100 for. Defaults to all (1993-present)
#'

get_countdowns <- function(year = "All"){

  message("Webscraping Hottest 100 Countdown results...")

  # Convert year to character to reduce chances of type errors between years and 'All' category

  year <- as.character(year)

  # List of all the options possible

  the_options <- c(as.character(1993:2019),"All")
  '%ni%' <- Negate('%in%')

  if(year %ni% the_options){
    stop("Year should be either 'All', a vector of years, or a single year between 1993 and 2019 inclusive.")
  }

  #----------------- Run the actual webscraper ---------------------

  # Pick which dataframe to retain

  if(year == "All"){
    year_nums <- c(1993:2019)
    store_here <- list()

    for(i in year_nums){
      general_url <- paste0("https://www.abc.net.au/triplej/hottest100/archive/search/?year=",i)

      link  <- read_html(general_url)
      tmp <- html_table(link) [[3]] %>%
        x
    }
    tmp1 <- rbindlist(store_here, use.names = TRUE)
  } else if (year != "All" & length(year) > 1){
    year_nums <- year
    store_here <- list()
    for(i in year_nums){
      general_url <- paste0("https://www.abc.net.au/triplej/hottest100/archive/search/?year=",i)

      link  <- read_html(general_url)
      tmp <- html_table(link) [[3]] %>%
        x
    }
    tmp1 <- rbindlist(store_here, use.names = TRUE)
  } else if(year != "All" & length(year) == 1){
    general_url <- paste0("https://www.abc.net.au/triplej/hottest100/archive/search/?year=",year)

    tmp1 <- read_html(general_url) %>%
      html_nodes(xpath = '//*[@id="main"]/div[4]/div/table') %>%
      html_table(fill = TRUE) %>%
      setNames (c("year", "rank", "artist", "song")) %>%
      as_tibble()
  }

  #----------------- Merge back and retention ----------------------

  return(tmp1)
}
