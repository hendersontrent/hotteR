#------------------------- Helper functions ------------------------

extract_element_text <- function(MyremDr, xpath){
  x <- MyremDr$findElement(using = "xpath", value = xpath)
  x <- x$getElementText()[[1]]
  return(x)
}

click_element <- function(MyremDr, xpath){
  x <- MyremDr$findElement(using = 'xpath', value = xpath)
  x$clickElement()
}

clean_html_str <- function(str){
  x <- str_extract(str, "(<option>(.*?)</option>)+")
  x <- gsub("/", "", x)
  x <- strsplit(x, "<option><option>")[[1]]
  x <- gsub("<option>", "", x)
  return(x)
}

extract_dropdown_options <- function(MyremDr, xpath){
  x <- MyremDr$findElement(using = 'xpath', value = xpath)
  return(x$getElementAttribute("outerHTML")[[1]] %>%
           clean_html_str())
}

#------------------------- Core function ---------------------------

#' Function to webscrape historical Triple J Hottest 100 Countdown results
#' and return a dataframe of cleaned results in an analysis-ready format. Note
#' that this function is highly unstable due to the nature of RSelenium.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import glue
#' @import RSelenium
#' @import stringr
#' @importFrom data.table rbindlist
#' @return a dataframe of results for all years in a tidy format
#' @author Joy Horng
#' @export
#' @examples
#' \dontrun{
#' get_countdowns()
#' }
#'

get_countdowns <- function(){

  start_time <<- Sys.time()

  # Will take 1.5 hrs

  message("Webscraping Hottest 100 Countdown results... This may take up to 1.5 hours. You must have Chrome version 87.0.4280.88 and Java Development Kit installed. This function is somewhat unstable on Mac and may error out at a random year. Please consider accessing the built-in dataset of all historical countdowns `historical_countdowns` instead.")

  # Fire up Selenium ----

  rD <<- RSelenium::rsDriver(browser = "chrome", chromever = "87.0.4280.88", verbose = FALSE, port = 4444L)
  remDr <<- rD[['client']]

  remDr$setTimeout(type = "page load") # set time out limit so it doesn't keep on loading
  suppressMessages({
    tryCatch({remDr$navigate("https://www.abc.net.au/triplej/hottest100/archive/search/?year=2019")},
             error = function(e){}) # catch timeout error
  })

  country_list <- extract_dropdown_options(MyremDr = remDr, '//*[@id="main"]/div[3]/div/div/div[5]/label/select')
  year_list <- extract_dropdown_options(MyremDr = remDr, '//*[@id="main"]/div[3]/div/div/div[1]/label/select')

  store_here <- list()

  # Loop through each year ----

  for(yr in seq(2, 36)){

    tibble_triple_j <- tibble::tibble()

    click_element(MyremDr = remDr, glue::glue('//*[@id="main"]/div[3]/div/div/div[1]/label/select/option[{yr}]'))
    this_year <- year_list[yr-1]
    print(glue::glue('====================== {this_year} ======================'))

    # Loop through the 20 countries to find songs for each year ----

    for(c in seq(2, 20)){
      click_element(glue::glue('//*[@id="main"]/div[3]/div/div/div[5]/label/select/option[{c}]'))

      this_country <- country_list[c-1]
      print(glue::glue('Now on {this_country} ...'))

      Sys.sleep(1) # I think moving too fast breaks Selenium... so build in some sleeping time

      for(i in seq(1:100)){ # each country can at most have 100 songs

        skip_to_next <- FALSE # a switch to control whether it keeps cycling through i

        suppressMessages({
          tryCatch({
            rank <- extract_element_text(MyremDr = remDr, glue::glue('//*[@id="main"]/div[4]/div/table/tbody/tr[{i}]/td[2]'))
            artist <- extract_element_text(MyremDr = remDr, glue::glue('//*[@id="main"]/div[4]/div/table/tbody/tr[{i}]/td[3]'))
            song <- extract_element_text(MyremDr = remDr, glue::glue('//*[@id="main"]/div[4]/div/table/tbody/tr[{i}]/td[4]'))
            if(rank == ""){
              print(glue::glue("No songs for {this_country}. Move on."))
              break() # if no song, move on to next country
            }
            tibble_triple_j <- dplyr::bind_rows(tibble_triple_j, tibble(rank = rank %>% as.numeric(),
                                                                 artist = artist,
                                                                 song = song,
                                                                 year = this_year,
                                                                 country = this_country
                                                                 ))
            Sys.sleep(1) # I think moving too fast breaks Selenium... so build in some sleeping time
          }, error = function(e){
            skip_to_next <<- TRUE # move on to next country
            print(glue::glue('\n ... Scraping for {this_country} completed.'))
          })
        })
        if(skip_to_next){break()}
      }
    }

    # Final check that we've scraped 100 songs before we move on to the next year

    if(nrow(dplyr::filter(tibble_triple_j, year == this_year)) != 100){
      stop("Didn't successfully scrape 100 songs for this year. Something must have gone wrong.")
    } else{
      print(glue::glue("===================================================="))
    }
    store_here[[yr-1]] <- tibble_triple_j %>% dplyr::arrange(rank)
  }

   all_tibble_triple_j <- data.table::rbindlist(store_here, use.names = TRUE)

   fin_time <<- Sys.time()
   message(glue::glue("Scraping completed! Time elapsed: {round(as.numeric(fin_time - start_time) * 60)} minutes."))

   remDr$close() # close the Webdriver
   system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE) # kill the RSelenium session
   return(all_tibble_triple_j)
}
