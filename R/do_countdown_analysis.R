#'
#' Function to produce a small analysis document for a given year's Hottest 100 Countdown
#' #' This function should ideally take the dataframe output straight from hotteR::get_countdowns()
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @author Trent Henderson
#' @export
#' @param data The dataframe of Hottest 100 results to analyse
#'

do_countdown_analysis <- function(data){

  message("Running analysis...")

  # Check if colnames of the input dataframe are correct

  tmp <- data %>%
    janitor::clean_names()

  my_names <- colnames(tmp)
  the_names <- c("year", "rank", "artist", "song")

  if(setequal(my_names, the_names) == FALSE){
    stop("Input dataframe should have the following column structure: year, rank, artist, song")
  }

  #--------------------- Analysis and data visualisation -----------

  # Country breakdown

  p <- tmp %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(counter = n()) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(aes(x = reorder(country, counter), y = counter)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Country breakdown",
                  x = "Country",
                  y = "Number of songs") +
    ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.minor = element_blank())

  # Artist breakdown

  p1 <- tmp %>%
    dplyr::group_by(artist) %>%
    dplyr::summarise(counter = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(indicator = ifelse(counter > 1,artist,"Single")) %>%
    dplyr::group_by(indicator) %>%
    dplyr::summarise(counter = sum(counter)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(aes(x = reorder(indicator, counter), y = counter)) +
    ggplot2::geom_bar(stat = "identity", fill = "#393F5F", alpha = 0.9) +
    ggplot2::labs(title = "Artist breakdown",
                  x = "Artist",
                  y = "Number of songs") +
    ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.minor = element_blank())

  #--------------------- Merge into one plot and export ------------

  p2 <- ggpubr::ggarrange(p, p1, nrows = 1, ncols = 2)

  return(p2)
}
