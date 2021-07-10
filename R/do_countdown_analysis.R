#' Function to produce a small analysis document for a given year's Hottest 100 Countdown
#' This function should ideally take the dataframe output straight from hotteR::get_countdowns()
#' or from the included historical_countdowns dataframe.
#'
#' @importFrom stats reorder
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom scales comma
#' @param data The dataframe of Hottest 100 results to analyse
#' @return Returns an object of class ggplot which contains a matrix of plots
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' do_countdown_analysis(historical_countdowns)
#' }
#'

do_countdown_analysis <- function(data = historical_countdowns){

  message("Running analysis...")

  # Check if colnames of the input dataframe are correct

  tmp <- data %>%
    janitor::clean_names() %>%
    dplyr::mutate(indicator = dplyr::case_when(
      grepl(" ", year) ~ "Remove",
      TRUE             ~ "Keep")) %>%
    dplyr::filter(indicator == "Keep") %>% # Remove specialist Countdowns (e.g. Of The Decade, All-Time)
    dplyr::select(-c(indicator))

  my_names <- colnames(tmp)
  the_names <- c("year", "rank", "artist", "song", "country")

  if(setequal(my_names, the_names) == FALSE){
    stop("Input dataframe should have the following columns: year, rank, artist, song, country")
  }

  #--------------------- Analysis and data visualisation -----------

  # Country breakdown

  p <- tmp %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(country, counter), y = counter)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Country breakdown",
                  x = "Country",
                  y = "Number of songs") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::coord_flip() +
    hotteR::theme_hotteR(grids = TRUE)

  # Artist breakdown

  p1 <- tmp %>%
    dplyr::group_by(artist) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(indicator = ifelse(counter > 10,artist,"Single")) %>%
    dplyr::filter(indicator != "Single") %>%
    dplyr::group_by(indicator) %>%
    dplyr::summarise(counter = sum(counter)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(indicator, counter), y = counter)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Artists with > 10 songs",
                  x = "Artist",
                  y = "Number of songs") +
    ggplot2::coord_flip() +
    hotteR::theme_hotteR(grids = TRUE)

  # Boxplot

  most_recent <- tmp %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::select(c(year)) %>%
    dplyr::distinct() %>%
    dplyr::top_n(year, n = 1)

  p2 <- tmp %>%
    dplyr::mutate(country = ifelse(country == "Australia", "Australian", "International")) %>%
    dplyr::mutate(decade = ifelse(year %in% most_recent$year, "Last Countdown", "Prior Countdowns")) %>%
    ggplot2::ggplot(ggplot2::aes(x = country, y = rank)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = country), alpha = 0.7, outlier.shape = NA, colour = "#331a38") +
    ggplot2::labs(title = "Rank distribution",
                  x = "Artist nationality",
                  y = "Hottest 100 rank",
                  fill = "Artist nationality") +
    ggplot2::scale_fill_manual(values = c("#fa448c", "#fec859")) +
    ggplot2::scale_y_continuous(limits = c(1,100)) +
    hotteR::theme_hotteR(grids = TRUE) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(~decade)

  # Time series

  if(length(unique(tmp$year)) > 1){

    p3 <- tmp %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::mutate(country = ifelse(country == "Australia", "Australian", "International")) %>%
      dplyr::group_by(year, country) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(props = (counter / sum(counter))*100) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = year, y = props)) +
      ggplot2::geom_line(ggplot2::aes(colour = country), stat = "identity", size = 1) +
      ggplot2::labs(title = "Time series of nationality",
                    x = "Year",
                    y = "Percentage of songs",
                    colour = "Artist nationality") +
      ggplot2::scale_colour_manual(values = c("#fa448c", "#fec859")) +
      ggplot2::scale_y_continuous(limits = c(0,100),
                                  breaks = c(0,25,50,75,100),
                                  labels = function(x) paste0(x,"%")) +
      ggplot2::scale_x_continuous(labels = function(x) round(x, digits = 0)) +
      hotteR::theme_hotteR(grids = TRUE)

    myplot <- ggpubr::ggarrange(p2,p3,p,p1)
  } else{
    myplot <- ggpubr::ggarrange(p3,
                            ggpubr::ggarrange(p, p1, ncol = 2),
                            nrow = 2)
  }

  #--------------------- Export ------------------------------------

  return(myplot)
}
