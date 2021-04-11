#'
#' Function to produce a small analysis document for a given selection of song plays based on tweets
#' This function should ideally take the dataframe output straight from hotteR::get_plays()
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_title
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @return Returns an object of class ggplot which contains a matrix of plots
#' @author Trent Henderson
#' @export
#' @param data The dataframe of triplejplays data to analyse
#' @examples
#' \dontrun{
#' d <- get_plays(year = 2020)
#' do_plays_analysis(d)
#' }
#'

do_plays_analysis <- function(data){

  message("Running analysis...")

  # Check if colnames of the input dataframe are correct

  tmp <- data %>%
    janitor::clean_names()

  my_names <- colnames(tmp)
  the_names <- c("hottest_100_year", "artist", "song", "plays")

  if(setequal(my_names, the_names) == FALSE){
    stop("Input dataframe should have the following column structure: ")
  }

  #--------------------- Analysis and data visualisation -----------

  # Artist breakdown

  p <- tmp %>%
    dplyr::group_by(artist) %>%
    dplyr::summarise(plays = sum(plays)) %>%
    dplyr::ungroup() %>%
    dplyr::top_n(15, plays) %>%
    ggplot2::ggplot(aes(x = reorder(artist, plays), y = plays)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Top 15 artists by plays",
                  subtitle = paste0("There was a total of ",sum(tmp$plays)," plays analysed."),
                  x = "Artist Twitter Handle",
                  y = "Number of plays") +
    ggplot2::coord_flip() +
    theme_hotteR(grids = TRUE)

  # Artist-by-song breakdown

  tmp_song_artist <- tmp %>%
    dplyr::group_by(artist, song) %>%
    dplyr::summarise(plays = sum(plays)) %>%
    dplyr::group_by(artist) %>%
    dplyr::summarise(number_of_songs = n()) %>%
    dplyr::ungroup()

  tmp_song_plays <- tmp %>%
    dplyr::group_by(artist) %>%
    dplyr::summarise(number_of_plays = sum(plays)) %>%
    dplyr::ungroup()

  tmp_song <- tmp_song_artist %>%
    dplyr::left_join(tmp_song_plays, by = c("artist" = "artist")) %>%
    tidyr::pivot_longer(!artist, names_to = "variable", values_to = "value") %>%
    dplyr::mutate(variable = stringr::str_to_title(variable),
                  variable = gsub("_", " ", variable))

  if(nrow(tmp_song) == 0){
    stop("An error occurred. No data found to plot.")
  }

  p1 <- tmp_song %>%
    ggplot2::ggplot(aes(x = log(value))) +
    ggplot2::geom_density() +
    ggplot2::labs(title = "All artists-by-song distributional breakdown",
                  x = "log(value)",
                  y = "Density",
                  caption = "Data log-scaled for interpretability.") +
    ggplot2::facet_wrap(~variable) +
    theme_hotteR(grids = TRUE)

  #--------------------- Merge into one plot and export ------------

  p2 <- ggpubr::ggarrange(p, p1, nrow = 2, ncol = 1)

  return(p2)
}
