#'
#' Function to produce a small analysis document for a given selection of song plays based on tweets
#' This function should ideally take the dataframe output straight from hotteR::get_plays()
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @import ggplot2
#' @author Trent Henderson
#' @export
#' @param data The dataframe of triplejplays data to analyse
#'

do_tweet_analysis <- function(data){

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

  p1 <- tmp %>%
    dplyr::group_by(artist, plays) %>%
    dplyr::summarise(counter = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(indicator = ifelse(counter <= 3,artist,"Artists with < 3 plays")) %>%
    dplyr::group_by(artist, indicator) %>%
    dplyr::summarise(plays = sum(plays)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(aes(x = reorder(indicator, plays), y = plays)) +
    ggplot2::geom_bar(stat = "identity", fill = "#393F5F") +
    ggplot2::labs(title = "Artist breakdown",
                  x = "Artist",
                  y = "Number of plays") +
    ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.minor = element_blank())

  # Artist-by-song breakdown

  tmp_song_artist <- tmp %>%
    dplyr::group_by(artist) %>%
    dplyr::summarise(counter = n()) %>%
    dplyr::ungroup()

  tmp_song_plays <- tmp %>%
    dplyr::group_by(artist, plays) %>%
    dplyr::summarise(counter = sum(plays)) %>%
    dplyr::ungroup()

  tmp_song <- tmp_song_artist %>%
    dplyr::left_join(tmp_song_plays, by = c("artist" = "artist"))

  if(nrow(tmp_song) == 0){
    stop("An error occurred. No data found to plot.")
  }

  p1 <- tmp_song %>%
    ggplot2::ggplot(aes(x = number_of_songs, y = number_of_plays)) +
    ggplot2::geom_point(size = 2, alpha = 0.8, colour = "#393F5F") +
    ggplot2::labs(title = "Artist-by-song breakdown",
                  x = "Number of songs played",
                  y = "Total plays",
                  caption = "Each point is for an artist.") +
    ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.minor = element_blank())

  #--------------------- Merge into one plot and export ------------

  p2 <- ggpubr::ggarrange(p, p1, nrows = 1, ncols = 2)

  return(p2)
}
