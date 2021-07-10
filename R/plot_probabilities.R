#'
#' Function to produce density plots of probability of membership by quartile and nationality
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @param data The dataframe of Hottest 100 results to analyse
#' @param timescale a value of either "Last Decade" or "All Time" indicating the timescale to calculate over
#' @return an object of class ggpot which holds the matrix of density plots
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' plot_probabilities(timescale = "Last Decade")
#' }
#'

plot_probabilities <- function(data = historical_countdowns, timescale = c("Last Decade", "All Time")){

  # Make All Time the default

  if(missing(timescale)){
    timescale <- "All Time"
  } else{
    timescale <- match.arg(timescale)
  }

  timescale_text <- timescale
  timescales <- c("Last Decade", "All Time")
  '%ni%' <- Negate('%in%')

  if(timescale %ni% timescales){
    stop("timescale should be a single entry of either 'Last Decade' or 'All Time'")
  }

  if(length(timescale) != 1){
    stop("timescale should be a single entry of either 'Last Decade' or 'All Time'")
  }

  #------------ Aggregate historical data -----------------

  message("Calculating probabilities...")

  tmp <- data %>%
    janitor::clean_names() %>%
    dplyr::mutate(indicator = dplyr::case_when(
      grepl(" ", year) ~ "Remove",
      TRUE             ~ "Keep")) %>%
    dplyr::filter(indicator == "Keep") %>% # Remove specialist Countdowns (e.g. Of The Decade, All-Time)
    dplyr::select(-c(indicator)) %>%
    dplyr::mutate(quartile = dplyr::case_when(
      rank <= 25             ~ "First Quartile", # Computes 4 quantiles to group rankings by
      rank > 25 & rank <= 50 ~ "Second Quartile",
      rank > 50 & rank <= 75 ~ "Third Quartile",
      rank > 75              ~ "Fourth Quartile")) %>%
    dplyr::mutate(nationality = ifelse(country == "Australia", "Australian", "International")) %>% # Buckets countries into binary
    dplyr::mutate(year = as.numeric(year))

  if(timescale == "Last Decade"){

    # Retrieve last decade of values

    last_decade <- tmp %>%
      dplyr::select(c(year)) %>%
      dplyr::distinct() %>%
      dplyr::top_n(year, n = 10) # Makes it dynamic instead of hard coding and working backwards

    # Aggregate over quartiles

    tmp1 <- tmp %>%
      dplyr::filter(year %in% last_decade$year) %>%
      dplyr::group_by(year, nationality, quartile) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::group_by(year, quartile) %>%
      dplyr::mutate(probs = round(counter / sum(counter), digits = 3)) %>%
      dplyr::ungroup()
  }

  if(timescale == "All Time"){

    tmp1 <- tmp %>%
      dplyr::group_by(year, nationality, quartile) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::group_by(year, quartile) %>%
      dplyr::mutate(probs = round(counter / sum(counter), digits = 3)) %>%
      dplyr::ungroup()
  }

  #------------ Render matrix of density plots ------------

  p <- tmp1 %>%
    dplyr::mutate(quartile = factor(quartile, levels = c("First Quartile", "Second Quartile",
                                                         "Third Quartile", "Fourth Quartile"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = probs)) +
    ggplot2::geom_density(ggplot2::aes(fill = nationality), alpha = 0.4, colour = "#331a38") +
    ggplot2::labs(title = "Probabilities of nationality by quartile",
                  subtitle = "First Quartile = Rank 1-25, Fourth Quartile = Rank 76-100",
                  x = "Probability",
                  y = "Density",
                  fill = "Artist nationality",
                  caption = paste0("Timescale = ", timescale_text)) +
    ggplot2::scale_x_continuous(limits = c(0,1),
                                breaks = seq(from = 0, to = 1, by = 0.2)) +
    hotteR::theme_hotteR(grids = TRUE) +
    ggplot2::scale_fill_manual(values = c("#fa448c", "#43b5a0")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(~quartile)

  return(p)
}
