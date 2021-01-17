#'
#' Function to calculate quartile time series composition for Australian artists and produce a stacked area graph
#' @import dplyr
#' @import ggplot2
#' @importFrom hotteR historical_countdowns
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @return an object of class `ggplot` which is a stacked area graph
#' @author Trent Henderson
#' @export
#'

plot_quartile_area <- function(){

  # Initial aggregation

  tmp <- historical_countdowns %>%
    janitor::clean_names() %>%
    dplyr::mutate(indicator = case_when(
      grepl(" ", year) ~ "Remove",
      TRUE             ~ "Keep")) %>%
    dplyr::filter(indicator == "Keep") %>% # Remove specialist Countdowns (e.g. Of The Decade, All-Time)
    dplyr::select(-c(indicator)) %>%
    dplyr::mutate(quartile = case_when(
      rank <= 25             ~ 1, # Computes 4 quantiles to group rankings by
      rank > 25 & rank <= 50 ~ 2,
      rank > 50 & rank <= 75 ~ 3,
      rank > 75              ~ 4)) %>%
    dplyr::mutate(nationality = ifelse(country == "Australia", 2, 1)) %>% # Buckets countries into binary
    dplyr::mutate(nationality = as.integer(nationality)) %>%
    dplyr::mutate(year = as.numeric(year))

  # Draw stacked area plot

  p <- tmp %>%
    dplyr::filter(nationality == 2) %>%
    dplyr::group_by(year, quartile) %>%
    dplyr::summarise(counter = n()) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(probs = counter / sum(counter)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(quartile = factor(quartile, levels = c(1,2,3,4))) %>%
    dplyr::mutate(quartile = case_when(
      quartile == "1" ~ "Rank 1-25",
      quartile == "2" ~ "Rank 26-50",
      quartile == "3" ~ "Rank 51-75",
      quartile == "4" ~ "Rank 76-100")) %>%
    ggplot2::ggplot(aes(x = year, y = probs)) +
    ggplot2::geom_area(aes(fill = quartile)) +
    ggplot2::labs(title = "Time series of Australian artist Hottest 100 composition by quartile",
         x = "Year",
         y = "Proportion of Australian artists",
         fill = "Quartile") +
    ggplot2::scale_y_continuous(limits = c(0,1),
                       breaks = seq(from = 0, to = 1, by = 0.2)) +
    ggplot2::scale_fill_manual(values = c("#fa448c", "#fec859", "#43b5a0", "#491d88")) +
    hotteR::theme_hotteR(grids = TRUE) +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}
