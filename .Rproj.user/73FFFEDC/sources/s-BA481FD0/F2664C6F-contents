#'
#' Function to pull tweets from the 'triplejplays' Twitter account where plays are stored
#' 
#' @import dplyr
#' @import rtweet
#' @author Trent Henderson
#' @export
#' @param year The year of interest to pull tweets from - only current year is possible due to API limitations
#'

get_plays <- function(year = 2020){
  
  message("Pulling tweets...")
  
  if(nchar(year) != 4){
    stop("Year should be a 4-digit number.")
  }
  
  if(!is.numeric(year)){
    stop("Year should be a 4-digit number.")
  }

  eligibility_open <- paste0("01-12-",year-1) # Eligibility opens the year before
  eligibility_deadline <- paste0("30-11-",year) # Closing date for a song to be released, so stop here
  
  tmp <- rtweet::get_timeline("triplejplays", n = 3200) # Get actual tweet data
  
  # Clean up and prep into tidy format for analysis
  # Only retain songs played in the specified range
  
  tmp1 <- tmp %>%
    dplyr::select(c(created_at, text)) %>%
    mutate(created_at_short = gsub(" ", "\\1", created_at)) %>% # Just for easy filtering
    mutate(created_at_short = as.Date(created_at_short)) %>%
    mutate(indicator = case_when(
           created_at_short > as.Date(eligibility_open, format = "%d-%m-%Y") & 
             created_at_short < as.Date(eligibility_deadline, format = "%d-%m-%Y") ~ "Keep",
           TRUE                                                                    ~ "Remove")) %>%
    filter(indicator == "Keep")
  
  #----------------- Need to parse @artists and non-tags -----------
  
  # @artists
  
  parse_tags <- tmp1 %>%
    filter(grepl("@", text)) %>%
    mutate(text = gsub("\\&amp;", "&", text)) %>%
    mutate(artist = gsub("^\\.@", "\\1", text),
           artist = gsub(" .*", "\\1", artist),
           song = gsub(".* -", "\\1", text),
           song = gsub(" \\{.*", "\\1", song),
           song = gsub(" \\[.*", "\\1", song))
  
  # No tags
  
  parse_no_tags <- tmp1 %>%
    filter(!grepl("@", text)) %>%
    mutate(text = gsub("\\&amp;", "&", text)) %>%
    mutate(artist = gsub(" - .*", "\\1", text),
           song = gsub(".* -", "\\1", text),
           song = gsub(" \\{.*", "\\1", song),
           song = gsub(" \\[.*", "\\1", song))
  
  #----------------- Merge back and retention ----------------------
  
  # Merge and calculate plays sums
  
  tmp2 <- bind_rows(parse_tags, parse_no_tags) %>%
    mutate(hottest_100_year = year) %>%
    group_by(hottest_100_year, artist, song) %>%
    summarise(plays = n()) %>%
    ungroup()
  
  if(nrow(tmp2) < 1){
    stop("An issue occurred with data summation.")
  }
  
  return(tmp2)
}
