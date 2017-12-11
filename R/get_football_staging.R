get_football_data <- function(games) {
  if (!(games %in% c("schedule", "bowls"))) {
    stop("Argument must be either schedule or bowls")
  }
  # create urls based on the desired years and if user wants all games or just bowl games
  urls <- paste0("https://www.sports-reference.com/cfb/years/",years,"-",games,".html")
  
  # get html from urls
  urls_html <- lapply(urls, function(x) {
    read_html(x)
  })
  
  # get the data for each url and combine it into one data table
  football_data_staging <- rbindlist(lapply(urls_html, function(x) {
    x %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table()
  }), fill = TRUE) # fill = true here because different years have different columns included in the data
  return(football_data_staging)
}

football_data_staging <- get_football_data("schedule")
football_data_staging_bowls <- get_football_data("bowls")
