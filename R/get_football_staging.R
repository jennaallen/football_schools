#create urls based on the years we want data for
urls <- paste0("https://www.sports-reference.com/cfb/years/",years,"-schedule.html")

#get html from urls
urls_html <- lapply(urls, function(x) {
  read_html(x)
})

#get the data for each url and combine it into one data table
football_data_staging <- rbindlist(lapply(urls_html, function(x) {
  x %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table()
  }), fill = TRUE) #fill has to equal true here because different years have different columns included in the data
