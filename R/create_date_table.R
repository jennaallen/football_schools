
load_date_table <- function () {
  date_value <- seq(as.Date(paste0("01/01/", years[1]),format ="%m/%d/%Y"), by = "day", length.out = 6940)
  date_sk <- as.integer(format(date_value, format="%Y%m%d"))
  school_year_sk <- ifelse(month(date_value) %in% c(1, 2, 3, 4, 5, 6, 7), year(date_value) - 1, year(date_value))
  month_value <- month(date_value)
  day_value <- day(date_value)
  year_value <- year(date_value)
  day_of_week_value <- wday(date_value)
  day_of_week_name <- wday(date_value, label = TRUE, abbr = FALSE)
  month_name <- month(date_value, label = TRUE, abbr = FALSE)
  return(data.table(date_sk, school_year_sk, date_value, month_value, day_value, year_value, day_of_week_value, day_of_week_name, month_name))
}

dimDate <- load_date_table()
dbWriteTable(conn = con, name = 'dimDate', value = dimDate, append = TRUE, row.names = FALSE)