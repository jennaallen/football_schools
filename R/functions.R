get_football_data <- function(games, years) {
  if (!(games %in% c("schedule", "bowls"))) {
    stop("Argument must be either 'schedule' or 'bowls'", call. = FALSE)
  }
  # create urls based on the desired years and if user wants all games or bowl games
  urls <- paste0("https://www.sports-reference.com/cfb/years/",years,"-",games,".html")
  
  # get html from urls
  urls_html <- map(urls, ~ read_html(.x))
  
  # get the data for each url and combine it into one data table
  football_data <- rbindlist(map(urls_html, ~ .x %>%
                                   html_nodes("table") %>%
                                   .[[1]] %>%
                                   html_table()), fill = TRUE) # fill = true here because different years have different columns included in the data
  return(football_data)
}

clean_school_data <- function(dirty_data) {
  state_lookups <- read_csv("lookups.csv") %>%
    filter(VariableName == "State abbreviation (HD2016)") %>%
    select(Value, ValueLabel)
  
  clean_data <- dirty_data %>%
    mutate(region = recode(region,
                           "0"	= "US Service schools",
                           "1"	= "New England",
                           "2"	= "Mid East",
                           "3"	= "Great Lakes",
                           "4"	= "Plains",
                           "5"	= "Southeast",
                           "6"	= "Southwest",
                           "7"	= "Rocky Mountains",
                           "8"	= "Far West",
                           "9"	= "Outlying"),
           control = recode(control,
                            "1"	= "Public",
                            "2"	= "Private not-for-profit"),
           iclevel = recode(iclevel,
                            "1"	= "4-year",
                            "2"	= "2-year",
                            "3"	= "Less-than-2-year"),
           zip = str_sub(zip, end = 5), # keep only the first 5 numbers for zip code; some values are missing dash between zip and zip +4 code
           school_size_category = case_when(ugds < 1000 ~ "Under 1,000",
                                            between(ugds, 1000, 4999) ~ "1,000 - 4,999",
                                            between(ugds, 5000, 9999) ~ "5,000 - 9,999",
                                            between(ugds, 10000, 19999) ~ "10,000 - 19,999",
                                            ugds > 20000 ~ "20,000 and above",
                                            TRUE ~ NA_character_)) %>%
    left_join(state_lookups, by = c("stabbr" = "Value")) %>%
    rename("ACT_composite_25th_percentile" = actcm25,
           "ACT_composite_75th_percentile" = actcm75,
           "ACT_composite_midpoint" = actcmmid,
           "ACT_english_25th_percentile" = acten25,
           "ACT_english_75th_percentile" = acten75,
           "ACT_english_midpoint" = actenmid,
           "ACT_math_25th_percentile" = actmt25,
           "ACT_math_75th_percentile" = actmt75, 
           "ACT_math_midpoint" = actmtmid,
           "school_admission_rate" = adm_rate,
           "school_alias" = alias,
           "school_graduation_rate_4yrs" = c100_4,
           "school_graduation_rate_6yrs" = c150_4,
           "school_city" = city,
           "school_control" = control,
           "school_median_debt_graduates" = grad_debt_mdn,
           "school_median_debt_graduates_monthly_payments" = grad_debt_mdn10yr,
           "school_name" = instnm,
           "school_url" = insturl,
           "school_latitude" = latitude,
           "school_level" = iclevel,
           "school_students_with_any_loan" = loan_ever,
           "school_longitude" = longitude,
           "school_main_campus_flag" = main,
           "school_opeid8" = opeid,
           "school_opeid6" = opeid6,
           "school_federal_loan_rate" = pctfloan,
           "school_region" = region,
           "school_retention_rate" = ret_ft4,
           "SAT_math_25th_percentile" = satmt25,
           "SAT_math_75th_percentile" = satmt75, 
           "SAT_math_midpoint" = satmtmid,
           "SAT_reading_25th_percentile" = satvr25,
           "SAT_reading_75th_percentile" = satvr75, 
           "SAT_reading_midpoint" = satvrmid,
           "school_st_abbr" = stabbr,
           "school_in_state_price" =  tuitionfee_in,
           "school_out_state_price" = tuitionfee_out, 
           "school_size" = ugds,
           "school_id" = unitid,
           "school_state" = ValueLabel,
           "school_year_start" =  year,
           "school_zip" = zip)
  return(clean_data)
}

clean_football_data <- function(all_games, bowls) {
  clean_bowls <- bowls %>% 
    set_tidy_names() %>%
    as_tibble() %>%
    mutate_at(vars(Date), ymd) %>% 
    select(Date, Bowl, Winner)
  clean_all_games <- all_games %>%
    set_tidy_names() %>% 
    as_tibble() %>%
    rename(school_points = Pts..6, opponent_points = Pts..9, home_away = V1) %>% 
    mutate_all(funs(replace(., . == '', NA))) %>%
    filter(!(Rk %in% "Rk") & !(Notes %like% "Cancelled")) %>%
    mutate(school_rank = str_extract(Winner, "\\d{1,2}"),
           opponent_rank = str_extract(Loser, "\\d{1,2}"),
           school = str_replace(Winner, "\\(\\d{1,2}\\)\\s", ""),
           opponent = str_replace(Loser, "\\(\\d{1,2}\\)\\s", ""),
           school_game_site = case_when(home_away %in% "@" ~ "away",
                                        is.na(Notes) ~ "home",
                                        TRUE ~ "neutral"),
           opponent_game_site = case_when(school_game_site %in% "away" ~ "home",
                                          school_game_site %in% "home" ~ "away",
                                          TRUE ~ "neutral"),
           school_win = as.integer(1),
           opponent_win = as.integer(0)) %>%
    mutate_at(vars(Rk, Wk, school_points, opponent_points, school_rank, opponent_rank), as.integer) %>% 
    mutate_at(vars(Date), mdy) %>%
    left_join(clean_bowls, by = c("Date" = "Date", "school" = "Winner")) %>% 
    mutate(bowl_flag = if_else(!is.na(Bowl), 1, 0),
           national_championship_flag = if_else(Bowl %like% "Championship", 1, 0)) %>%
    mutate_at(vars(bowl_flag, national_championship_flag), as.integer) %>%
    rename("game_number" = Rk,
           "football_week" = Wk,
           "game_date" = Date,
           "game_day" = Day,
           "football_notes" = Notes,
           "game_time" = Time,
           "game_tv" = TV,
           "bowl" = Bowl) %>% 
    select(-Winner, -Loser, -home_away)
  return(clean_all_games) 
}

fuzzy_text_matching <- function(x, y, text = NULL, before_after = NULL) {
  # x = transformed school data
  # y = transformed football data
  # get unique school names
  unique_school_names <- x %>% 
    filter(school_year_start == max(school_year_start) & school_main_campus_flag == 1) %>% 
    select(school_name) %>%
    distinct() %>% 
    mutate(dummy = 1) # create dummy variable to join on
  
  # get unique football school names by combining values in the school and opponent columns into one column
  unique_football_names <- y %>% 
    select(school, opponent) %>% 
    gather("delete", "football_name", 1:2) %>%
    select(football_name) %>% 
    distinct() %>% 
    mutate(dummy = 1) # create dummy variable to join on
  
  # use stringdist for fuzzy text matching; run through all possible methods used for distance calculation
  # cross join football names with all school names (227 football names * 2187 school names = 496449 possible combinations)
  ft_all <- unique_football_names %>% 
    mutate(low_football_name = tolower(football_name)) %>% 
    inner_join(unique_school_names, by = "dummy") %>% # join on dummy variable to essentially do a cross join
    mutate(low_school_name = tolower(school_name)) 
  
  method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  if (is.null(text) & is.null(before_after)) {
    ft_result <- method_list %>% 
      map_dfc(~ tibble(stringdist(ft_all$low_football_name, ft_all$low_school_name, method = .x))) %>% 
      set_names(method_list) %>% 
      bind_cols(ft_all, .) %>% 
      select(-dummy, -low_school_name, -low_football_name)
  } else if (is.null(text) & !is.null(before_after)) {
    stop("Text to concantenate with football name is missing", call. = FALSE)
  } else if (!is.null(text) & is.null(before_after)) {
    stop("Please specify if text should be concantenated before or after the football name", call. = FALSE)
  } else if (!(before_after %in% c("before", "after"))) {
    stop("The 'before_after' argument must have a value of before or after", call. = FALSE)
  } else if (before_after == "before") {
    ft_before <- ft_all %>%
      mutate(text_football_name = paste(tolower(text), low_football_name))
    ft_result <- method_list %>% 
      map_dfc(~ tibble(stringdist(ft_before$text_football_name, ft_before$low_school_name, method = .x))) %>% 
      set_names(method_list) %>% 
      bind_cols(ft_before, .) %>% 
      select(-text_football_name, -dummy, -low_school_name, -low_football_name)
  } else {
    ft_after <- ft_all %>%
      mutate(football_name_text = paste(low_football_name, tolower(text)))
    ft_result <- method_list %>% 
      map_dfc(~ tibble(stringdist(ft_after$football_name_text, ft_after$low_school_name, method = .x))) %>% 
      set_names(method_list) %>% 
      bind_cols(ft_after, .) %>% 
      select(-football_name_text, -dummy, -low_school_name, -low_football_name)
  }
  return(ft_result)
}

get_missing_data <- function(file_path, column_names, column_pos) {
  file_names <- list.files(path = file_path, pattern = "*\\.csv", full.names = TRUE)
  # Merge all school data files
  combined_data <- file_names %>% 
    map_dfr(~ fread(.x, skip = 1, header = FALSE, select = column_pos, col.names = column_names), .id = "index") %>% 
    as_tibble() %>%
    mutate_at(vars(index), as.integer) %>% 
    mutate(file = basename(file_names[index]))
  return(combined_data)
}

clean_missing_data <- function(missing1, missing2) {
  clean_data <- missing1 %>% 
    bind_rows(missing2) %>% 
    filter(school_id %in% c(197036, 128328, 164155)) %>% 
    mutate(school_zip = str_sub(zip, end = 5),
           school_region = recode(region,"0" = "US Service school"),  
           school_level = recode(level,"1" = "4-year"),
           school_control = recode(control,"1" = "Public"),
           school_size_category = recode(size_category,"2" = "1,000 - 4,999"),  
           school_state = recode(school_st_abbr, 
                                 "CO" = "Colorado", 
                                 "NY" = "New York", 
                                 "MD" = "Maryland"),
           school_year_start = str_sub(file, end = 4),
           school_main_campus_flag = 1) %>% 
    mutate_at(vars(school_graduation_rate_6yrs, school_admission_rate, school_retention_rate), funs(. / 100)) %>% 
    mutate_at(vars(school_year_start), as.integer) %>% 
    select(-index, -file, -zip, -region, -level, -control, -size_category)
  
  return(clean_data)
}

create_date_table <- function(start_year) {
  d <- tibble(date_value = seq(mdy(paste0("01/01/", start_year)), by = "day", length.out = 6940),
              date_sk = as.integer(format(date_value, format = "%Y%m%d")),
              school_year_sk = if_else(month(date_value) %in% c(1, 2, 3, 4, 5, 6, 7), as.integer(year(date_value) - 1), as.integer(year(date_value))),
              month_value = month(date_value),
              day_value = day(date_value),
              year_value = year(date_value),
              day_of_week_value = wday(date_value),
              day_of_week_name = wday(date_value, label = TRUE, abbr = FALSE),
              month_name = month(date_value, label = TRUE, abbr = FALSE)) %>% 
    select(date_sk, school_year_sk, everything())
  return(d)
}

create_school_year <- function(dates) {
  # x should be dimDate or equivalent
  s <- dates %>% 
    mutate(school_year_value = school_year_sk) %>% 
    select(school_year_sk, school_year_value) %>% 
    distinct()
  return(s)
}

create_factGame <- function(football, dates, text_match) {
  # football should be clean, transformed football data
  # dates should be the dimDate table or equivalent
  # text_match should be the final fuzzy text matching data
  # get SKs
  school_opponent_sks <- dbGetQuery(con, "SELECT school_sk, school_name
                                    FROM FootballSchoolDW.dimSchool")
  
  # create unique game id by concantenating the school year with the original game number
  final_football_data <- football %>% 
    left_join(select(dates, date_value, date_sk, school_year_sk), by = c("game_date" = "date_value")) %>%
    unite(game_id, school_year_sk, game_number, sep = "") %>%
    mutate_at(vars(game_id), as.integer) %>% 
    left_join(text_match, by = c("school" = "football_name")) %>% 
    rename(school_lookup = school_name) %>% 
    left_join(school_opponent_sks, by = c("school_lookup" = "school_name")) %>% 
    left_join(text_match, by = c("opponent" = "football_name")) %>% 
    rename(opponent_lookup = school_name) %>% 
    left_join(school_opponent_sks, by = c("opponent_lookup" = "school_name")) %>% 
    rename(school_sk = school_sk.x, opponent_sk = school_sk.y) 
  
  if (any(is.na(final_football_data$school_sk))) { 
    warning("Some school SKs are NA. Check before loading data into database.", call. = FALSE)
  }
  if (any(is.na(final_football_data$opponent_sk))) { 
    warning("Some opponent SKs are NA. Check before loading data into database.", call. = FALSE)
  }
  
  winner <- final_football_data %>% 
    select(school_sk,
           opponent_sk,
           date_sk,
           game_id,
           school_points,
           opponent_points,
           school_win,
           football_notes,
           school_rank,
           opponent_rank,
           school_game_site,
           opponent_game_site,
           bowl, 
           bowl_flag,
           national_championship_flag)
  
  loser <- final_football_data %>%
    select(opponent_sk,
           school_sk,
           date_sk,
           game_id,
           opponent_points,
           school_points,
           opponent_win,
           football_notes,
           opponent_rank,
           school_rank,
           opponent_game_site,
           school_game_site,
           bowl,
           bowl_flag,
           national_championship_flag)
  
  # I intentionally want to bind these two tibbles together by the column orders specified above so I get a row for the school and a row for the opponent in the final table
  final_football_ordered <- rbindlist(list(winner, loser)) %>% 
    as_tibble() %>% 
    arrange(game_id)
  return(final_football_ordered)
}

create_factSchool <- function(school, missing_data, school_year, text_match) {
  # school should be clean, transformed school data
  # missing_data is the missing school data
  # school_year should be the dimSchoolYear table or equivalent
  # text_match should be the final fuzzy text matching data
  # get SKs
  school_sks <- dbGetQuery(con, "SELECT school_sk, school_name 
                           FROM FootballSchoolDW.dimSchool")
  
  final_school_data <- school %>%
    semi_join(text_match, by = "school_name") %>% 
    bind_rows(missing_data) %>% 
    left_join(school_sks, by = "school_name") %>% 
    left_join(school_year, by = c("school_year_start" = "school_year_value")) %>%
    select(school_sk, 
           school_year_sk, 
           school_admission_rate, 
           school_in_state_price,
           school_out_state_price,
           school_retention_rate,
           school_graduation_rate_4yrs,
           school_graduation_rate_6yrs,
           school_federal_loan_rate,
           school_students_with_any_loan,
           school_median_debt_graduates,
           school_median_debt_graduates_monthly_payments,
           SAT_reading_25th_percentile, 
           SAT_reading_75th_percentile,
           SAT_math_25th_percentile,
           SAT_math_75th_percentile,
           SAT_reading_midpoint,
           SAT_math_midpoint,
           ACT_composite_25th_percentile,
           ACT_composite_75th_percentile,
           ACT_english_25th_percentile,
           ACT_english_75th_percentile,
           ACT_math_25th_percentile,
           ACT_math_75th_percentile,
           ACT_composite_midpoint,
           ACT_english_midpoint,
           ACT_math_midpoint)
  
  if (any(is.na(final_school_data$school_sk))) { 
    warning("Some school SKs are NA. Check before loading data into database.", call. = FALSE)
  }
  return(final_school_data)
}