load_factGame <- function() {
  # get SKs
  school_opponent_sks <- dbGetQuery(con, "SELECT school_sk, LOWER(school_name) AS school_name FROM FootballSchoolDW.dimSchool")
  
  # create unique game id by concantenating the school year with the original game number
  football_data_transform <- left_join(football_data_transform, dimDate[ , c("date_value","school_year_sk")], by = c("game_date" = "date_value"))
  football_data_transform$game_id <- paste0(football_data_transform$school_year_sk, football_data_transform$game_number)
  
  # get school sks and warn user if any lookups result in NA
  football_data_transform$school_lookup <- plyr::mapvalues(tolower(football_data_transform$school), from = fuzzy_text_match_final$football_schools, to = fuzzy_text_match_final$schools, warn_missing = FALSE)
  football_data_transform <- left_join(football_data_transform, school_opponent_sks, by = c("school_lookup" = "school_name"))
  if (any(is.na(football_data_transform$school_sk))) { 
    warning("Some school SKs are NA. Check before loading data into database.")
  }
  
  # get opponent sks and warn user if any lookups result in NA
  football_data_transform$opponent_lookup <- plyr::mapvalues(tolower(football_data_transform$opponent), from = fuzzy_text_match_final$football_schools, to = fuzzy_text_match_final$schools, warn_missing = FALSE)
  football_data_transform <- left_join(football_data_transform, school_opponent_sks, by = c("opponent_lookup" = "school_name"))
  if (any(is.na(football_data_transform$school_sk.y))) { 
    warning("Some opponent SKs are NA. Check before loading data into database.")
  }
  # rename school_sk and opponent_sk columns
  setnames(football_data_transform, old = c("school_sk.x", "school_sk.y"), new = c("school_sk", "opponent_sk"))
  
  # get date sk
  football_data_transform <- left_join(football_data_transform, dimDate[, c("date_value","date_sk")], by = c("game_date" = "date_value"))
  
  # filter winner/school data
  football_data_transform$school_win <- 1
  football_data_transform$opponent_win <- 0
  football_data_transform_winner <- football_data_transform[, c("school_sk", "opponent_sk", "date_sk", "game_id", "school_points", "opponent_points", "school_win", "football_notes", "school_rank", "opponent_rank", "school_game_site", "opponent_game_site", "bowl", "bowl_flag", "national_championship_flag")]
  
  # filter loser/opponent data
  football_data_transform_loser <- football_data_transform[, c("opponent_sk", "school_sk", "date_sk", "game_id", "opponent_points", "school_points", "opponent_win", "football_notes", "opponent_rank", "school_rank", "opponent_game_site", "school_game_site", "bowl", "bowl_flag", "national_championship_flag")]
  
  fact_table <- rbindlist(list(football_data_transform_winner, football_data_transform_loser))
  return(fact_table[order(fact_table$game_id), ])
}

factGame <- load_factGame()
dbWriteTable(conn = con, name = 'factGame', value = factGame, append = TRUE, row.names = FALSE)