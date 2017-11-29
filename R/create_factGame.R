create_factGame <- function() {
  #get SKs
  school_opponent_sks <- dbGetQuery(con, "SELECT school_sk, lower(school_name)as school_name FROM FootballSchoolDW.dimSchool")
  
  #create unique game id by concantenating the school year with the original game id
  football_data_transform <- left_join(football_data_transform, dimDate[ , c("date_value","school_year_sk")], by = c("game_date" = "date_value"))
  football_data_transform$game_id <- paste0(football_data_transform$school_year_sk, football_data_transform$game_number)
  
  #get school sks
  football_data_transform$school_lookup <- plyr::mapvalues(tolower(football_data_transform$school), from = fuzzy_text_match_final$football_schools, to = fuzzy_text_match_final$schools, warn_missing = FALSE)
  football_data_transform <- left_join(football_data_transform, school_opponent_sks, by = c("school_lookup" = "school_name"))
  
  
  #get opponent sks
  football_data_transform$opponent_lookup <- plyr::mapvalues(tolower(football_data_transform$opponent), from = fuzzy_text_match_final$football_schools, to = fuzzy_text_match_final$schools, warn_missing = FALSE)
  football_data_transform <- left_join(football_data_transform, school_opponent_sks, by = c("opponent_lookup" = "school_name"))
   football_data_transform$opponent_sk <- plyr::mapvalues(tolower(football_data_transform$opponent_lookup), from = school_opponent_sks$school_name, to = school_opponent_sks$school_sk)
  
   football_data_transform[is.na(football_data_transform$school_sk.y), ]
  #get date sk
  football_data_transform$date_sk <- plyr::mapvalues(football_data_transform$game_date, from = dimDate$date_value, to = dimDate$date_sk)
   
}

str(dimDate)
str(football_data_transform)
