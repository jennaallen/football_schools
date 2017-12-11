# create function for cleaning football data

clean_football_data <- function(all_games, bowls) {
  # create a copy of the staging data for transformations and replace blank values with NAs
  football_data_transform <- all_games
  is.na(football_data_transform) <- football_data_transform == ""
  
  # dataframe has two columns with "Pts" name so renaming one here to avoid issues
  names(football_data_transform)[6] <- "school_points"
  
  # remove rows where the headers are repeated and games were cancelled
  football_data_transform <- football_data_transform[!(football_data_transform$Rk %in% "Rk") & !(football_data_transform$Notes %like% "Cancelled"), ] 
  
  # change column data types
  football_data_transform$Rk <- as.integer(football_data_transform$Rk)
  football_data_transform$school_points <- as.integer(football_data_transform$school_points)
  football_data_transform$Pts <- as.integer(football_data_transform$Pts)
  football_data_transform$Date <- as.Date(football_data_transform$Date, format = "%b %d, %Y")
  
  # use regex to remove the rank from the school name and put rank in a new column
  football_data_transform$school_rk <- as.integer(str_trim(gsub("[^0-9]", "", football_data_transform$Winner), "right"))
  football_data_transform$Winner <- str_trim(gsub("\\(\\d{1,2}\\)", "", football_data_transform$Winner, "left"))
  football_data_transform$opponent_rk <- as.integer(str_trim(gsub("[^0-9]", "", football_data_transform$Loser), "right"))
  football_data_transform$Loser <- str_trim(gsub("\\(\\d{1,2}\\)", "", football_data_transform$Loser, "left"))
  
  # create fields indicating if the winners and losers were at home, away, or at a neutral site
  football_data_transform$school_home <- ifelse(football_data_transform$V1 %in% "@", "away", 
                                                ifelse(is.na(football_data_transform$Notes), "home", "neutral")) 
  football_data_transform$opponent_home <- ifelse(football_data_transform$school_home %in% "away", "home",
                                                  ifelse(football_data_transform$school_home %in% "home", "away", "neutral"))
  
  #create flags for bowl games and national championship games
  bowls$Date <- as.Date(bowls$Date)
  football_data_transform <- left_join(football_data_transform, bowls[, 1:3], by = c("Date" = "Date", "Winner" = "Winner"))
  football_data_transform$bowl_flag <- ifelse(!is.na(football_data_transform$Bowl), 1, 0)
  football_data_transform$national_championship_flag <- ifelse(football_data_transform$Bowl %like% "Championship", 1, 0)
  
  # rename columns to give more readable names
  old_football_names <- names(football_data_transform)
  new_football_names <- c("game_number",
                          "football_week",
                          "game_date", 
                          "game_day", 
                          "school",
                          "school_points",
                          "home_away",
                          "opponent",
                          "opponent_points",
                          "football_notes",
                          "game_time",
                          "game_tv",
                          "school_rank",
                          "opponent_rank",
                          "school_game_site",
                          "opponent_game_site",
                          "bowl",
                          "bowl_flag",
                          "national_championship_flag")
  setnames(football_data_transform, old = old_football_names, new = new_football_names)
  return(football_data_transform) 
}

football_data_transform <- clean_football_data(football_data_staging, football_data_staging_bowls)

   