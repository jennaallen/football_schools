football_data_transform <- football_data_staging #create a copy of the staging data for transformations

names(football_data_transform)[6] <- "school_points" #dataframe has two columns with same name which causes problems down the raod
football_data_transform <- football_data_transform[football_data_transform$Rk != "Rk" & football_data_transform$Notes != "Game Cancelled",] #remove rows where the headers are repeated and games were cancelled
football_data_transform$school_rk <- str_trim(gsub("[^0-9]", "", football_data_transform$Winner), "right") #put winner rank in separate column
football_data_transform$Winner <- str_trim(gsub("\\(\\d{1,2}\\)", "", football_data_transform$Winner, "left")) #remove winner rank from school name
football_data_transform$opponent_rk <- str_trim(gsub("[^0-9]", "", football_data_transform$Loser), "right") #put loser rank in separate column
football_data_transform$Loser <- str_trim(gsub("\\(\\d{1,2}\\)", "", football_data_transform$Loser, "left")) #remove loser rank from school name
football_data_transform$school_home <- ifelse(football_data_transform$V1 == "@", "away", 
                                              ifelse(football_data_transform$Notes == "", "home", "neutral")) #create field for if the winner was at home, away, or at a neutral site
football_data_transform$opponent_home <- ifelse(football_data_transform$school_home == "away", "home",
                                                ifelse(football_data_transform$school_home == "home", "away", "neutral")) #create field for if the loser was at home, away, or at a neutral site


old_football_names <- names(football_data_transform)
new_football_names <- c("game_id",
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
                        "opponent_game_site")

setnames(football_data_transform, old = old_football_names, new = new_football_names)
