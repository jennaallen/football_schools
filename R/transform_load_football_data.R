football_data_transform <- football_data_staging #create a copy of the staging data for transformations

football_data_transform <- football_data_transform[football_data_transform$Rk != "Rk" & football_data_transform$Notes != "Game Cancelled",] #remove rows where the headers are repeated
football_data_transform$winner_rk <- str_trim(gsub("[^0-9]", "", football_data_transform$Winner), "right")
football_data_transform$Winner <- str_trim(gsub("\\(\\d{1,2}\\)", "", football_data_transform$Winner, "left"))
football_data_transform$loser_rk <- str_trim(gsub("[^0-9]", "", football_data_transform$Loser), "right")
football_data_transform$Loser <- str_trim(gsub("\\(\\d{1,2}\\)", "", football_data_transform$Loser, "left"))
football_data_transform$winner_home <- ifelse(football_data_transform$V1 == "@", "away", 
                                              ifelse(football_data_transform$Notes == "", "home", "unknown"))

table(football_data_transform$Notes)
