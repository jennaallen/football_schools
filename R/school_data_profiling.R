school_data_character_lengths <- lapply(school_data_transform, function(x) max(nchar(x), na.rm = TRUE))
summary(select_if(school_data_transform, function(col) is.integer(col) || is.numeric(col)))

football_data_character_lengths <- lapply(football_data_transform, function(x) max(nchar(x), na.rm = TRUE))
summary(select_if(football_data_transform, is.integer))
