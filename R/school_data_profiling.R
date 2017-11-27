str(school_data_transform)

school_data_character_lengths <- lapply(school_data_transform, function(x) max(nchar(x), na.rm = TRUE))

str(football_data_transform)
football_data_character_lengths <- lapply(football_data_transform, function(x) max(nchar(x), na.rm = TRUE))
summary(school_data_transform[ , sapply(school_data_transform, is.integer) | sapply(school_data_transform, is.numeric)])

