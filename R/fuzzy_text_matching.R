#get unique school names
#there are some colleges that have the same name but are in different locations, but for matching school name we only need one instance of the name
unique_school_names <- data.table(schools = unique(school_data_transform[school_data_transform$school_year_start == 2015 & school_data_transform$school_main_campus_flag == 1, "school_name"]))
unique_school_alias <- data.table(school_alias = unique(school_data_transform[school_data_transform$school_year_start == 2015, "school_alias"]))

#get unique football school names by combining values in the school and opponent columns into one column
unique_football_names <- data.table(football_schools = c(as.matrix(football_data_transform[, c("school", "opponent")])))
unique_football_names <- unique(unique_football_names)

#use stringdist for fuzzy text matching; run through all possible methods used for distance calculation
#cross join football names with all school names (227 football names * 2187 school names = 496449 possible combinations)
fuzzy_text_results_all <- CJ(football_schools = tolower(unique_football_names$football_schools), schools = tolower(unique_school_names$schools))
method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
for( i in method_list){
  
  fuzzy_text_results_all[,i] <- stringdist(fuzzy_text_results_all$football_schools, fuzzy_text_results_all$schools, method = i)
  
}

#filter data based on exact and close matches
exact_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$cosine == 0, ]
#for this text it seems that filtering on jw provides the best results
close_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$jw < 0.20 & !(fuzzy_text_results_all$football_schools %in% exact_text_matches$football_schools), ]
close_text_matches$correct_match <- 1
edit(close_text_matches)
unmatched_text <- fuzzy_text_results_all[!(fuzzy_text_results_all$football_schools %in% exact_text_matches$football_schools) & !(fuzzy_text_results_all$football_schools %in% close_text_matches$football_schools), ]
unmatched_text_close <- unmatched_text[unmatched_text$jw < 0.29, ]

fuzzy_text_match_final <- data.table(exact_text_matches$football_schools, exact_text_matches$schools)
