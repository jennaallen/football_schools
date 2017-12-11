# there are some colleges that have the same name but are in different locations, none of these correspond to the football data. If they did there would be a problem because name is not unique

fuzzy_text_matching <- function(football_text = NULL, before_after = NULL) {
  # get unique school names
  unique_school_names <- data.table(schools = unique(school_data_transform[school_data_transform$school_year_start %in% 2015 & school_data_transform$school_main_campus_flag %in% 1, "school_name"]))
  
  # get unique football school names by combining values in the school and opponent columns into one column
  unique_football_names <- unique(data.table(football_schools = c(as.matrix(football_data_transform[, c("school", "opponent")]))))
  
  # use stringdist for fuzzy text matching; run through all possible methods used for distance calculation
  # cross join football names with all school names (227 football names * 2187 school names = 496449 possible combinations)
  fuzzy_text_results_all <- CJ(football_schools = tolower(unique_football_names$football_schools), schools = tolower(unique_school_names$schools))
  if(is.null(football_text) & is.null(before_after)) {
    method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    for (i in method_list) {
      fuzzy_text_results_all[,i] <- stringdist(fuzzy_text_results_all$football_schools, fuzzy_text_results_all$schools, method = i)
    }
  } else if (is.null(football_text) & !is.null(before_after)) {
    stop("Text to concantenate with football name is missing")
  } else if (!is.null(football_text) & is.null(before_after)) {
    stop("Please specify if text should be concantenated before or after the football name")
  } else if (!(before_after %in% c("before", "after"))) {
    stop("The second function argument must hava a value of before or after")
  } else if (before_after == "before") {
    fuzzy_text_results_all$football_schools <- paste(tolower(football_text), fuzzy_text_results_all$football_schools)
    method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    for (i in method_list) {
      fuzzy_text_results_all[,i] <- stringdist(fuzzy_text_results_all$football_schools, fuzzy_text_results_all$schools, method = i)
    }
    fuzzy_text_results_all$football_schools <- str_trim(gsub(tolower(football_text), "", fuzzy_text_results_all$football_schools), side = "left")
  } else {
    fuzzy_text_results_all$football_schools <- paste(fuzzy_text_results_all$football_schools, tolower(football_text))
    method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    for (i in method_list) {
      fuzzy_text_results_all[,i] <- stringdist(fuzzy_text_results_all$football_schools, fuzzy_text_results_all$schools, method = i)
    }
    fuzzy_text_results_all$football_schools <- str_trim(gsub(tolower(football_text), "", fuzzy_text_results_all$football_schools), side = "right")
  }
  return(fuzzy_text_results_all)
}

fuzzy_text_results_all <- fuzzy_text_matching()


# filter data based on exact matches
exact_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$cosine == 0, ]

# for this text it seems that filtering on jw provides the best results for close matches
text_matches_for_review <- fuzzy_text_results_all[fuzzy_text_results_all$jw < 0.20 & !(fuzzy_text_results_all$football_schools %in% exact_text_matches$football_schools), ]

# add column for user to manually update with 0 if the match is no good
text_matches_for_review$correct_match <- 1
text_matches_for_review <- edit(text_matches_for_review)

#this is an iterative process where you can add the correct matches to a final match data table until all schools are matched
fuzzy_text_match_final <- exact_text_matches[ , c("football_schools", "schools")]
fuzzy_text_match_final <- rbindlist(list(fuzzy_text_match_final, text_matches_for_review[text_matches_for_review$correct_match %in% 1, c("football_schools", "schools")]))

fuzzy_text_results_conc_before <- fuzzy_text_matching("university of", "before")
# remove the results that have already been matched
fuzzy_text_results_conc_before <- fuzzy_text_results_conc_before[!(fuzzy_text_results_conc_before$football_schools %in% fuzzy_text_match_final$football_schools), ]

# add column for user to manually update with 0 if the match is no good
fuzzy_text_results_conc_before_review <- fuzzy_text_results_conc_before[fuzzy_text_results_conc_before$jw < 0.1, ]
fuzzy_text_results_conc_before_review$correct_match <- 1
fuzzy_text_results_conc_before_review <- edit(fuzzy_text_results_conc_before_review)

#this is an iterative process where you can add the correct matches to a final match data table until all schools are matched
fuzzy_text_match_final <- rbindlist(list(fuzzy_text_match_final, fuzzy_text_results_conc_before_review[fuzzy_text_results_conc_before_review$correct_match %in% 1, c("football_schools", "schools")]))


