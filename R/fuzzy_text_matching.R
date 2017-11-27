fuzzy_text_matching <- function(football_text = NULL, before_after = NULL) {
  #get unique school names
  #there are some colleges that have the same name but are in different locations, but for matching school name we only need one instance of the name
  unique_school_names <- data.table(schools = unique(school_data_transform[school_data_transform$school_year_start == 2015 & school_data_transform$school_main_campus_flag == 1, "school_name"]))
  
  #get unique football school names by combining values in the school and opponent columns into one column
  unique_football_names <- unique(data.table(football_schools = c(as.matrix(football_data_transform[, c("school", "opponent")]))))
  
  #use stringdist for fuzzy text matching; run through all possible methods used for distance calculation
  #cross join football names with all school names (227 football names * 2187 school names = 496449 possible combinations)
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
    fuzzy_text_results_all$football_schools <- paste(football_text, fuzzy_text_results_all$football_schools)
    method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    for (i in method_list) {
      fuzzy_text_results_all[,i] <- stringdist(fuzzy_text_results_all$football_schools, fuzzy_text_results_all$schools, method = i)
    }
    fuzzy_text_results_all$football_schools <- str_trim(gsub(football_text, "", fuzzy_text_results_all$football_schools), side = "left")
  } else {
    fuzzy_text_results_all$football_schools <- paste(fuzzy_text_results_all$football_schools, football_text)
    method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    for (i in method_list) {
      fuzzy_text_results_all[,i] <- stringdist(fuzzy_text_results_all$football_schools, fuzzy_text_results_all$schools, method = i)
    }
    fuzzy_text_results_all$football_schools <- str_trim(gsub(football_text, "", fuzzy_text_results_all$football_schools), side = "right")
  }
  return(fuzzy_text_results_all)
}

fuzzy_text_results_all <- fuzzy_text_matching()



#filter data based on exact and close matches
exact_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$cosine == 0, ]
#for this text it seems that filtering on jw provides the best results
close_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$jw < 0.20 & !(fuzzy_text_results_all$football_schools %in% exact_text_matches$football_schools), ]
#add column for user to update with 0 if the match is no good
close_text_matches$correct_match <- 1
#requires manual editing to determine good matches
close_text_matches <- edit(close_text_matches)

#this is an iterative process
fuzzy_text_match_final <- exact_text_matches[ , c("football_schools", "schools")]
fuzzy_text_match_final <- rbindlist(list(fuzzy_text_match_final, close_text_matches[close_text_matches$correct_match == 1, c("football_schools", "schools")]))

#trying to find parameters for the next round of filtering
summary(close_text_matches[close_text_matches$correct_match == 1, ])

unmatched_text <- fuzzy_text_results_all[!(fuzzy_text_results_all$football_schools %in% fuzzy_text_match_final$football_schools), ]
unmatched_text_close <- unmatched_text[unmatched_text$jw < 0.35 & unmatched_text$cosine < 0.4, ]
unmatched_text_close$correct_match <- 0
unmatched_text_close <- edit(unmatched_text_close)

fuzzy_text_match_final <- rbindlist(list(fuzzy_text_match_final, unmatched_text_close[unmatched_text_close$correct_match == 1, c("football_schools", "schools")]))

#concantenate university on all remaining unmatched schools and run text matching again
unmatched_schools <- unmatched_text[, c("football_schools", "schools")]
unmatched_schools$football_schools <- paste("university of", unmatched_schools$football_schools)

method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
for( i in method_list){
  
  unmatched_schools[,i] <- stringdist(unmatched_schools$football_schools, unmatched_schools$schools, method = i)
  
}

unmatched_schools_close <- unmatched_schools[unmatched_schools$jw < 0.11, ]
unmatched_schools_close$correct_match <- 1
unmatched_schools_close <- edit(unmatched_schools_close)

unmatched_schools_close$football_schools <- gsub("university of ", "", unmatched_schools_close$football_schools)
fuzzy_text_match_final <- rbindlist(list(fuzzy_text_match_final, unmatched_schools_close[unmatched_schools_close$correct_match == 1, c("football_schools", "schools")]))

unmatched_schools_2 <- data.table(unique(unmatched_schools$football_schools))
write.csv(unmatched_schools_2, file = "unmatched.csv", row.names = FALSE)
