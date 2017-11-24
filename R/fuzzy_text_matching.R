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
exact_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$cosine == 0]
close_text_matches <- fuzzy_text_results_all[fuzzy_text_results_all$jw < 0.20]


## Hypothetically assumed threshold to predict the suspicious match cases
## Cosine score < 0.20 and qgram score < 10
## To avoid exact match from the dataset; Remove cosine score = 0.00
suspicious_match <- ndf[ndf$cosine < 0.20 & ndf$cosine != 0 & ndf$qgram < 10, ]
suspicious_match <- suspicious_match[order(suspicious_match$n1,suspicious_match$cosine),]
head(suspicious_match)
