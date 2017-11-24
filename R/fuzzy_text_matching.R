#get unique school names
#there are some colleges that have the same name but are in different locations, but for matching school name we only need one instance of the name
unique_school_names <- data.table(schools = unique(school_data_transform[school_data_transform$school_year_start == 2015 & school_data_transform$school_main_campus_flag == 1, "school_name"]))
unique_school_alias <- data.table(school_alias = unique(school_data_transform[school_data_transform$school_year_start == 2015, "school_alias"]))

#get unique football school names by combining values in the school and opponent columns into one column
unique_football_names <- data.table(football_schools = c(as.matrix(football_data_transform[, c("school", "opponent")])))
unique_football_names <- unique(unique_football_names)

agrep2 <- Vectorize(agrep, vectorize.args = "pattern")
fuzzy_text_matches <- agrep2(unique_football_names$football_schools, unique_school_names$schools, max.distance = 0, ignore.case = TRUE, value = TRUE)
fuzzy_text_matches_values <- agrep2(unique_football_names$football_schools, unique_school_names$schools, max.distance = 0, ignore.case = TRUE, value = FALSE)

#no
agrepl2 <- Vectorize(agrepl, vectorize.args = "pattern")
fuzzy_text_matches2 <- agrepl2(unique_football_names$football_schools, unique_school_names$schools, max.distance = 0, ignore.case = TRUE)


fuzzy_text_matches3 <- stringdist(tolower(unique_football_names$football_schools), tolower(unique_school_names$schools))

table(fuzzy_names$football_schools)
class(ndf)
#fuzzy_names <- cbind(football_schools = tolower(unique_football_names$football_schools), schools = tolower(unique_school_names$schools))
#fuzzy_names <- data.table(football_schools = tolower(unique_football_names$football_schools), schools = tolower(unique_school_names$schools))
#ndf <- expand.grid(lapply(fuzzy_names, levels)) 
ndf <- CJ(tolower(unique_football_names$football_schools), tolower(unique_school_names$schools))
#ndf <- ndf[order(ndf$n1),]
method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
for( i in method_list){
  
  ndf[,i] <- stringdist(ndf$V1,ndf$V2,method=i)
  
}

## Hypothetically assumed threshold to predict the suspicious match cases
## Cosine score < 0.20 and qgram score < 10
## To avoid exact match from the dataset; Remove cosine score = 0.00
suspicious_match <- ndf[ndf$cosine < 0.20 & ndf$cosine != 0 & ndf$qgram < 10, ]
suspicious_match <- suspicious_match[order(suspicious_match$n1,suspicious_match$cosine),]
head(suspicious_match)
