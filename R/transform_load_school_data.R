clean_school_data <- function(original_school_data) {
  # decode region, control, level, and state abbreviations
  lookups <- fread("lookups.csv", stringsAsFactors = FALSE)
  lookups$VariableName <- gsub("\\s\\([^)]*\\)", "", lookups$VariableName, perl = TRUE) # remove parentheses after field name using regex
  
  # subset lookup values for region, control, level, and state abbr
  control <- lookups[lookups$VariableName %in% "Control of institution", ]
  level <- lookups[lookups$VariableName %in% "Level of institution", ]
  regions <- lookups[lookups$VariableName %in% "Bureau of Economic Analysis regions", ]
  states <- lookups[lookups$VariableName %in% "State abbreviation", ]
  
  # perform lookups
  school_data_transform <- original_school_data # make a copy of the original data so we can go back to it if we need to 
  school_data_transform$region <- plyr::mapvalues(school_data_transform$region, from = regions$Value, to = regions$ValueLabel)
  school_data_transform$control <- plyr::mapvalues(school_data_transform$control, from = control$Value, to = control$ValueLabel)
  school_data_transform$iclevel <- plyr::mapvalues(school_data_transform$iclevel, from = level$Value, to = level$ValueLabel)
  school_data_transform <- left_join(school_data_transform, states[,2:3], by = c("stabbr" = "Value"))
  
  # clean up region value using regex
  school_data_transform$region_clean <- str_trim(gsub("((?!US))(\\b[[:alpha:]]{2}\\b)", " ", school_data_transform$region, perl = TRUE), side = "right")
  
  # keep only the first 5 numbers for zip code; some values are missing dash between zip and zip +4 code
  school_data_transform$zip <- str_sub(school_data_transform$zip, 1, 5)
  
  # create field for size category
  school_data_transform$school_size_category <- ifelse(school_data_transform$ugds < 1000, "Under 1,000",
                                                       ifelse(between(school_data_transform$ugds, 1000, 4999),"1,000 - 4,999",
                                                              ifelse(between(school_data_transform$ugds, 5000, 9999), "5,000 - 9,999",
                                                                     ifelse(between(school_data_transform$ugds, 10000, 19999),"10,000 - 19,999",
                                                                            ifelse(school_data_transform$ugds > 20000, "20,000 and above", NA)))))
  
  # rename columns to give more readable names
  old_school_names <- names(school_data_transform)
  new_school_names <- c("school_region_all",
                        "school_longitude", 
                        "school_main_campus_flag", 
                        "school_name", 
                        "school_city", 
                        "school_control", 
                        "school_level", 
                        "school_zip", 
                        "school_latitude", 
                        "school_opeid6", 
                        "school_url", 
                        "school_opeid8", 
                        "school_st_abbr", 
                        "school_id", 
                        "school_admission_rate", 
                        "SAT_reading_25th_percentile", 
                        "SAT_reading_75th_percentile", 
                        "SAT_math_25th_percentile", 
                        "SAT_math_75th_percentile", 
                        "SAT_reading_midpoint", 
                        "SAT_math_midpoint", 
                        "ACT_composite_25th_percentile", 
                        "ACT_composite_75th_percentile", 
                        "ACT_english_25th_percentile",
                        "ACT_english_75th_percentile",
                        "ACT_math_25th_percentile",
                        "ACT_math_75th_percentile",
                        "ACT_composite_midpoint",
                        "ACT_english_midpoint",
                        "ACT_math_midpoint",
                        "school_size",
                        "school_in_state_price",
                        "school_out_state_price",
                        "school_graduation_rate_6yrs",
                        "school_retention_rate",
                        "school_federal_loan_rate",
                        "school_median_debt_graduates",
                        "school_median_debt_graduates_monthly_payments",
                        "school_students_with_any_loan",
                        "school_alias",
                        "school_graduation_rate_4yrs",
                        "school_year_start",
                        "school_state",
                        "school_region",
                        "school_size_category")
  setnames(school_data_transform, old = old_school_names, new = new_school_names)
  return(school_data_transform)
}

school_data_transform <- clean_school_data(school_data_staging_api)


# filter for only 2015 data and only columns needed for dimSchool
dimSchool <- school_data_transform[school_data_transform$school_year_start %in% 
                                     max(school_data_transform$school_year_start) & tolower(school_data_transform$school_name) %in% fuzzy_text_match_final$schools, 
                                   c("school_id",
                                     "school_name",
                                     "school_alias",
                                     "school_city",
                                     "school_st_abbr",
                                     "school_state",
                                     "school_zip",
                                     "school_region",
                                     "school_longitude",
                                     "school_latitude",
                                     "school_main_campus_flag",
                                     "school_size_category",
                                     "school_url",
                                     "school_control",
                                     "school_level")]
# union in missing school data
dimSchool <- rbindlist(list(dimSchool, missing_school_data[missing_school_data$school_year_start %in% 
                                                             max(missing_school_data$school_year_start), c("school_id",
                                                                                                           "school_name",
                                                                                                           "school_alias",
                                                                                                           "school_city",
                                                                                                           "school_st_abbr",
                                                                                                           "school_state",
                                                                                                           "school_zip",
                                                                                                           "school_region",
                                                                                                           "school_longitude",
                                                                                                           "school_latitude",
                                                                                                           "school_main_campus_flag",
                                                                                                           "school_size_category",
                                                                                                           "school_url",
                                                                                                           "school_control",
                                                                                                           "school_level")]))

dbWriteTable(con, name = "dimSchool", value = dimSchool , append = TRUE, row.names = FALSE)