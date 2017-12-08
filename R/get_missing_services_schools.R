get_missing_data <- function (folder, column_names, column_pos) {
  setwd(paste0("/Users/jallen/documents/dataprojects/football_schools/school_data/", folder))
  # Get the files names
  school_file_names <- list.files(pattern = "*.csv")
  #Merge all school data files
  school_data_staging <- rbindlist(lapply(school_file_names, function(x) {
    data.frame(id = basename(x), 
               fread(x, stringsAsFactors = FALSE, skip = 1, header = FALSE, select = column_pos, col.names = column_names))
  })) 
  return(school_data_staging)
}

missing_school_data_1 <- get_missing_data("column_order1", c("school_id",
                                                             "school_name",
                                                             "school_alias",
                                                             "school_city",
                                                             "school_zip",
                                                             "school_st_abbr",
                                                             "school_url",
                                                             "school_longitude",
                                                             "school_latitude",
                                                             "school_region",
                                                             "school_level",
                                                             "school_control",
                                                             "school_size_category",
                                                             "school_graduation_rate_6yrs",
                                                             "SAT_reading_25th_percentile", 
                                                             "SAT_reading_75th_percentile",
                                                             "SAT_math_25th_percentile",
                                                             "SAT_math_75th_percentile",
                                                             "ACT_composite_25th_percentile",
                                                             "ACT_composite_75th_percentile",
                                                             "ACT_english_25th_percentile",
                                                             "ACT_english_75th_percentile",
                                                             "ACT_math_25th_percentile",
                                                             "ACT_math_75th_percentile",
                                                             "school_addmission_rate",
                                                             "school_retention_rate"), c(1:6, 8:11, 13:15, 22, 25:34, 37:38))

missing_school_data_2 <- get_missing_data("column_order2", c("school_id",
                                                              "school_name",
                                                              "school_alias",
                                                              "school_city",
                                                              "school_zip",
                                                              "school_st_abbr",
                                                              "school_url",
                                                              "school_longitude",
                                                              "school_latitude",
                                                              "school_region",
                                                              "school_level",
                                                              "school_control",
                                                              "school_size_category",
                                                              "school_graduation_rate_6yrs",
                                                              "school_addmission_rate",
                                                              "school_retention_rate",
                                                              "SAT_reading_25th_percentile", 
                                                              "SAT_reading_75th_percentile",
                                                              "SAT_math_25th_percentile",
                                                              "SAT_math_75th_percentile",
                                                              "ACT_composite_25th_percentile",
                                                              "ACT_composite_75th_percentile",
                                                              "ACT_english_25th_percentile",
                                                              "ACT_english_75th_percentile",
                                                              "ACT_math_25th_percentile",
                                                              "ACT_math_75th_percentile"), c(1:6, 8:11, 13:15, 22, 27:28, 32:41))

transform_missing_data <- function(data1, data2) {
  missing_school_data_serviceschools_1 <- data1[data1$school_id %in% c(197036, 128328, 164155), ]
  missing_school_data_serviceschools_2 <- data2[data2$school_id %in% c(197036, 128328, 164155), c(1:15, 18:27, 16:17)]
  
  missing_school_data_final <- rbindlist(list(missing_school_data_serviceschools_1, missing_school_data_serviceschools_2))
  missing_school_data_final$school_zip <- str_sub(school_data_transform$school_zip, 1, 5)
  missing_school_data_final$school_region <- revalue(as.character(missing_school_data_final$school_region), c("0" = "US Service schools"))
  missing_school_data_final$school_level <- revalue(as.character(missing_school_data_final$school_level), c("1" = "4-year"))
  missing_school_data_final$school_control <- revalue(as.character(missing_school_data_final$school_control), c("1" = "Public"))
  missing_school_data_final$school_size_category <- revalue(as.character(missing_school_data_final$school_size_category), c("2" = "1,000 - 4,999"))
  missing_school_data_final$school_year_start <- as.integer(str_sub(missing_school_data_final$id, end = 4))
  missing_school_data_final$school_state <- revalue(missing_school_data_final$school_st_abbr, c("CO" = "Colorado", "NY" = "New York", "MD" = "Maryland"))
  missing_school_data_final$school_main_campus_flag <- 1
  missing_school_data_final$school_graduation_rate_6yrs <- missing_school_data_final$school_graduation_rate_6yrs/100
  missing_school_data_final$school_addmission_rate <- missing_school_data_final$school_addmission_rate/100
  missing_school_data_final$school_retention_rate <- missing_school_data_final$school_retention_rate/100
  return(missing_school_data_final)
}

missing_school_data <- transform_missing_data(missing_school_data_1, missing_school_data_2)

missing_school_data_dimSchool <- missing_school_data_final[missing_school_data_final$school_year %in% 2015, c("school_id",
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
dbWriteTable(conn = con, name = 'dimSchool', value = missing_school_data_dimSchool, append = TRUE, row.names = FALSE)
