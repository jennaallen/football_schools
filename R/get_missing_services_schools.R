column_names <- c('SchoolID', 'SchoolName', 'SchoolAlias', 'SchoolCity', 'SchoolZip', 'SchoolState', 'OnlineAppWebAddress', 'SchoolWebAddr', 'SchoolLongitude', 'SchoolLatitude', 'SchoolGeographicRegion', 'SchoolSector', 'SchoolLevel', 'SchoolControl', 'SchoolSize', 'UndergradsReceivingStudentLoadAid', 'UndergradAvgStudentLoanAid', 'FTFTAvgInstitutionalGrantAid', 'FTFTReceivingGrantAid', 'FTFTReceivingStudentLoadAid', 'FTFTAvgStudentLoanAid', 'GraduationRate', 'NCAAFootball', 'NCAA', 'SATReading25thPercentile', 'SATReading75thPercentile', 'SATMath25thPercentile', 'SATMath75thPercentile', 'ACTComposite25thPercentile', 'ACTComposite75thPercentile', 'ACTEnglish25thPercentile', 'ACTEnglish75thPercentile', 'ACTMath25thPercentile', 'ACTMath75thPercentile', 'InStatePrice', 'OutofStatePrice', 'Admitted', 'RetentionRate', 'FallCohort', 'StudenttoFacultyRatio', 'PercentFreshman', 'Blank' )
column_names <- c('SchoolID', 'SchoolName', 'SchoolAlias', 'SchoolCity', 'SchoolZip', 'SchoolState', 'OnlineAppWebAddress', 'SchoolWebAddr', 'SchoolLongitude', 'SchoolLatitude', 'SchoolGeographicRegion', 'SchoolSector', 'SchoolLevel', 'SchoolControl', 'SchoolSize', 'UndergradsReceivingStudentLoadAid', 'UndergradAvgStudentLoanAid', 'FTFTAvgInstitutionalGrantAid', 'FTFTReceivingGrantAid', 'FTFTReceivingStudentLoadAid', 'FTFTAvgStudentLoanAid', 'GraduationRate', 'NCAAFootball', 'NCAA', 'InStatePrice', 'OutofStatePrice', 'Admitted', 'RetentionRate', 'FallCohort', 'StudenttoFacultyRatio', 'PercentFreshman', 'SATReading25thPercentile', 'SATReading75thPercentile', 'SATMath25thPercentile', 'SATMath75thPercentile', 'ACTComposite25thPercentile', 'ACTComposite75thPercentile', 'ACTEnglish25thPercentile', 'ACTEnglish75thPercentile', 'ACTMath25thPercentile', 'ACTMath75thPercentile', 'Blank' )

get_missing_data <- function (folder, column_names, column_pos) {
  setwd(paste0("/Users/jallen/documents/dataprojects/football_schools/school_data/",folder))
  # Get the files names
  school_file_names <- list.files(pattern = "*.csv")
  #Merge all school data files
  school_data_staging <- rbindlist(lapply(school_file_names, function(x) {
    data.frame(id = basename(x), 
               fread(x, stringsAsFactors = FALSE, skip = 1, header = FALSE, select = column_pos, col.names = column_names))
  })) 
  return(school_data_staging)
}


missing_school_data_1 <- get_missing_data("column_order1",c("school_id",
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
                                   "school_retention_rate") , c(1:6, 8:11, 13:15, 22, 25:34, 37:38))

missing_school_data_2 <- get_missing_data("column_order2",c("school_id",
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
                                                            "ACT_math_75th_percentile") , c(1:6, 8:11, 13:15, 22, 27:28, 32:41))






get_missing_data1 <- function (column_names, column_pos) {
  setwd("/Users/jallen/documents/dataprojects/football_schools/school_data/column_order1")
  # Get the files names
  school_file_names <- list.files(pattern = "*.csv")
  column_names <- c('SchoolID', 'SchoolName', 'SchoolAlias', 'SchoolCity', 'SchoolZip', 'SchoolState', 'OnlineAppWebAddress', 'SchoolWebAddr', 'SchoolLongitude', 'SchoolLatitude', 'SchoolGeographicRegion', 'SchoolSector', 'SchoolLevel', 'SchoolControl', 'SchoolSize', 'UndergradsReceivingStudentLoadAid', 'UndergradAvgStudentLoanAid', 'FTFTAvgInstitutionalGrantAid', 'FTFTReceivingGrantAid', 'FTFTReceivingStudentLoadAid', 'FTFTAvgStudentLoanAid', 'GraduationRate', 'NCAAFootball', 'NCAA', 'SATReading25thPercentile', 'SATReading75thPercentile', 'SATMath25thPercentile', 'SATMath75thPercentile', 'ACTComposite25thPercentile', 'ACTComposite75thPercentile', 'ACTEnglish25thPercentile', 'ACTEnglish75thPercentile', 'ACTMath25thPercentile', 'ACTMath75thPercentile', 'InStatePrice', 'OutofStatePrice', 'Admitted', 'RetentionRate', 'FallCohort', 'StudenttoFacultyRatio', 'PercentFreshman', 'Blank' )
  #Merge all school data files
  school_data_staging <- rbindlist(lapply(school_file_names, function(x) {
    data.frame(id = basename(x), 
               fread(x, stringsAsFactors = FALSE, skip = 1, header = FALSE, col.names = column_names))
  })) 
  school_data_staging$Blank <- NULL
  school_data_staging$FallCohort <- NULL
  school_data_staging$PercentFreshman <- NULL
  return(school_data_staging)
}

get_missing_data2 <- function () {
  setwd("/Users/jallen/documents/dataprojects/football_schools/school_data/column_order2")
  # Get the files names
  school_file_names <- list.files(pattern = "*.csv")
  column_names <- c('SchoolID', 'SchoolName', 'SchoolAlias', 'SchoolCity', 'SchoolZip', 'SchoolState', 'OnlineAppWebAddress', 'SchoolWebAddr', 'SchoolLongitude', 'SchoolLatitude', 'SchoolGeographicRegion', 'SchoolSector', 'SchoolLevel', 'SchoolControl', 'SchoolSize', 'UndergradsReceivingStudentLoadAid', 'UndergradAvgStudentLoanAid', 'FTFTAvgInstitutionalGrantAid', 'FTFTReceivingGrantAid', 'FTFTReceivingStudentLoadAid', 'FTFTAvgStudentLoanAid', 'GraduationRate', 'NCAAFootball', 'NCAA', 'InStatePrice', 'OutofStatePrice', 'Admitted', 'RetentionRate', 'FallCohort', 'StudenttoFacultyRatio', 'PercentFreshman', 'SATReading25thPercentile', 'SATReading75thPercentile', 'SATMath25thPercentile', 'SATMath75thPercentile', 'ACTComposite25thPercentile', 'ACTComposite75thPercentile', 'ACTEnglish25thPercentile', 'ACTEnglish75thPercentile', 'ACTMath25thPercentile', 'ACTMath75thPercentile', 'Blank' )
  #Merge all school data files
  school_data_staging <- rbindlist(lapply(school_file_names, function(x) {
    data.frame(id = basename(x), 
               fread(x, stringsAsFactors = FALSE, skip = 1, header = FALSE, col.names = column_names))
  })) 
  school_data_staging$Blank <- NULL
  school_data_staging$FallCohort <- NULL
  school_data_staging$PercentFreshman <- NULL
  return(school_data_staging)
}


missing_school_data1 <- get_missing_data1()
missing_school_data2 <- get_missing_data2()


missing_school_data_serviceschools1 <- missing_school_data1[missing_school_data1$SchoolID %in% c(197036, 128328, 164155), c(1:7,9:12, 14:16, 23, 38:39, 26:35) ]
missing_school_data_serviceschools2 <- missing_school_data2[missing_school_data2$SchoolID %in% c(197036, 128328, 164155), c(1:7,9:12, 14:16, 23, 28:29, 31:40) ]

missing_school_data_final <- rbindlist(list(missing_school_data_serviceschools1, missing_school_data_serviceschools2))
missing_school_data_final$SchoolGeographicRegion <- revalue(as.character(missing_school_data_final$SchoolGeographicRegion), c("0"="US Service schools"))
missing_school_data_final$SchoolLevel <- revalue(as.character(missing_school_data_final$SchoolLevel), c("1"="4-year"))
missing_school_data_final$SchoolControl <- revalue(as.character(missing_school_data_final$SchoolControl), c("1"="Public"))
missing_school_data_final$SchoolSize <- revalue(as.character(missing_school_data_final$SchoolSize), c("2"="1,000 - 4,999"))
missing_school_data_final$year <- as.integer(str_sub(missing_school_data_final$id, end = 4))
missing_school_data_final$school_state_full <- revalue(missing_school_data_final$SchoolState, c("CO"="Colorado", "NY"="New York", "MD" ="Maryland"))
missing_school_data_final$main_campus_flag <- 1
missing_school_data_final$GraduationRate <- missing_school_data_final$GraduationRate/100
missing_school_data_final$Admitted <- missing_school_data_final$Admitted/100
missing_school_data_final$RetentionRate <- missing_school_data_final$RetentionRate/100


missing_school_data_final_load <- missing_school_data_final[missing_school_data_final$year %in% 2015, c("SchoolID",
                                                                                                   "SchoolName",
                                                                                                   "SchoolAlias",
                                                                                                   "SchoolCity",
                                                                                                   "SchoolState",
                                                                                                   "school_state_full",
                                                                                                   "SchoolZip",
                                                                                                   "SchoolGeographicRegion",
                                                                                                   "SchoolLongitude",
                                                                                                   "SchoolLatitude",
                                                                                                   "main_campus_flag",
                                                                                                   "SchoolSize",
                                                                                                   "SchoolWebAddr",
                                                                                                   "SchoolControl",
                                                                                                   "SchoolLevel")]

setnames(missing_school_data_final_load, old = c("SchoolID",
                                                 "SchoolName",
                                                 "SchoolAlias",
                                                 "SchoolCity",
                                                 "SchoolState",
                                                 "school_state_full",
                                                 "SchoolZip",
                                                 "SchoolGeographicRegion",
                                                 "SchoolLongitude",
                                                 "SchoolLatitude",
                                                 "main_campus_flag",
                                                 "SchoolSize",
                                                 "SchoolWebAddr",
                                                 "SchoolControl",
                                                 "SchoolLevel"), new = c("school_id",
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
                                                                         "school_level"))
dbWriteTable(conn = con, name = 'dimSchool', value = missing_school_data_final_load, append = TRUE, row.names = FALSE)



