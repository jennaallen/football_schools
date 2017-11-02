#transform school staging data and load into AWS tables dimSchool and factSchool

#decode region, control, level, and state abbreviations
#get lookup table
lookups <- read.csv("lookups.csv", stringsAsFactors = FALSE)
lookups$VariableName <- gsub("\\s\\([^)]*\\)","",lookups$VariableName,perl = TRUE) #remove parentheses after field name

#subset lookup values for region, control, level, and state abbr
attach(lookups)
control <- lookups[VariableName =="Control of institution",]
level <- lookups[VariableName =="Level of institution",]
regions <- lookups[VariableName =="Bureau of Economic Analysis regions",]
states <- lookups[VariableName =="State abbreviation",]
detach(lookups)


#perform lookups
school_data_transform <- school_data_staging_api
school_data_transform$region <- plyr::mapvalues(school_data_transform$region, from = regions$Value, to = regions$ValueLabel)
school_data_transform$control <- plyr::mapvalues(school_data_transform$control, from = control$Value, to = control$ValueLabel)
school_data_transform$iclevel <- plyr::mapvalues(school_data_transform$iclevel, from = level$Value, to = level$ValueLabel)
school_data_transform <- left_join(school_data_transform, states[,2:3], by = c("stabbr" = "Value"))

#clean up region value
school_data_transform$region_clean <- str_trim(gsub("((?!US))(\\b[[:alpha:]]{2}\\b)", " ", school_data_transform$region, perl = TRUE), side = "right")

#rename columns to give more readable names
old_school_names <- names(school_data_transform)
new_school_names <- c("school_region_all", "school_longitude", "school_main_campus_flag", "school_name", "school_city", "school_control", "school_level", "school_zip", "school_latitude", "school_opeid6", "school_url", "school_opeid8", "school_st_abbr", "school_id", "school_addmission_rate", "SAT_reading_25th_percentile" )
setnames(school_data_transform, old = old_school_names, new = new_school_names)
school_data_transform <- rename(school_data_transform,c("main" = "main_campus_flag", 
                                                        "longtitude" = "school_longitude", 
                                                        'SchoolID',
                                                        "instnm"= "school_name",
                                                        'V3'='ReceivingGrantAid',
                                                        'V4'='AvgInstitutionalGrantAid',
                                                        'V5'='ReceivingStudentLoadAid',
                                                        'V6'='AvgStudentLoanAid', 
                                                        'V7'='SchoolCity',"zip" = "school_zip", 
                                                        'V9'='SchoolState', 
                                                        'V10'= 'SchoolGeographicRegion', 'V11'='SchoolSector', 'V12'='SchoolLevel', 'V13'='SchoolControl', 'V14'='SchoolSize', 'V15'='InStatePrice','V16'='OutofStatePrice', 'V17'='Admitted', 'V18'='SATReading25thPercentile', 'V19'='SATReading75thPercentile', 'V20'='SATMath25thPercentile', 'V21'='SATMath75thPercentile', 'V22'='ACTComposite25thPercentile', 'V23'='ACTComposite75thPercentile', 'V24'='ACTEnglish25thPercentile', 'V25'='ACTEnglish75thPercentile', 'V26'='ACTMath25thPercentile', 'V27'='ACTMath75thPercentile', 'V28'='GraduationRate', 'V29'='RetentionRate'))
