#transform school staging data and load into AWS tables dimSchool and factSchool

#decode region, control,level, and state abbreviations
#get look up tables
regions <- read.csv("regions.csv", stringsAsFactors = FALSE)
control <- read.csv("control.csv", stringsAsFactors = FALSE)
level <- read.csv("level.csv", stringsAsFactors = FALSE)
states <- read.csv("state_abbr.csv", stringsAsFactors = FALSE)

school_data_transform <- left_join(school_data_staging_api, regions[,2:3], by = c("region" = "value"))
school_data_transform <- left_join(school_data_transform, control[,2:3], by = c("control" = "value"))
school_data_transform <- left_join(school_data_transform, level[,2:3], by = c("iclevel" = "value"))
school_data_transform <- left_join(school_data_transform, states[,2:3], by = c("stabbr" = "value"))