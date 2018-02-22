source("R/libraries.R")
source("R/functions.R")

years <- seq(2011, 2015)

# get school data for all 4-year schools that are public or private non-profit for desired years and combine it all into one tibble
school_data_staging <- map_dfr(years, ~
                                 sc_init() %>%
                                 sc_filter(ICLEVEL == 1, CONTROL == 1:2) %>%
                                 sc_select(UNITID, OPEID, OPEID6, INSTNM, CITY, STABBR, ZIP, INSTURL, MAIN, CONTROL, REGION, LATITUDE, LONGITUDE, ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATVRMID, SATMTMID, ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTCMMID, ACTENMID, ACTMTMID, UGDS, TUITIONFEE_IN, TUITIONFEE_OUT, C150_4, RET_FT4, PCTFLOAN, GRAD_DEBT_MDN, GRAD_DEBT_MDN10YR, LOAN_EVER, ALIAS, C100_4, ICLEVEL) %>%
                                 sc_year(.x) %>%
                                 sc_get()
)

# get regular season and bowl game football data
football_data_staging <- get_football_data("schedule", years)
football_data_staging_bowls <- get_football_data("bowls", years)

# clean school data
school_data_transform <- clean_school_data(school_data_staging)

# clean football data
football_data_transform <- clean_football_data(football_data_staging, football_data_staging_bowls)

# profile data to determine data types and sizes for database
school_data_char_length <- school_data_transform %>% 
  map(~ max(nchar(.x), na.rm = TRUE))

school_data_transform %>% 
  mutate_if(is.character, as.factor) %>% 
  summary()

football_data_char_length <- football_data_transform %>% 
  map(~ max(nchar(.x), na.rm = TRUE))

football_data_transform %>% 
  mutate_if(is.character, as.factor) %>% 
  summary()

# fuzzy text matching to match the school name from the football data and the name from the school data
fuzzy_text_res <- fuzzy_text_matching(school_data_transform, football_data_transform)

# filter data based on exact matches and results for close matches
text_matches_for_review <- fuzzy_text_res %>% 
  filter(jw < 0.20) %>% 
  mutate(correct_match = 1) %>% # add column for user to manually update with 0 if the match is no good
  edit()

# this is an iterative process where you can add the correct matches to a final match data table until all schools are matched
fuzzy_text_match_final <- text_matches_for_review %>% 
  filter(correct_match == 1) %>% 
  select(football_name, school_name)

# concatenate "university of" to the football school names to see if that improved the text matching for the remaining unmatched schools
fuzzy_text_conc_before <- fuzzy_text_matching(school_data_transform, football_data_transform, "university of", "before")
# remove the results that have already been matched
fuzzy_text_conc_before <- fuzzy_text_conc_before %>% 
  anti_join(fuzzy_text_match_final, by = "football_name")

fuzzy_text_conc_before_review <- fuzzy_text_conc_before %>% 
  filter(jw < 0.10) %>% 
  mutate(correct_match = 1) %>%
  edit()

# this is an iterative process where you can add the correct matches to a final match data table until all schools are matched
fuzzy_text_match_final <- fuzzy_text_conc_before_review %>% 
  filter(correct_match == 1) %>% 
  select(football_name, school_name) %>% 
  bind_rows(fuzzy_text_match_final)

# final school name lookup table
fuzzy_text_final <- read_csv("fuzzy_text_final.csv")

# get missing data that was not available in collegescorecard api
missing_school_data_1 <- get_missing_data("~/Documents/DataProjects/football_schools/school_data/column_order1", 
                                          c("school_id", 
                                            "school_name",
                                            "school_alias",
                                            "school_city",
                                            "zip",
                                            "school_st_abbr",
                                            "school_url",
                                            "school_longitude",
                                            "school_latitude",
                                            "region",
                                            "level",
                                            "control",
                                            "size_category",
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
                                            "school_admission_rate",
                                            "school_retention_rate"), c(1:6, 8:11, 13:15, 22, 25:34, 37:38))

missing_school_data_2 <- get_missing_data("~/Documents/DataProjects/football_schools/school_data/column_order2", 
                                          c("school_id",
                                            "school_name",
                                            "school_alias",
                                            "school_city",
                                            "zip",
                                            "school_st_abbr",
                                            "school_url",
                                            "school_longitude",
                                            "school_latitude",
                                            "region",
                                            "level",
                                            "control",
                                            "size_category",
                                            "school_graduation_rate_6yrs",
                                            "school_admission_rate",
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

# clean missing data
missing_school_data <- clean_missing_data(missing_school_data_1, missing_school_data_2)

# connect to AWS RDS
con <- dbConnect(MySQL(),
                 user = Sys.getenv("RDSuser"),
                 password = Sys.getenv("RDSpw"),
                 host = Sys.getenv("RDShost"),
                 dbname = 'FootballSchoolDW')

# cread dimDate table and write it to the database
# filter for only 2015 data and only columns needed for dimSchool
dimSchool <- school_data_transform %>%
  semi_join(fuzzy_text_final, by = "school_name") %>% 
  bind_rows(missing_school_data) %>% 
  filter(school_year_start %in% max(school_year_start)) %>%
  select(school_id,
         school_name,
         school_alias,
         school_city,
         school_st_abbr,
         school_state,
         school_zip,
         school_region,
         school_longitude,
         school_latitude,
         school_main_campus_flag,
         school_size_category,
         school_url,
         school_control,
         school_level)

dbWriteTable(con, name = "dimSchool", value = dimSchool , append = TRUE, row.names = FALSE)

# create dimDate table and write it to the database
dimDate <- create_date_table(2011)

dbWriteTable(conn = con, name = 'dimDate', value = dimDate, append = TRUE, row.names = FALSE)

# create dimSchoolYear table and write it to the database
dimSchoolYear <- create_school_year(dimDate)

dbWriteTable(conn = con, name = 'dimSchoolYear', value = dimSchoolYear, append = TRUE, row.names = FALSE)

# create factGame table and write it to the database
factGame <- create_factGame(football_data_transform, dimDate, fuzzy_text_final)

dbWriteTable(conn = con, name = 'factGame', value = factGame, append = TRUE, row.names = FALSE)

# create factSchool table and write it to the database
factSchool <- create_factSchool(school_data_transform, missing_school_data, dimSchoolYear, fuzzy_text_final)

dbWriteTable(conn = con, name = 'factSchool', value = factSchool, append = TRUE, row.names = FALSE)

# create view in SQL
dbGetQuery(con, "CREATE VIEW viewAnalysis AS 
SELECT
  ds.school_id,
  ds.school_name,
  ds.school_city,
  ds.school_st_abbr,
  ds.school_state,
  ds.school_zip,
  ds.school_region,
  ds.school_longitude,
  ds.school_latitude,
  ds.school_main_campus_flag,
  ds.school_size_category,
  ds.school_url,
  ds.school_control,
  ds.school_level,
  fs.school_admission_rate,
  fs.school_in_state_price,
  fs.school_out_state_price,
  fs.school_retention_rate,
  fs.school_graduation_rate_4yrs,
  fs.school_graduation_rate_6yrs,
  fs.school_federal_loan_rate,
  fs.school_students_with_any_loan,
  fs.school_median_debt_graduates,
  fs.school_median_debt_graduates_monthly_payments,
  fs.SAT_reading_25th_percentile,
  fs.SAT_reading_75th_percentile,
  fs.SAT_math_25th_percentile,
  fs.SAT_math_75th_percentile,
  fs.SAT_reading_midpoint,
  fs.SAT_math_midpoint,
  fs.ACT_composite_25th_percentile,
  fs.ACT_composite_75th_percentile,
  fs.ACT_english_25th_percentile,
  fs.ACT_english_75th_percentile,
  fs.ACT_math_25th_percentile,
  fs.ACT_math_75th_percentile,
  fs.ACT_composite_midpoint,
  fs.ACT_english_midpoint,
  fs.ACT_math_midpoint,
  sy.school_year_value,
  SUM(fg.school_points) AS school_points,
  SUM(fg.opponent_points) AS opponent_points,
  SUM(fg.school_win) AS school_wins,
  MIN(fg.school_rank) AS min_school_rank,
  MIN(fg.opponent_rank) AS min_opponent_rank,
  SUM(fg.bowl_flag) AS bowl_games,
  SUM(CASE WHEN (fg.bowl_flag = 1 AND fg.school_win = 1)
  THEN 1
  ELSE 0
  END) AS bowl_wins,
  SUM(fg.national_championship_flag) AS national_championship_games,
  SUM(CASE WHEN (fg.national_championship_flag = 1 AND fg.school_win = 1)
  THEN 1
  ELSE 0
  END) AS national_championship_wins,
  SUM(CASE WHEN (fg.school_game_site = 'home' AND fg.school_win = 1)
  THEN 1
  ELSE 0
  END) AS home_wins, 
  SUM(CASE WHEN (fg.school_game_site = 'home' AND fg.school_win = 0)
  THEN 1
  ELSE 0
  END) AS home_loses,
  SUM(CASE WHEN (fg.school_game_site = 'home')
  THEN 1
  ELSE 0
  END) AS home_games,
  SUM(CASE WHEN ((fg.school_game_site = 'away' OR fg.school_game_site = 'neutral' ) AND fg.school_win = 1)
  THEN 1
  ELSE 0
  END) AS road_wins,
  SUM(CASE WHEN ((fg.school_game_site = 'away' OR fg.school_game_site = 'neutral' ) AND  fg.school_win = 0)
  THEN 1
  ELSE 0
  END) AS road_loses,
  SUM(CASE WHEN (fg.school_game_site = 'away' OR fg.school_game_site = 'neutral' )
  THEN 1
  ELSE 0
  END) AS road_games,
  SUM(CASE WHEN (fg.opponent_rank IS NOT NULL AND fg.school_win = 1)
  THEN 1
  ELSE 0
  END) AS wins_against_ranked_opponents,
  SUM(CASE WHEN (fg.opponent_rank IS NOT NULL AND fg.school_win = 0)
  THEN 1
  ELSE 0
  END) AS loses_against_ranked_opponent,
  SUM(CASE WHEN (fg.opponent_rank IS NOT NULL)
  THEN 1
  ELSE 0
  END) AS games_against_ranked_opponent,
  SUM(fg.school_points) - SUM(fg.opponent_points) AS point_differential,
  COUNT(*) as total_games
  FROM FootballSchoolDW.factGame AS fg
  INNER JOIN FootballSchoolDW.dimSchool AS ds
  ON fg.school_sk = ds.school_sk
  INNER JOIN FootballSchoolDW.dimDate AS dd
  ON dd.date_sk = fg.date_sk
  INNER JOIN FootballSchoolDW.dimSchoolYear AS sy
  ON dd.school_year_sk = sy.school_year_sk
  INNER JOIN FootballSchoolDW.factSchool AS fs
  ON fs.school_sk = ds.school_sk and fs.school_year_sk = sy.school_year_sk
  GROUP BY 
  ds.school_id,
  ds.school_name,
  ds.school_city,
  ds.school_st_abbr,
  ds.school_state,
  ds.school_zip,
  ds.school_region,
  ds.school_longitude,
  ds.school_latitude,
  ds.school_main_campus_flag,
  ds.school_size_category,
  ds.school_url,
  ds.school_control,
  ds.school_level,
  fs.school_admission_rate,
  fs.school_in_state_price,
  fs.school_out_state_price,
  fs.school_retention_rate,
  fs.school_graduation_rate_4yrs,
  fs.school_graduation_rate_6yrs,
  fs.school_federal_loan_rate,
  fs.school_students_with_any_loan,
  fs.school_median_debt_graduates,
  fs.school_median_debt_graduates_monthly_payments,
  fs.SAT_reading_25th_percentile,
  fs.SAT_reading_75th_percentile,
  fs.SAT_math_25th_percentile,
  fs.SAT_math_75th_percentile,
  fs.SAT_reading_midpoint,
  fs.SAT_math_midpoint,
  fs.ACT_composite_25th_percentile,
  fs.ACT_composite_75th_percentile,
  fs.ACT_english_25th_percentile,
  fs.ACT_english_75th_percentile,
  fs.ACT_math_25th_percentile,
  fs.ACT_math_75th_percentile,
  fs.ACT_composite_midpoint,
  fs.ACT_english_midpoint,
  fs.ACT_math_midpoint,
  sy.school_year_value
  ORDER BY sy.school_year_value")

# disconnect from RDS
dbDisconnect(con)