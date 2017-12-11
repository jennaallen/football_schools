# added rscorecare API key to .Renviron file as DATAGOV_API_KEY which is searched for when sc_get() is run
paste(readLines('columns.csv'), collapse = ", ") # get comma separated column names for input into sc_selct
years <- seq(2011, 2015) # want data for school years starting 2011-2015 

# get school data for all 4-year schools that are public or private non-profit for desired years and combine it all into one data table
school_data_staging_api <- rbindlist(lapply(years, function(x) {
  sc_init() %>%
    sc_filter(ICLEVEL == 1, CONTROL == 1:2) %>%
    sc_select(UNITID, OPEID, OPEID6, INSTNM, CITY, STABBR, ZIP, INSTURL, MAIN, CONTROL, REGION, LATITUDE, LONGITUDE, ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATVRMID, SATMTMID, ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTCMMID, ACTENMID, ACTMTMID, UGDS, TUITIONFEE_IN, TUITIONFEE_OUT, C150_4, RET_FT4, PCTFLOAN, GRAD_DEBT_MDN, GRAD_DEBT_MDN10YR, LOAN_EVER, ALIAS, C100_4, ICLEVEL) %>%
    sc_year(x) %>%
    sc_get()
})) 

# found I was missing the following from the school data: united states military academy (army), united states naval academy (navy), and united states air force academy (air force)
# looked for this data using REGION == 0 (US service schools), but the only data available is for marines