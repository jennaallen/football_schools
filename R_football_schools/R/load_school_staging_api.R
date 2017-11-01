#added rscorecare API key to .Renviron file as DATAGOV_API_KEY which is searched for when sc_get() is run
paste(readLines('columns.csv'), collapse = ", ")
years <- seq(2011, 2015)

school_data_staging_api <- rbindlist(lapply(years, function(x) {
  sc_init() %>%
    sc_filter(control == 1:2, main == 1) %>%
    sc_select(UNITID, OPEID, OPEID6, INSTNM, CITY, STABBR, ZIP, INSTURL, MAIN, CONTROL, REGION, LATITUDE, LONGITUDE, ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATVRMID, SATMTMID, ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTCMMID, ACTENMID, ACTMTMID, UGDS, TUITIONFEE_IN, TUITIONFEE_OUT, C150_4, RET_FT4, PCTFLOAN, GRAD_DEBT_MDN, GRAD_DEBT_MDN10YR, LOAN_EVER, ALIAS, C100_4, ICLEVEL) %>%
    sc_year(x) %>%
    sc_get()
})) 


