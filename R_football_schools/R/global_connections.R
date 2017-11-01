#load libraries
library(RMySQL)
library(data.table)
library(dplyr)
library(rscorecard)

#added AWS credentials to .Renviron file

rmysql_settings <- "/Users/jallen/documents/dataprojects/secrets/my.cnf"
con <- dbConnect(MySQL(), 
                 default.file = rmysql_settings, 
                 group = "FootballSchools", 
                 user = NULL, 
                 password = NULL) #open connection to RDS

con <- dbConnect(MySQL(),
                 user = Sys.getenv("RDSuser"),
                 password = Sys.getenv("RDSpw"),
                 host = Sys.getenv("RDShost"),
                 dbname='FootballSchoolsDW') #open connection to RDS

dbDisconnect(con) #disconnect from RDS


