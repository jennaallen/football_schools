#load libraries
library(RMySQL)
library(data.table)
library(plyr)
library(dplyr)
library(rscorecard)
library(stringr)
library(rvest)
library(stringdist)
library(utils)

#added AWS credentials to .Renviron file

con <- dbConnect(MySQL(),
                 user = Sys.getenv("RDSuser"),
                 password = Sys.getenv("RDSpw"),
                 host = Sys.getenv("RDShost"),
                 dbname='FootballSchoolsDW') #open connection to RDS

dbDisconnect(con) #disconnect from RDS


