library(DBI)
library(dbplyr)
library(tidyverse)
library(RPostgres)
library(rvest)
wrds <- DBI::dbConnect(RPostgres::Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='jiazhang')
dbListTables(wrds)
dbExistsTable(wrds, 'crsp')

sql <- '
  SELECT *
  FROM pg_stat_statements
'
as_tibble(dbGetQuery(wrds, sql))

